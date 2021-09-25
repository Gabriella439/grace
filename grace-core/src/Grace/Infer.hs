{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

{-| This module implements the bidirectional type-checking algorithm from:

    Dunfield, Joshua, and Neelakantan R. Krishnaswami. \"Complete and easy bidirectional typechecking for higher-rank polymorphism.\" ACM SIGPLAN Notices 48.9 (2013): 429-442.

    The source code of this module strives to use the same variable-naming
    conventions as the original paper.  The source code also includes comments
    highlighting which subexpressions in the code correspond to which judgments
    from the paper.

    The main difference from the original algorithm that this module uses
    `Control.Monad.State.Strict.StateT` to thread around `Context`s and
    manipulate them instead of explicit `Context` passing as in the original
    paper.

    Also, the code will not add comments for code that corresponds 1-to-1 with
    the original paper.  Instead, the comments will mostly explain new
    type-checking logic that wasn't already covered by the paper.  The comments
    are intended to be read top-to-bottom.
-}
module Grace.Infer
    ( -- * Type inference
      typeOf
    , typeWith

      -- * Errors related to type inference
    , TypeInferenceError(..)
    ) where

import Data.Text (Text)

import Control.Applicative ((<|>))
import Control.Exception.Safe (Exception(..))
import Control.Monad (when)
import Control.Monad.Except (MonadError(..))
import Control.Monad.State.Strict (MonadState)
import Data.Foldable (traverse_)
import Data.Sequence (ViewL(..))
import Data.String.Interpolate (__i)
import Data.Void (Void)
import Grace.Context (Context, Entry)
import Grace.Existential (Existential)
import Grace.Location (Location(..))
import Grace.Monotype (Monotype)
import Grace.Pretty (Pretty(..))
import Grace.Syntax (Syntax(Syntax))
import Grace.Type (Type(..))
import Grace.Value (Value)

import qualified Control.Monad       as Monad
import qualified Control.Monad.State as State
import qualified Data.Map            as Map
import qualified Data.Sequence       as Seq
import qualified Data.Text           as Text
import qualified Grace.Context       as Context
import qualified Grace.Domain        as Domain
import qualified Grace.Location      as Location
import qualified Grace.Monotype      as Monotype
import qualified Grace.Pretty
import qualified Grace.Syntax        as Syntax
import qualified Grace.Type          as Type
import qualified Prettyprinter       as Pretty

-- | Type-checking state
data Status = Status
    { count :: !Int
      -- ^ Used to generate fresh unsolved variables (e.g. α̂, β̂)
    , context :: Context Location
      -- ^ The type-checking context (e.g. Γ, Δ, Θ)
    }

orDie :: MonadError e m => Maybe a -> e -> m a
Just x  `orDie` _ = return x
Nothing `orDie` e = throwError e

fresh :: MonadState Status m => m (Existential a)
fresh = do
    Status{ count = n, .. } <- State.get

    State.put $! Status{ count = n + 1, .. }

    return (fromIntegral n)

-- The main place where we depart from the paper is that we don't explicitly
-- thread the `Context` around.  Instead, we mutate the ambient state using
-- the following utility functions

-- | Push a new `Context` `Entry` onto the stack
push :: MonadState Status m => Entry Location -> m ()
push entry = State.modify (\s -> s { context = entry : context s })

-- | Retrieve the current `Context`
get :: MonadState Status m => m (Context Location)
get = State.gets context

-- | Set the `Context` to a new value
set :: MonadState Status m => Context Location -> m ()
set context = State.modify (\s -> s{ context })

-- | Discard all `Context` entries up to and including the specified `Entry`
discardUpTo :: MonadState Status m => Entry Location -> m ()
discardUpTo entry =
    State.modify (\s -> s{ context = Context.discardUpTo entry (context s) })

{-| This corresponds to the judgment:

    > Γ ⊢ A

    … which checks that under context Γ, the type A is well-formed
-}
wellFormedType :: MonadError TypeInferenceError m => Context Location -> Type Location -> m ()
wellFormedType _Γ Type{..} =
    case node of
        -- UvarWF
        Type.VariableType a
            | Context.Variable Domain.Type a `elem` _Γ -> do
                return ()
            | otherwise -> throwError (UnboundTypeVariable location a)

        -- ArrowWF
        Type.Function _A _B -> do
            wellFormedType _Γ _A
            wellFormedType _Γ _B

        -- ForallWF
        Type.Forall _ a domain _A -> do
            wellFormedType (Context.Variable domain a : _Γ) _A

        -- ForallWF
        Type.Exists _ a domain _A -> do
            wellFormedType (Context.Variable domain a : _Γ) _A

        -- EvarWF / SolvedEvarWF
        _A@(Type.UnsolvedType a0)
            | any predicate _Γ -> do
                return ()
            | otherwise -> do
                throwError (IllFormedType location _A _Γ)
          where
            predicate (Context.UnsolvedType a1  ) = a0 == a1
            predicate (Context.SolvedType   a1 _) = a0 == a1
            predicate  _                          = False

        Type.TypeHole -> do
            return ()

        Type.Optional _A -> do
            wellFormedType _Γ _A

        Type.List _A -> do
            wellFormedType _Γ _A

        Type.Record (Type.Fields kAs Monotype.EmptyFields) -> do
            traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs

        Type.Record (Type.Fields kAs (Monotype.UnsolvedFields a0))
            | any predicate _Γ -> do
                traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
            | otherwise -> do
                throwError (IllFormedFields location a0 _Γ)
          where
            predicate (Context.UnsolvedFields a1  ) = a0 == a1
            predicate (Context.SolvedFields   a1 _) = a0 == a1
            predicate  _                            = False

        Type.Record (Type.Fields kAs Monotype.HoleFields) -> do
            traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs

        Type.Record (Type.Fields kAs (Monotype.VariableFields a))
            | Context.Variable Domain.Fields a `elem` _Γ -> do
                traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
            | otherwise -> do
                throwError (UnboundFields location a)

        Type.Union (Type.Alternatives kAs Monotype.EmptyAlternatives) -> do
            traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs

        Type.Union (Type.Alternatives kAs (Monotype.UnsolvedAlternatives a0))
            | any predicate _Γ -> do
                traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
            | otherwise -> do
                throwError (IllFormedAlternatives location a0 _Γ)
          where
            predicate (Context.UnsolvedAlternatives a1  ) = a0 == a1
            predicate (Context.SolvedAlternatives   a1 _) = a0 == a1
            predicate  _                                  = False

        Type.Union (Type.Alternatives kAs Monotype.HoleAlternatives) -> do
            traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs

        Type.Union (Type.Alternatives kAs (Monotype.VariableAlternatives a))
            | Context.Variable Domain.Alternatives a `elem` _Γ -> do
                traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
            | otherwise -> do
                throwError (UnboundAlternatives location a)

        Type.Scalar _ -> do
            return ()

{-| This corresponds to the judgment:

    > Γ ⊢ A <: B ⊣ Δ

    … which updates the context Γ to produce the new context Δ, given that the
    type A is a subtype of type B.
-}
subtype
    :: (MonadState Status m, MonadError TypeInferenceError m)
    => Type Location -> Type Location -> m ()
subtype _A0 _B0 = do
    _Γ <- get

    case (Type.node _A0, Type.node _B0) of
        (Type.TypeHole, _) -> do
            a <- fresh

            push (Context.MarkerType a)
            push (Context.UnsolvedType a)

            let _A1 = _A0{ node = Type.UnsolvedType a }

            subtype _A1 _B0

            discardUpTo (Context.MarkerType a)

        (_, Type.TypeHole) -> do
            b <- fresh

            push (Context.MarkerType b)
            push (Context.UnsolvedType b)

            let _B1 = _B0{ node = Type.UnsolvedType b }

            subtype _A0 _B1

            discardUpTo (Context.MarkerType b)

        -- <:Var
        (Type.VariableType a0, Type.VariableType a1)
            | a0 == a1 -> do
                wellFormedType _Γ _A0

        -- <:Exvar
        (Type.UnsolvedType a0, Type.UnsolvedType a1)
            | a0 == a1 && Context.UnsolvedType a0 `elem` _Γ -> do
                return ()

        -- InstantiateL
        (Type.UnsolvedType a, _)
            -- The `not (a `Type.typeFreeIn` _B)` is the "occurs check" which
            -- prevents a type variable from being defined in terms of itself
            -- (i.e. a type should not "occur" within itself).
            --
            -- Later on you'll see matching "occurs checks" for record types and
            -- union types so that Fields variables and Alternatives variables
            -- cannot refer to the record or union that they belong to,
            -- respectively.
            | not (a `Type.typeFreeIn` _B0) && elem (Context.UnsolvedType a) _Γ -> do
                instantiateTypeL a _B0

        -- InstantiateR
        (_, Type.UnsolvedType a)
            |   not (a `Type.typeFreeIn` _A0)
            &&  elem (Context.UnsolvedType a) _Γ -> do
                instantiateTypeR _A0 a

        -- <:→
        (Type.Function _A1 _A2, Type.Function _B1 _B2) -> do
            subtype _B1 _A1

            _Θ <- get

            -- CAREFULLY NOTE: Pay really close attention to how we need to use
            -- `Context.solveType` any time we update the context.  The paper
            -- already mentions this, but if you forget to do this then you
            -- will get bugs due to unsolved variables not getting solved
            -- correctly.
            --
            -- A much more reliable way to fix this problem would simply be to
            -- have every function (like `subtype`, `instantiateL`, …)
            -- apply `solveType` to its inputs.  For example, this very
            -- `subtype` function could begin by doing:
            --
            --     _Γ <- get
            --     let _A0' = Context.solveType _Γ _A0
            --     let _B0' = Context.solveType _Γ _B0
            --
            -- … and then use _A0' and _B0' for downstream steps.  If we did
            -- that at the beginning of each function then everything would
            -- "just work".
            --
            -- However, this would be more inefficient because we'd calling
            -- `solveType` wastefully over and over with the exact same context
            -- in many cases.  So, the tradeoff here is that we get improved
            -- performance if we're willing to remember to call `solveType` in
            -- the right places.
            subtype (Context.solveType _Θ _A2) (Context.solveType _Θ _B2)

        -- One of the main extensions that is not present in the original paper
        -- is the addition of existential quantification.  This was actually
        -- pretty easy to implement: you just take the rules for universal
        -- quantification and flip them around and everything works.  Elegant!
        --
        -- For example, the <:∃R rule is basically the same as the <:∀L rule,
        -- except with the arguments flipped.  Similarly, the <:∃L rule is
        -- basically the same as the <:∀R rule with the arguments flipped.

        -- <:∃R
        (_, Type.Exists nameLocation a0 Domain.Type _B) -> do
            a1 <- fresh

            push (Context.MarkerType a1)
            push (Context.UnsolvedType a1)

            let a1' =
                    Type{ location = nameLocation, node = Type.UnsolvedType a1 }

            subtype _A0 (Type.substituteType a0 0 a1' _B)

            discardUpTo (Context.MarkerType a1)

        (_, Type.Exists _ a0 Domain.Fields _B) -> do
            a1 <- fresh

            push (Context.MarkerFields   a1)
            push (Context.UnsolvedFields a1)

            let a1' = Type.Fields [] (Monotype.UnsolvedFields a1)

            subtype _A0 (Type.substituteFields a0 0 a1' _B)

            discardUpTo (Context.MarkerFields a1)

        (_, Type.Exists _ a0 Domain.Alternatives _B) -> do
            a1 <- fresh

            push (Context.MarkerAlternatives a1)
            push (Context.UnsolvedAlternatives a1)

            let a1' = Type.Alternatives [] (Monotype.UnsolvedAlternatives a1)

            subtype _A0 (Type.substituteAlternatives a0 0 a1' _B)

            discardUpTo (Context.MarkerAlternatives a1)

        -- <:∀L
        (Type.Forall nameLocation a0 Domain.Type _A, _) -> do
            a1 <- fresh

            push (Context.MarkerType a1)
            push (Context.UnsolvedType a1)

            let a1' =
                    Type{ location = nameLocation, node = Type.UnsolvedType a1 }

            subtype (Type.substituteType a0 0 a1' _A) _B0

            discardUpTo (Context.MarkerType a1)

        (Type.Forall _ a0 Domain.Fields _A, _) -> do
            a1 <- fresh

            push (Context.MarkerFields   a1)
            push (Context.UnsolvedFields a1)

            let a1' = Type.Fields [] (Monotype.UnsolvedFields a1)

            subtype (Type.substituteFields a0 0 a1' _A) _B0

            discardUpTo (Context.MarkerFields a1)

        (Type.Forall _ a0 Domain.Alternatives _A, _) -> do
            a1 <- fresh

            push (Context.MarkerAlternatives a1)
            push (Context.UnsolvedAlternatives a1)

            let a1' = Type.Alternatives [] (Monotype.UnsolvedAlternatives a1)

            subtype (Type.substituteAlternatives a0 0 a1' _A) _B0

            discardUpTo (Context.MarkerAlternatives a1)

        -- <:∃L
        (Type.Exists _ a domain _A, _) -> do
            push (Context.Variable domain a)

            subtype _A _B0

            discardUpTo (Context.Variable domain a)

        -- <:∀R
        (_, Type.Forall _ a domain _B) -> do
            push (Context.Variable domain a)

            subtype _A0 _B

            discardUpTo (Context.Variable domain a)

        (Type.Scalar s0, Type.Scalar s1)
            | s0 == s1 -> do
                return ()

        (Type.Optional _A, Type.Optional _B) -> do
            subtype _A _B

        (Type.List _A, Type.List _B) -> do
            subtype _A _B

        -- This is where you need to add any non-trivial subtypes.  For example,
        -- the following three rules specify that `Natural` is a subtype of
        -- `Integer`, which is in turn a subtype of `Real`.
        (Type.Scalar Monotype.Natural, Type.Scalar Monotype.Integer) -> do
            return ()

        (Type.Scalar Monotype.Natural, Type.Scalar Monotype.Real) -> do
            return ()

        (Type.Scalar Monotype.Integer, Type.Scalar Monotype.Real) -> do
            return ()

        -- Similarly, this is the rule that says that `T` is a subtype of
        -- `Optional T`.  If that feels unprincipled to you then delete this
        -- rule.
        (_, Type.Optional _B) -> do
            subtype _A0 _B

        -- The type-checking code for records is the first place where we
        -- implement a non-trivial type that wasn't already covered by the
        -- paper, so we'll go into more detail here to explain the general
        -- type-checking principles of the paper.
        (Type.Record (Type.Fields kAs Monotype.HoleFields), _) -> do
            p <- fresh

            push (Context.MarkerFields p)
            push (Context.UnsolvedFields p)

            subtype Type{ location = Type.location _A0, node = Type.Record (Type.Fields kAs (Monotype.UnsolvedFields p)) } _B0

            discardUpTo (Context.MarkerFields p)

        (_, Type.Record (Type.Fields kBs Monotype.HoleFields)) -> do
            p <- fresh

            push (Context.MarkerFields p)
            push (Context.UnsolvedFields p)

            subtype _A0 Type{ location = Type.location _B0, node = Type.Record (Type.Fields kBs (Monotype.UnsolvedFields p)) }

            discardUpTo (Context.MarkerFields p)

        (_A@(Type.Record (Type.Fields kAs0 fields0)), _B@(Type.Record (Type.Fields kBs0 fields1))) -> do
            let mapA = Map.fromList kAs0
            let mapB = Map.fromList kBs0

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            let flexible  Monotype.EmptyFields       = False
                flexible (Monotype.VariableFields _) = False
                flexible (Monotype.UnsolvedFields _) = True
                flexible  Monotype.HoleFields        = True

            let okayA = Map.null extraA
                    || (flexible fields1 && fields0 /= fields1)

            let okayB = Map.null extraB
                    || (flexible fields0 && fields0 /= fields1)

            -- First we check that there are no mismatches in the record types
            -- that cannot be resolved by just setting an unsolved Fields
            -- variable to the right type.
            --
            -- For example, `{ x: Bool }` can never be a subtype of
            -- `{ y: Text }`, but `{ x: Bool, a }` can be a subtype of
            -- `{ y: Text, b }` if we solve `a` to be `{ y: Text }` and solve
            -- `b` to be `{ x : Bool }`.
            if | not okayA && not okayB -> do
                throwError (RecordTypeMismatch _A0 _B0 extraA extraB)

               | not okayA -> do
                throwError (RecordTypeMismatch _A0 _B0 extraA mempty)

               | not okayB -> do
                throwError (RecordTypeMismatch _A0 _B0 mempty extraB)

               | otherwise -> do
                   return ()

            -- If record A is a subtype of record B, then all fields in A
            -- must be a subtype of the matching fields in record B
            let process (_A1, _B1) = do
                    _Θ <- get

                    subtype
                        (Context.solveType _Θ _A1)
                        (Context.solveType _Θ _B1)

            -- We only check fields are present in `both` records.  For
            -- mismatched fields present only in one record type we have to
            -- skip to the next step of resolving the mismatch by solving Fields
            -- variables.
            traverse_ process both

            -- Here is where we handle fields that were only present in one
            -- record type.  They still might be okay if one or both of the
            -- record types has an unsolved fields variable.
            case (fields0, fields1) of
                -- The two records are identical, so there's nothing left to do
                _ | null extraA && null extraB && fields0 == fields1 -> do
                        return ()

                -- Both records type have unsolved Fields variables.  Great!
                -- This is the most flexible case, since we can replace these
                -- unsolved variables with whatever fields we want to make the
                -- types match.
                --
                -- However, it's not as simple as setting each Fields variable
                -- to the extra fields from the opposing record type.  For
                -- example, if the two record types we're comparing are:
                --
                --     { x: Bool, p0 } <: { y: Text, p1 }
                --
                -- … then it's not correct to say:
                --
                --     p0 = y: Text
                --     p1 = x: Bool
                --
                -- … because that is not the most general solution for `p0` and
                -- `p1`!  The actual most general solution is:
                --
                --     p0 = { y: Text, p2 }
                --     p1 = { x: Bool, p2 }
                --
                -- … where `p2` is a fresh Fields type variable representing the
                -- fact that both records could potentially have even more
                -- fields other than `x` and `y`.
                (Monotype.UnsolvedFields p0, Monotype.UnsolvedFields p1) -> do
                    p2 <- fresh

                    _Γ0 <- get

                    -- We have to insert p2 before both p0 and p1 within the
                    -- context because the bidirectional type-checking algorithm
                    -- requires that the context is ordered and all variables
                    -- within the context can only reference prior variables
                    -- within the context.
                    --
                    -- Since `p0` and `p1` both have to reference `p2`, then we
                    -- need to insert `p2` right before `p0` or `p1`, whichever
                    -- one comes first
                    let p0First = do
                            (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields p0 _Γ0

                            Monad.guard (Context.UnsolvedFields p1 `elem` _ΓR)

                            let command =
                                    set (   _ΓR
                                        <>  ( Context.UnsolvedFields p0
                                            : Context.UnsolvedFields p2
                                            : _ΓL
                                            )
                                        )

                            return command

                    let p1First = do
                            (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields p1 _Γ0

                            Monad.guard (Context.UnsolvedFields p0 `elem` _ΓR)

                            let command =
                                    set (   _ΓR
                                        <>  ( Context.UnsolvedFields p1
                                            : Context.UnsolvedFields p2
                                            : _ΓL
                                            )
                                        )

                            return command

                    case p0First <|> p1First of
                        Nothing -> do
                            throwError (MissingOneOfFields [Type.location _A0, Type.location _B0] p0 p1 _Γ)

                        Just setContext -> do
                            setContext

                    _Θ <- get

                    -- Now we solve for `p0`.  This is basically saying:
                    --
                    -- p0 = { extraFieldsFromRecordB, p2 }
                    instantiateFieldsL
                        p0
                        (Type.location _B0)
                        (Context.solveRecord _Θ
                            (Type.Fields (Map.toList extraB)
                                (Monotype.UnsolvedFields p2)
                            )
                        )

                    _Δ <- get

                    -- Similarly, solve for `p1`.  This is basically saying:
                    --
                    -- p1 = { extraFieldsFromRecordA, p2 }
                    instantiateFieldsR
                        (Type.location _A0)
                        (Context.solveRecord _Δ
                            (Type.Fields (Map.toList extraA)
                                (Monotype.UnsolvedFields p2)
                            )
                        )
                        p1

                -- If only one of the records has a Fields variable then the
                -- solution is simpler: just set the Fields variable to the
                -- extra fields from the opposing record
                (Monotype.UnsolvedFields p0, _) -> do
                    _Θ <- get

                    instantiateFieldsL
                        p0
                        (Type.location _B0)
                        (Context.solveRecord _Θ
                            (Type.Fields (Map.toList extraB) fields1)
                        )

                (_, Monotype.UnsolvedFields p1) -> do
                    _Θ <- get

                    instantiateFieldsR
                        (Type.location _A0)
                        (Context.solveRecord _Θ
                            (Type.Fields (Map.toList extraA) fields0)
                        )
                        p1

                (_, _) -> do
                    throwError (NotRecordSubtype (Type.location _A0) _A (Type.location _B0) _B)

        -- Checking if one union is a subtype of another union is basically the
        -- exact same as the logic for checking if a record is a subtype of
        -- another record.
        (Type.Union (Type.Alternatives kAs Monotype.HoleAlternatives), _) -> do
            p <- fresh

            push (Context.MarkerAlternatives p)
            push (Context.UnsolvedAlternatives p)

            subtype Type{ location = Type.location _A0, node = Type.Union (Type.Alternatives kAs (Monotype.UnsolvedAlternatives p)) } _B0

            discardUpTo (Context.MarkerAlternatives p)

        (_, Type.Union (Type.Alternatives kBs Monotype.HoleAlternatives)) -> do
            p <- fresh

            push (Context.MarkerAlternatives p)
            push (Context.UnsolvedAlternatives p)

            subtype _A0 Type{ location = Type.location _B0, node = Type.Union (Type.Alternatives kBs (Monotype.UnsolvedAlternatives p)) }

            discardUpTo (Context.MarkerAlternatives p)

        (_A@(Type.Union (Type.Alternatives kAs0 alternatives0)), _B@(Type.Union (Type.Alternatives kBs0 alternatives1))) -> do
            let mapA = Map.fromList kAs0
            let mapB = Map.fromList kBs0

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            let flexible  Monotype.EmptyAlternatives       = False
                flexible (Monotype.VariableAlternatives _) = False
                flexible (Monotype.UnsolvedAlternatives _) = True
                flexible  Monotype.HoleAlternatives        = True

            let okayA = Map.null extraA
                    ||  (flexible alternatives1 && alternatives0 /= alternatives1)
            let okayB = Map.null extraB
                    ||  (flexible alternatives0 && alternatives0 /= alternatives1)

            if | not okayA && not okayB -> do
                throwError (UnionTypeMismatch _A0 _B0 extraA extraB)

               | not okayA && okayB -> do
                throwError (UnionTypeMismatch _A0 _B0 extraA mempty)

               | okayA && not okayB -> do
                throwError (UnionTypeMismatch _A0 _B0 mempty extraB)

               | otherwise -> do
                return ()

            let process (_A1, _B1) = do
                    _Θ <- get

                    subtype
                        (Context.solveType _Θ _A1)
                        (Context.solveType _Θ _B1)

            traverse_ process both

            case (alternatives0, alternatives1) of
                (Monotype.UnsolvedAlternatives p0, Monotype.UnsolvedAlternatives p1) -> do
                    p2 <- fresh

                    _Γ0 <- get

                    let p0First = do
                            (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives p0 _Γ0

                            Monad.guard (Context.UnsolvedAlternatives p1 `elem` _ΓR)

                            let command =
                                    set (   _ΓR
                                        <>  ( Context.UnsolvedAlternatives p0
                                            : Context.UnsolvedAlternatives p2
                                            : _ΓL
                                            )
                                        )

                            return command

                    let p1First = do
                            (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives p1 _Γ0

                            Monad.guard (Context.UnsolvedAlternatives p0 `elem` _ΓR)

                            let command =
                                    set (   _ΓR
                                        <>  ( Context.UnsolvedAlternatives p1
                                            : Context.UnsolvedAlternatives p2
                                            : _ΓL
                                            )
                                        )

                            return command

                    case p0First <|> p1First of
                        Nothing -> do
                            throwError (MissingOneOfAlternatives [Type.location _A0, Type.location _B0] p0 p1 _Γ)

                        Just setContext -> do
                            setContext

                    _Θ <- get

                    instantiateAlternativesL
                        p0
                        (Type.location _B0)
                        (Context.solveUnion _Θ
                            (Type.Alternatives (Map.toList extraB)
                                (Monotype.UnsolvedAlternatives p2)
                            )
                        )

                    _Δ <- get

                    instantiateAlternativesR
                        (Type.location _A0)
                        (Context.solveUnion _Δ
                            (Type.Alternatives (Map.toList extraA)
                                (Monotype.UnsolvedAlternatives p2)
                            )
                        )
                        p1

                (Monotype.EmptyAlternatives, Monotype.EmptyAlternatives) -> do
                    return ()

                (Monotype.UnsolvedAlternatives p0, _) -> do
                    _Θ <- get

                    instantiateAlternativesL
                        p0
                        (Type.location _B0)
                        (Context.solveUnion _Θ
                            (Type.Alternatives (Map.toList extraB)
                                alternatives1
                            )
                        )

                (Monotype.VariableAlternatives p0, Monotype.VariableAlternatives p1)
                    | p0 == p1 -> do
                        return ()

                (_, Monotype.UnsolvedAlternatives p1) -> do
                    _Θ <- get

                    instantiateAlternativesR
                        (Type.location _A0)
                        (Context.solveUnion _Θ
                            (Type.Alternatives (Map.toList extraA)
                                alternatives0
                            )
                        )
                        p1

                (_, _) -> do
                    throwError (NotUnionSubtype (Type.location _A0) _A (Type.location _B0) _B)

        -- Unfortunately, we need to have this wildcard match at the end,
        -- otherwise we'd have to specify a number of cases that is quadratic
        -- in the number of `Type` constructors.  That in turn means that you
        -- can easily forget to add cases like:
        --
        --     (Type.List _A, Type.List _B) -> do
        --         subtype _A _B
        --
        -- … because the exhaustivity checker won't warn you if you forget to
        -- add that case.
        --
        -- The way I remember to do this is that when I add new complex types I
        -- grep the codebase for all occurrences of an existing complex type
        -- (like `List`), and then one of the occurrences will be here in this
        -- `subtype` function and then I'll remember to add a case for my new
        -- complex type here.
        (_A, _B) -> do
            throwError (NotSubtype (Type.location _A0) _A (Type.location _B0) _B)

{-| This corresponds to the judgment:

    > Γ ⊢ α̂ :≦ A ⊣ Δ

    … which updates the context Γ to produce the new context Δ, by instantiating
    α̂ such that α̂ <: A.

    The @instantiate*@ family of functions should really be called @solve*@
    because their job is to solve an unsolved variable within the context.
    However, for consistency with the paper we still name them @instantiate*@.
-}
instantiateTypeL
    :: (MonadState Status m, MonadError TypeInferenceError m)
    => Existential Monotype -> Type Location -> m ()
instantiateTypeL a _A0 = do
    _Γ0 <- get

    (_Γ', _Γ) <- Context.splitOnUnsolvedType a _Γ0 `orDie` MissingVariable a _Γ0

    let instLSolve τ = do
            wellFormedType _Γ _A0

            set (_Γ' <> (Context.SolvedType a τ : _Γ))

    case Type.node _A0 of
        Type.TypeHole -> do
            b <- fresh

            push (Context.MarkerType b)
            push (Context.UnsolvedType b)

            instantiateTypeL a Type{ location = Type.location _A0, node = Type.UnsolvedType b }

            discardUpTo (Context.MarkerType b)

        -- InstLReach
        Type.UnsolvedType b
            | let _ΓL = _Γ
            , Just (_ΓR, _ΓM) <- Context.splitOnUnsolvedType b _Γ' -> do
                set (_ΓR <> (Context.SolvedType b (Monotype.UnsolvedType a) : _ΓM) <> (Context.UnsolvedType a : _ΓL))

        -- InstLSolve
        Type.UnsolvedType b -> do
            instLSolve (Monotype.UnsolvedType b)
        Type.VariableType b -> do
            instLSolve (Monotype.VariableType b)
        Type.Scalar scalar -> do
            instLSolve (Monotype.Scalar scalar)

        -- InstLExt
        Type.Exists nameLocation b0 Domain.Type _B -> do
            b1 <- fresh

            push (Context.MarkerType b1)
            push (Context.UnsolvedType b1)

            let b1' = Type{ location = nameLocation, node = Type.UnsolvedType b1 }
            instantiateTypeR (Type.substituteType b0 0 b1' _B) a

            discardUpTo (Context.MarkerType b1)
        Type.Exists _ b0 Domain.Fields _B -> do
            b1 <- fresh

            push (Context.MarkerFields b1)
            push (Context.UnsolvedFields b1)

            let b1' = Type.Fields [] (Monotype.UnsolvedFields b1)
            instantiateTypeR (Type.substituteFields b0 0 b1' _B) a

            discardUpTo (Context.MarkerFields b1)
        Type.Exists _ b0 Domain.Alternatives _B -> do
            b1 <- fresh

            push (Context.MarkerAlternatives b1)
            push (Context.UnsolvedAlternatives b1)

            let b1' = Type.Alternatives [] (Monotype.UnsolvedAlternatives b1)
            instantiateTypeR (Type.substituteAlternatives b0 0 b1' _B) a

            discardUpTo (Context.MarkerAlternatives b1)

        -- InstLArr
        Type.Function _A1 _A2 -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            a1 <- fresh
            a2 <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Function (Monotype.UnsolvedType a1) (Monotype.UnsolvedType a2)) : Context.UnsolvedType a1 : Context.UnsolvedType a2 : _ΓL))

            instantiateTypeR _A1 a1

            _Θ <- get

            instantiateTypeL a2 (Context.solveType _Θ _A2)

        -- InstLAllR
        Type.Forall _ b domain _B -> do
            push (Context.Variable domain b)

            instantiateTypeL a _B

            discardUpTo (Context.Variable domain b)

        -- This case is the first example of a general pattern we have to
        -- follow when solving unsolved variables.
        --
        -- Typically when you solve an unsolved variable (e.g. `a`) to some
        -- type (e.g. `A`), you cannot just directly solve the variable as:
        --
        --     a = A
        --
        -- … because unsolved variables can only be solved to `Monotype`s, but
        -- `A` is typically a `Type`.
        --
        -- So, instead, what you do is you solve the variable one layer at a
        -- time.  For example, if you try to solve `a` to (the `Type`)
        -- `Optional (List Bool)`, you will actually get three solved variables
        -- added to the context:
        --
        --     a = Optional b
        --     b = List c
        --     c = Bool
        --
        -- In other words, each time you solve one layer of a complex type, you
        -- need to create a fresh unsolved variable for each inner type and
        -- solve each inner unsolved variable.
        --
        -- This may seem really indirect and tedious, but if you try to skip
        -- this one-layer-at-a-time solving process then you will likely get
        -- bugs due to solved variables referring to each other out of order.
        --
        -- This wasn't obvious to me from reading the original paper since they
        -- didn't really cover how to type-check complex types other than
        -- function types.
        Type.Optional _A -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            -- To solve `a` against `Optional _A` we create a fresh unsolved
            -- variable named `a1`, …
            a1 <- fresh

            -- … solve `a` to `Optional a1`, taking care that `a1` comes before
            -- `a` within the context, (since `a` refers to `a1`)  …
            set (_ΓR <> (Context.SolvedType a (Monotype.Optional (Monotype.UnsolvedType a1)) : Context.UnsolvedType a1 : _ΓL))

            -- … and then solve `a1` against _A`
            instantiateTypeL a1 _A

        -- We solve an unsolved variable against `List` using the same
        -- principles described above for solving `Optional`
        Type.List _A -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            a1 <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.List (Monotype.UnsolvedType a1)) : Context.UnsolvedType a1 : _ΓL))

            instantiateTypeL a1 _A

        -- This is still the same one-layer-at-a-time principle, with a small
        -- twist.  In order to solve:
        --
        --     a = { r }
        --
        -- We replace `r` with a new unsolved Fields variable and then solve for
        -- that Fields variable.
        Type.Record r -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            p <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Record (Monotype.Fields [] (Monotype.UnsolvedFields p))) : Context.UnsolvedFields p : _ΓL))

            instantiateFieldsL p (Type.location _A0) r

        -- Same principle as for `Record`, but replacing the Field variable with
        -- an Alternatives variable
        Type.Union u -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            p <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Union (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives p))) : Context.UnsolvedAlternatives p : _ΓL))

            instantiateAlternativesL p (Type.location _A0) u

{-| This corresponds to the judgment:

    > Γ ⊢ A ≦: α̂ ⊣ Δ

    … which updates the context Γ to produce the new context Δ, by instantiating
    α̂ such that A :< α̂.
-}
instantiateTypeR
    :: (MonadState Status m, MonadError TypeInferenceError m)
    => Type Location -> Existential Monotype -> m ()
instantiateTypeR _A0 a = do
    _Γ0 <- get

    (_Γ', _Γ) <- Context.splitOnUnsolvedType a _Γ0 `orDie` MissingVariable a _Γ0

    let instRSolve τ = do
            wellFormedType _Γ _A0

            set (_Γ' <> (Context.SolvedType a τ : _Γ))

    case Type.node _A0 of
        Type.TypeHole -> do
            b <- fresh

            push (Context.MarkerType b)
            push (Context.UnsolvedType b)

            instantiateTypeL a Type{ location = Type.location _A0, node = Type.UnsolvedType b }

            discardUpTo (Context.MarkerType b)

        -- InstRReach
        Type.UnsolvedType b
            | let _ΓL = _Γ
            , Just (_ΓR, _ΓM) <- Context.splitOnUnsolvedType b _Γ' -> do
                set (_ΓR <> (Context.SolvedType b (Monotype.UnsolvedType a) : _ΓM) <> (Context.UnsolvedType a : _ΓL))

        -- InstRSolve
        Type.UnsolvedType b -> do
            instRSolve (Monotype.UnsolvedType b)
        Type.VariableType b -> do
            instRSolve (Monotype.VariableType b)
        Type.Scalar scalar -> do
            instRSolve (Monotype.Scalar scalar)

        -- InstRArr
        Type.Function _A1 _A2 -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            a1 <- fresh
            a2 <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Function (Monotype.UnsolvedType a1) (Monotype.UnsolvedType a2)) : Context.UnsolvedType a1 : Context.UnsolvedType a2 : _ΓL))

            instantiateTypeL a1 _A1

            _Θ <- get

            instantiateTypeR (Context.solveType _Θ _A2) a2

        -- InstRExtL
        Type.Exists _ b domain _B -> do
            push (Context.Variable domain b)

            instantiateTypeL a _B

            discardUpTo (Context.Variable domain b)

        -- InstRAllL
        Type.Forall nameLocation b0 Domain.Type _B -> do
            b1 <- fresh

            push (Context.MarkerType b1)
            push (Context.UnsolvedType b1)

            let b1' = Type{ location = nameLocation, node = Type.UnsolvedType b1 }
            instantiateTypeR (Type.substituteType b0 0 b1' _B) a

            discardUpTo (Context.MarkerType b1)
        Type.Forall _ b0 Domain.Fields _B -> do
            b1 <- fresh

            push (Context.MarkerFields b1)
            push (Context.UnsolvedFields b1)

            let b1' = Type.Fields [] (Monotype.UnsolvedFields b1)
            instantiateTypeR (Type.substituteFields b0 0 b1' _B) a

            discardUpTo (Context.MarkerFields b1)
        Type.Forall _ b0 Domain.Alternatives _B -> do
            b1 <- fresh

            push (Context.MarkerAlternatives b1)
            push (Context.UnsolvedAlternatives b1)

            let b1' = Type.Alternatives [] (Monotype.UnsolvedAlternatives b1)
            instantiateTypeR (Type.substituteAlternatives b0 0 b1' _B) a

            discardUpTo (Context.MarkerAlternatives b1)

        Type.Optional _A -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            a1 <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Optional (Monotype.UnsolvedType a1)) : Context.UnsolvedType a1 : _ΓL))

            instantiateTypeR _A a1

        Type.List _A -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            a1 <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.List (Monotype.UnsolvedType a1)) : Context.UnsolvedType a1 : _ΓL))

            instantiateTypeR _A a1

        Type.Record r -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            p <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Record (Monotype.Fields [] (Monotype.UnsolvedFields p))) : Context.UnsolvedFields p : _ΓL))

            instantiateFieldsR (Type.location _A0) r p

        Type.Union u -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            p <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Union (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives p))) : Context.UnsolvedAlternatives p : _ΓL))

            instantiateAlternativesR (Type.location _A0) u p

{- The following `equateFields` / `instantiateFieldsL` / `instantiateFieldsR`,
   `equateAlternatives` / `instantiateAlternativesL` /
   `instantiateAlternativesR` judgments are not present in the bidirectional
   type-checking paper.  These were added in order to support row polymorphism
   and variant polymorphism, by following the same general type-checking
   principles as the original paper.

   If you understand how the `instantiateTypeL` and `instantiateTypeR` functions
   work, then you will probably understand how these functions work because they
   follow the same rules:

   * Always make sure that solved variables only reference variables earlier
     within the context

   * Solve for unsolved variables one layer at a time

   Note that the implementation and the user-facing terminology use the term
   fields/alternatives instead of rows/variants, respectively.
-}

equateFields
    :: (MonadState Status m, MonadError TypeInferenceError m)
    => Existential Monotype.Record -> Existential Monotype.Record -> m ()
equateFields p0 p1 = do
    _Γ0 <- get

    let p0First = do
            (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields p1 _Γ0

            Monad.guard (Context.UnsolvedFields p0 `elem` _ΓL)

            return (set (_ΓR <> (Context.SolvedFields p1 (Monotype.Fields [] (Monotype.UnsolvedFields p0)) : _ΓL)))

    let p1First = do
            (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields p0 _Γ0

            Monad.guard (Context.UnsolvedFields p1 `elem` _ΓL)

            return (set (_ΓR <> (Context.SolvedFields p0 (Monotype.Fields [] (Monotype.UnsolvedFields p1)) : _ΓL)))

    case p0First <|> p1First of
        Nothing -> do
            throwError (MissingOneOfFields [] p0 p1 _Γ0)

        Just setContext -> do
            setContext

instantiateFieldsL
    :: (MonadState Status m, MonadError TypeInferenceError m)
    => Existential Monotype.Record -> Location -> Type.Record Location -> m ()
instantiateFieldsL p0 location r@(Type.Fields kAs rest) = do
    when (p0 `Type.fieldsFreeIn` Type{ node = Type.Record r, .. }) do
        throwError (NotFieldsSubtype location p0 r)

    let process (k, _A) = do
            b <- fresh

            return (k, _A, b)

    kAbs <- traverse process kAs

    let bs  = map (\(_, _, b) -> Context.UnsolvedType b      ) kAbs
    let kbs = map (\(k, _, b) -> (k, Monotype.UnsolvedType b)) kAbs

    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields p0 _Γ `orDie` MissingAllFields p0 _Γ

    case rest of
        Monotype.UnsolvedFields p1 -> do
            p2 <- fresh

            set (_ΓR <> (Context.SolvedFields p0 (Monotype.Fields kbs (Monotype.UnsolvedFields p2)) : Context.UnsolvedFields p2 : bs <> _ΓL))

            equateFields p1 p2

        _ -> do
            wellFormedType (bs <> _ΓL)
                Type{ location, node = Type.Record (Type.Fields [] rest) }

            set (_ΓR <> (Context.SolvedFields p0 (Monotype.Fields kbs rest) : bs <> _ΓL))

    let instantiate (_, _A, b) = do
            _Θ <- get

            instantiateTypeL b (Context.solveType _Θ _A)

    traverse_ instantiate kAbs

instantiateFieldsR
    :: (MonadState Status m, MonadError TypeInferenceError m)
    => Location -> Type.Record Location -> Existential Monotype.Record -> m ()
instantiateFieldsR location r@(Type.Fields kAs rest) p0 = do
    when (p0 `Type.fieldsFreeIn` Type{ node = Type.Record r, .. }) do
        throwError (NotFieldsSubtype location p0 r)

    let process (k, _A) = do
            b <- fresh

            return (k, _A, b)

    kAbs <- traverse process kAs

    let bs  = map (\(_, _, b) -> Context.UnsolvedType b      ) kAbs
    let kbs = map (\(k, _, b) -> (k, Monotype.UnsolvedType b)) kAbs

    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields p0 _Γ `orDie` MissingAllFields p0 _Γ

    case rest of
        Monotype.UnsolvedFields p1 -> do
            p2 <- fresh

            set (_ΓR <> (Context.SolvedFields p0 (Monotype.Fields kbs (Monotype.UnsolvedFields p2)) : Context.UnsolvedFields p2 : bs <> _ΓL))

            equateFields p1 p2

        _ -> do
            wellFormedType (bs <> _ΓL)
                Type{ location, node = Type.Record (Type.Fields [] rest) }

            set (_ΓR <> (Context.SolvedFields p0 (Monotype.Fields kbs rest) : bs <> _ΓL))

    let instantiate (_, _A, b) = do
            _Θ <- get

            instantiateTypeR (Context.solveType _Θ _A) b

    traverse_ instantiate kAbs

equateAlternatives
    :: (MonadState Status m, MonadError TypeInferenceError m)
    => Existential Monotype.Union-> Existential Monotype.Union -> m ()
equateAlternatives p0 p1 = do
    _Γ0 <- get

    let p0First = do
            (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives p1 _Γ0

            Monad.guard (Context.UnsolvedAlternatives p0 `elem` _ΓL)

            return (set (_ΓR <> (Context.SolvedAlternatives p1 (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives p0)) : _ΓL)))

    let p1First = do
            (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives p0 _Γ0

            Monad.guard (Context.UnsolvedAlternatives p1 `elem` _ΓL)

            return (set (_ΓR <> (Context.SolvedAlternatives p0 (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives p1)) : _ΓL)))

    case p0First <|> p1First of
        Nothing -> do
            throwError (MissingOneOfAlternatives [] p0 p1 _Γ0)

        Just setContext -> do
            setContext

instantiateAlternativesL
    :: (MonadState Status m, MonadError TypeInferenceError m)
    => Existential Monotype.Union -> Location -> Type.Union Location -> m ()
instantiateAlternativesL p0 location u@(Type.Alternatives kAs rest) = do
    when (p0 `Type.alternativesFreeIn` Type{ node = Type.Union u, .. }) do
        throwError (NotAlternativesSubtype location p0 u)

    let process (k, _A) = do
            b <- fresh

            return (k, _A, b)

    kAbs <- traverse process kAs

    let bs  = map (\(_, _, b) -> Context.UnsolvedType b      ) kAbs
    let kbs = map (\(k, _, b) -> (k, Monotype.UnsolvedType b)) kAbs

    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives p0 _Γ `orDie` MissingAllAlternatives p0 _Γ

    case rest of
        Monotype.UnsolvedAlternatives p1 -> do
            p2 <- fresh

            set (_ΓR <> (Context.SolvedAlternatives p0 (Monotype.Alternatives kbs (Monotype.UnsolvedAlternatives p2)) : Context.UnsolvedAlternatives p2 : bs <> _ΓL))

            equateAlternatives p1 p2

        _ -> do
            wellFormedType (bs <> _ΓL)
                Type{ location, node = Type.Union (Type.Alternatives [] rest) }

            set (_ΓR <> (Context.SolvedAlternatives p0 (Monotype.Alternatives kbs rest) : bs <> _ΓL))

    let instantiate (_, _A, b) = do
            _Θ <- get

            instantiateTypeL b (Context.solveType _Θ _A)

    traverse_ instantiate kAbs

instantiateAlternativesR
    :: (MonadState Status m, MonadError TypeInferenceError m)
    => Location -> Type.Union Location -> Existential Monotype.Union -> m ()
instantiateAlternativesR location u@(Type.Alternatives kAs rest) p0 = do
    when (p0 `Type.alternativesFreeIn` Type{ node = Type.Union u, .. }) do
        throwError (NotAlternativesSubtype location p0 u)

    let process (k, _A) = do
            b <- fresh

            return (k, _A, b)

    kAbs <- traverse process kAs

    let bs  = map (\(_, _, b) -> Context.UnsolvedType b      ) kAbs
    let kbs = map (\(k, _, b) -> (k, Monotype.UnsolvedType b)) kAbs

    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives p0 _Γ `orDie` MissingAllAlternatives p0 _Γ

    case rest of
        Monotype.UnsolvedAlternatives p1 -> do
            p2 <- fresh

            set (_ΓR <> (Context.SolvedAlternatives p0 (Monotype.Alternatives kbs (Monotype.UnsolvedAlternatives p2)) : Context.UnsolvedAlternatives p2 : bs <> _ΓL))

            equateAlternatives p1 p2

        _ -> do
            wellFormedType (bs <> _ΓL)
                Type{ location, node = Type.Union (Type.Alternatives [] rest) }

            set (_ΓR <> (Context.SolvedAlternatives p0 (Monotype.Alternatives kbs rest) : bs <> _ΓL))

    let instantiate (_, _A, b) = do
            _Θ <- get

            instantiateTypeR (Context.solveType _Θ _A) b

    traverse_ instantiate kAbs

{-| This corresponds to the judgment:

    > Γ ⊢ e ⇒ A ⊣ Δ

    … which infers the type of e under input context Γ, producing an inferred
    type of A and an updated context Δ.
-}
infer
    :: (MonadState Status m, MonadError TypeInferenceError m)
    => Syntax Location (Type Location, Value)
    -> m (Type Location)
infer e0 = do
    let _Type :: Type Location
        _Type = Type
            { location = Syntax.location e0
            , node = error "_Type: Uninitialized node field"
            }

    let a ~> b = _Type{ node = Type.Function a b }

    case Syntax.node e0 of
        -- Var
        _A@(Syntax.Variable x0 n) -> do
            _Γ <- get

            Context.lookup x0 n _Γ `orDie` UnboundVariable (Syntax.location e0) x0 n

        -- →I⇒
        Syntax.Lambda nameLocation x e -> do
            a <- fresh
            b <- fresh

            let _A = Type{ location = nameLocation, node = Type.UnsolvedType a }

            let _B =
                    Type{ location = Syntax.location e, node = Type.UnsolvedType b }

            push (Context.UnsolvedType a)
            push (Context.UnsolvedType b)
            push (Context.Annotation x _A)

            check e _B

            discardUpTo (Context.Annotation x _A)

            return _Type{ node = Type.Function _A _B }

        -- →E
        Syntax.Application e1 e2 -> do
            _A <- infer e1

            _Θ <- get

            inferApplication (Context.solveType _Θ _A) e2

        -- Anno
        Syntax.Annotation e _A -> do
            _Γ <- get

            wellFormedType _Γ _A

            check e _A

            return _A

        Syntax.Let bindings b -> do
            let process Syntax.Binding{ name, annotation = Nothing, assignment } = do
                    _A <- infer assignment

                    push (Context.Annotation name _A)
                process Syntax.Binding{ name, annotation = Just _A,  assignment } = do
                    check assignment _A

                    push (Context.Annotation name _A)

            traverse_ process bindings

            infer b

        Syntax.List xs -> do
            case Seq.viewl xs of
                EmptyL -> do
                    a <- fresh

                    push (Context.UnsolvedType a)

                    return _Type
                        { node = Type.List _Type{ node = Type.UnsolvedType a }
                        }
                y :< ys -> do
                    _A <- infer y

                    let process element = do
                            _Γ <- get

                            check element (Context.solveType _Γ _A)

                    traverse_ process ys

                    return _Type{ node = Type.List _A }

        Syntax.Record kvs -> do
            let process (k, v) = do
                    _A <- infer v

                    return (k, _A)

            kAs <- traverse process kvs

            return _Type{ node = Type.Record (Type.Fields kAs Monotype.EmptyFields) }

        Syntax.Alternative k -> do
            a <- fresh
            p <- fresh

            push (Context.UnsolvedType a)
            push (Context.UnsolvedAlternatives p)

            return _Type
                { node =
                    Type.Function
                        _Type{ node = Type.UnsolvedType a }
                        _Type
                            { node =
                                Type.Union
                                    (Type.Alternatives
                                        [ ( k
                                          , _Type{ node = Type.UnsolvedType a }
                                          )
                                        ]
                                        (Monotype.UnsolvedAlternatives p)
                                    )
                            }
                }

        Syntax.Merge record -> do
            p <- fresh

            push (Context.UnsolvedFields p)

            let _R = Type{ location = Syntax.location record, node = Type.Record (Type.Fields [] (Monotype.UnsolvedFields p)) }

            check record _R

            _Γ <- get

            let _R' = Context.solveType _Γ _R

            case Type.node _R' of
                Type.Record (Type.Fields keyTypes Monotype.EmptyFields) -> do
                    b <- fresh

                    push (Context.UnsolvedType b)

                    let process (key, Type{ node = Type.Function _A _B }) = do
                            _ϴ <- get

                            let b' = Type
                                    { location = Type.location _B
                                    , node = Type.UnsolvedType b
                                    }
                            subtype (Context.solveType _ϴ _B) (Context.solveType _ϴ b')

                            return (key, _A)
                        process (_, _A) = do
                            throwError (MergeInvalidHandler (Type.location _A) _A)

                    keyTypes' <- traverse process keyTypes

                    return _Type
                        { node =
                            Type.Function
                                _Type
                                    { node =
                                        Type.Union
                                            (Type.Alternatives
                                                keyTypes'
                                                Monotype.EmptyAlternatives
                                            )
                                    }
                                _Type{ node = Type.UnsolvedType b }
                        }

                Type.Record _ -> do
                    throwError (MergeConcreteRecord (Type.location _R) _R)

                _ -> do
                    throwError (MergeRecord (Type.location _R) _R)

        Syntax.Field record location key -> do
            a <- fresh
            p <- fresh

            push (Context.UnsolvedType a)
            push (Context.UnsolvedFields p)

            check record Type
                { location
                , node =
                    Type.Record
                        (Type.Fields
                            [ ( key
                              , Type{ location , node = Type.UnsolvedType a }
                              )
                            ]
                            (Monotype.UnsolvedFields p)
                        )
                }

            return Type{ location, node = Type.UnsolvedType a }

        Syntax.If predicate l r -> do
            check predicate _Type{ node = Type.Scalar Monotype.Bool }

            _L0 <- infer l

            _Γ  <- get

            let _L1 = Context.solveType _Γ _L0

            check r _L1

            return _L1

        -- All the type inference rules for scalars go here.  This part is
        -- pretty self-explanatory: a scalar literal returns the matching
        -- scalar type.
        Syntax.Scalar (Syntax.Bool _) -> do
            return _Type{ node = Type.Scalar Monotype.Bool }

        Syntax.Scalar (Syntax.Real _) -> do
            return _Type{ node = Type.Scalar Monotype.Real }

        Syntax.Scalar (Syntax.Integer _) -> do
            return _Type{ node = Type.Scalar Monotype.Integer }

        Syntax.Scalar (Syntax.Natural _) -> do
            return _Type{ node = Type.Scalar Monotype.Natural }

        Syntax.Scalar (Syntax.Text _) -> do
            return _Type{ node = Type.Scalar Monotype.Text }

        Syntax.Scalar Syntax.Null -> do
            -- NOTE: You might think that you could just infer that `null`
            -- has type `forall (a : Type) . Optional a`.  This does not work
            -- because it will lead to data structures with impredicative types
            -- if you store a `null` inside of, say, a `List`.
            a <- fresh

            push (Context.UnsolvedType a)

            return _Type
                { node = Type.Optional _Type{ node = Type.UnsolvedType a } }

        Syntax.Operator l location Syntax.And r -> do
            check l Type{ location, node = Type.Scalar Monotype.Bool }
            check r Type{ location, node = Type.Scalar Monotype.Bool }

            return Type{ location, node = Type.Scalar Monotype.Bool }

        Syntax.Operator l location Syntax.Or r -> do
            check l Type{ location, node = Type.Scalar Monotype.Bool }
            check r Type{ location, node = Type.Scalar Monotype.Bool }

            return Type{ location, node = Type.Scalar Monotype.Bool }

        Syntax.Operator l _ Syntax.Times r -> do
            _L <- infer l
            _R <- infer r

            check l _R
            check r _L

            _Γ <- get

            let _L' = Context.solveType _Γ _L

            case node _L' of
                Type.Scalar Monotype.Natural -> return _L
                Type.Scalar Monotype.Integer -> return _L
                Type.Scalar Monotype.Real    -> return _L
                _ -> do
                    throwError (InvalidOperands (Syntax.location l) _L')

        Syntax.Operator l _ Syntax.Plus r -> do
            _L <- infer l
            _R <- infer r

            check l _R
            check r _L

            _Γ <- get

            let _L' = Context.solveType _Γ _L

            case node _L' of
                Type.Scalar Monotype.Natural -> return _L
                Type.Scalar Monotype.Integer -> return _L
                Type.Scalar Monotype.Real    -> return _L
                Type.Scalar Monotype.Text    -> return _L
                Type.List   _                -> return _L

                _ -> do
                    throwError (InvalidOperands (Syntax.location l) _L')

        Syntax.Builtin Syntax.RealEqual-> do
            return
                (   _Type{ node = Type.Scalar Monotype.Real }
                ~>  (   _Type{ node = Type.Scalar Monotype.Real }
                    ~>  _Type{ node = Type.Scalar Monotype.Bool }
                    )
                )

        Syntax.Builtin Syntax.RealLessThan -> do
            return
                (   _Type{ node = Type.Scalar Monotype.Real }
                ~>  (   _Type{ node = Type.Scalar Monotype.Real }
                    ~>  _Type{ node = Type.Scalar Monotype.Bool }
                    )
                )

        Syntax.Builtin Syntax.RealNegate -> do
            return
                (   _Type{ node = Type.Scalar Monotype.Real }
                ~>  _Type{ node = Type.Scalar Monotype.Real }
                )

        Syntax.Builtin Syntax.RealShow -> do
            return
                (   _Type{ node = Type.Scalar Monotype.Real }
                ~>  _Type{ node = Type.Scalar Monotype.Text }
                )

        Syntax.Builtin Syntax.ListDrop -> do
            return _Type
                { node =
                    Type.Forall (Syntax.location e0) "a" Domain.Type
                        (   _Type{ node = Type.Scalar Monotype.Natural }
                        ~>  (   _Type{ node = Type.List _Type{ node = "a" } }
                            ~>  _Type{ node = Type.List _Type{ node = "a" } }
                            )
                        )
                }

        Syntax.Builtin Syntax.ListHead -> do
            return _Type
                { node =
                    Type.Forall (Syntax.location e0) "a" Domain.Type _Type
                        { node =
                            Type.Forall (Syntax.location e0) "b" Domain.Alternatives
                                (   _Type{ node = Type.List _Type{ node = "a" } }
                                ~>  _Type
                                        { node =
                                            Type.Union
                                                (Type.Alternatives
                                                    [ ("Some", _Type{ node = "a" })
                                                    , ("None", _Type{ node = Type.Record (Type.Fields [] Monotype.EmptyFields) })
                                                    ]
                                                    (Monotype.VariableAlternatives "b")
                                                )
                                        }
                                )
                        }
                }

        Syntax.Builtin Syntax.ListEqual -> do
            return _Type
                { node =
                    Type.Forall (Syntax.location e0) "a" Domain.Type
                        (   (   _Type{ node = "a" }
                            ~>  (  _Type{ node = "a" }
                                ~> _Type{ node = Type.Scalar Monotype.Bool }
                                )
                            )
                        ~>  (   _Type{ node = Type.List _Type{ node = "a" } }
                            ~>  (   _Type{ node = Type.List _Type{ node = "a" } }
                                ~>  _Type{ node = Type.Scalar Monotype.Bool }
                                )
                            )
                        )
                }

        Syntax.Builtin Syntax.ListFold -> do
            return _Type
                { node =
                    Type.Forall (Syntax.location e0) "a" Domain.Type _Type
                        { node =
                            Type.Forall (Syntax.location e0) "b" Domain.Type
                                (   _Type
                                        { node =
                                            Type.Record
                                                (Type.Fields
                                                    [ ("cons", _Type{ node = "a" } ~> (_Type{ node = "b" } ~> _Type{ node = "b" }))
                                                    , ("nil", _Type{ node = "b" })
                                                    ]
                                                    Monotype.EmptyFields
                                                )
                                        }
                                ~>  (   _Type{ node = Type.List _Type{ node = "a" } }
                                    ~>  _Type{ node = "b" }
                                    )
                                )
                        }
                }


        Syntax.Builtin Syntax.ListIndexed -> do
            return _Type
                { node =
                    Type.Forall (Syntax.location e0) "a" Domain.Type
                        (   _Type{ node = Type.List _Type{ node = "a" } }
                        ~>  _Type
                                { node =
                                    Type.List _Type
                                        { node =
                                            Type.Record
                                                (Type.Fields
                                                    [ ("index", _Type{ node = Type.Scalar Monotype.Natural })
                                                    , ("value", _Type{ node = "a" })
                                                    ]
                                                    Monotype.EmptyFields
                                                )
                                        }
                                }
                        )
                }

        Syntax.Builtin Syntax.ListLast -> do
            return _Type
                { node =
                    Type.Forall (Syntax.location e0) "a" Domain.Type _Type
                        { node =
                            Type.Forall (Syntax.location e0) "b" Domain.Alternatives
                                (   _Type{ node = Type.List _Type{ node = "a" } }
                                ~>  _Type
                                        { node =
                                            Type.Union
                                                (Type.Alternatives
                                                    [ ("Some", _Type{ node = "a" })
                                                    , ("None", _Type{ node = Type.Record (Type.Fields [] Monotype.EmptyFields) })
                                                    ]
                                                    (Monotype.VariableAlternatives "b")
                                                )
                                        }
                                )
                        }
                }

        Syntax.Builtin Syntax.ListLength -> do
            return _Type
                { node =
                    Type.Forall (Syntax.location e0) "a" Domain.Type
                        (   _Type{ node = Type.List _Type{ node = "a" } }
                        ~>  _Type{ node = Type.Scalar Monotype.Natural }
                        )
                }

        Syntax.Builtin Syntax.ListMap -> do
            return _Type
                { node =
                    Type.Forall (Syntax.location e0) "a" Domain.Type _Type
                        { node =
                            Type.Forall (Syntax.location e0) "b" Domain.Type
                                (   (   _Type{ node = "a" }
                                    ~>  _Type{ node = "b" }
                                    )
                                ~>  (   _Type{ node = Type.List _Type{ node = "a" } }
                                    ~>  _Type{ node = Type.List _Type{ node = "b" } }
                                    )
                                )
                        }
                }

        Syntax.Builtin Syntax.IntegerAbs -> do
            return
                (   _Type{ node = Type.Scalar Monotype.Integer }
                ~>  _Type{ node = Type.Scalar Monotype.Natural }
                )

        Syntax.Builtin Syntax.IntegerEven -> do
            return
                (   _Type{ node = Type.Scalar Monotype.Integer }
                ~>  _Type{ node = Type.Scalar Monotype.Bool }
                )

        Syntax.Builtin Syntax.IntegerNegate -> do
            return
                (   _Type{ node = Type.Scalar Monotype.Integer }
                ~>  _Type{ node = Type.Scalar Monotype.Integer }
                )

        Syntax.Builtin Syntax.IntegerOdd -> do
            return
                (   _Type{ node = Type.Scalar Monotype.Integer }
                ~>  _Type{ node = Type.Scalar Monotype.Bool }
                )

        Syntax.Builtin Syntax.ListReverse -> do
            return _Type
                { node =
                    Type.Forall (Syntax.location e0) "a" Domain.Type
                        (   _Type{ node = Type.List _Type{ node = "a" } }
                        ~>  _Type{ node = Type.List _Type{ node = "a" } }
                        )
                }

        Syntax.Builtin Syntax.ListTake -> do
            return _Type
                { node =
                    Type.Forall (Syntax.location e0) "a" Domain.Type
                        (   _Type{ node = Type.Scalar Monotype.Natural }
                        ~>  (   _Type{ node = Type.List _Type{ node = "a" } }
                            ~>  _Type{ node = Type.List _Type{ node = "a" } }
                            )
                        )
                }

        Syntax.Builtin Syntax.JSONFold -> do
            return _Type
                { node =
                    Type.Forall (Syntax.location e0) "a" Domain.Type
                        (   _Type
                                { node =
                                    Type.Record
                                        (Type.Fields
                                            [ ( "array", _Type{ node = Type.List _Type{ node = "a" } } ~> _Type{ node = "a" })
                                            , ("bool", _Type{ node = Type.Scalar Monotype.Bool } ~> _Type{ node = "a" })
                                            , ("double", _Type{ node = Type.Scalar Monotype.Real } ~> _Type{ node = "a" })
                                            , ("integer", _Type{ node = Type.Scalar Monotype.Integer } ~> _Type{ node = "a" })
                                            , ("natural", _Type{ node = Type.Scalar Monotype.Natural } ~> _Type{ node = "a" })
                                            , ("null", _Type{ node = "a" })
                                            , ( "object"
                                              ,     _Type
                                                        { node =
                                                            Type.List
                                                                _Type
                                                                    { node =
                                                                        Type.Record
                                                                            (Type.Fields
                                                                                [ ("key", _Type{ node = Type.Scalar Monotype.Text })
                                                                                , ("value", _Type{ node = "a" })
                                                                                ]
                                                                                Monotype.EmptyFields
                                                                            )
                                                                    }
                                                        }
                                                ~>  _Type{ node = "a" }
                                              )
                                            , ("string", _Type{ node = Type.Scalar Monotype.Text } ~> _Type{ node = "a" })
                                            ]
                                            Monotype.EmptyFields
                                        )
                                }
                        ~>  (   _Type{ node = Type.Scalar Monotype.JSON }
                            ~>  _Type{ node = "a" }
                            )
                        )
                }

        Syntax.Builtin Syntax.NaturalFold -> do
            return _Type
                { node =
                    Type.Forall (Syntax.location e0) "a" Domain.Type
                        (   _Type{ node = Type.Scalar Monotype.Natural }
                        ~>  (  (_Type{ node = "a" } ~> _Type{ node = "a" })
                            ~> (_Type{ node = "a" } ~> _Type{ node = "a" })
                            )
                        )
                }

        Syntax.Builtin Syntax.TextEqual-> do
            return
                (   _Type{ node = Type.Scalar Monotype.Text }
                ~>  (   _Type{ node = Type.Scalar Monotype.Text }
                    ~>  _Type{ node = Type.Scalar Monotype.Bool }
                    )
                )

        Syntax.Embed (type_, _) -> do
            return type_

{-| This corresponds to the judgment:

    > Γ ⊢ e ⇐ A ⊣ Δ

    … which checks that e has type A under input context Γ, producing an updated
    context Δ.
-}
check
    :: (MonadState Status m, MonadError TypeInferenceError m)
    => Syntax Location (Type Location, Value) -> Type Location -> m ()
-- The check function is the most important function to understand for the
-- bidirectional type-checking algorithm.
--
-- Most people, when they first run across the `check` function think that you
-- could get rid of most rules except for the final `Sub` rule, but that's not
-- true!
--
-- The reason you should add `check` rules for many more types (especially
-- complex types) is to ensure that subtyping rules work correctly.  For
-- example, consider this expression:
--
--     [ 2, -3 ]
--
-- If you omit the `check` rule for `List`s then the above expression will
-- fail to type-check because the first element of the list is a `Natural`
-- number and the second element of the `List` is an `Integer`.
--
-- However, if you keep the `check` rule for `List`s and add a type annotation:
--
--     [ 2, -3 ] : List Integer
--
-- … then it works because the interpreter knows to treat both elements as an
-- `Integer`.
--
-- In general, if you want subtyping to work reliably then you need to add
-- more cases to the `check` function so that the interpreter can propagate
-- top-level type annotations down to the "leaves" of your syntax tree.  If
-- you do this consistently then the user only ever needs to provide top-level
-- type annotations to fix any type errors that they might encounter, which is
-- a desirable property!

check e _A@Type{ node = Type.TypeHole } = do
    a <- fresh

    push (Context.MarkerType a)
    push (Context.UnsolvedType a)

    check e _A{ node = Type.UnsolvedType a }

    discardUpTo (Context.MarkerType a)

-- →I
check Syntax{ node = Syntax.Lambda _ x e } Type{ node = Type.Function _A _B } = do
    push (Context.Annotation x _A)

    check e _B

    discardUpTo (Context.Annotation x _A)

-- ∃I
check e Type{ node = Type.Exists nameLocation a0 Domain.Type _A } = do
    a1 <- fresh

    push (Context.MarkerType a1)
    push (Context.UnsolvedType a1)

    let a1' = Type{ location = nameLocation, node = Type.UnsolvedType a1 }

    check e (Type.substituteType a0 0 a1' _A)

    discardUpTo (Context.MarkerType a1)
check e Type{ node = Type.Exists _ a0 Domain.Fields _A } = do
    a1 <- fresh

    push (Context.MarkerFields a1)
    push (Context.UnsolvedFields a1)

    let a1' = Type.Fields [] (Monotype.UnsolvedFields a1)

    check e (Type.substituteFields a0 0 a1' _A)

    discardUpTo (Context.MarkerFields a1)
check e Type{ node = Type.Exists _ a0 Domain.Alternatives _A } = do
    a1 <- fresh

    push (Context.MarkerAlternatives a1)
    push (Context.UnsolvedAlternatives a1)

    let a1' = Type.Alternatives [] (Monotype.UnsolvedAlternatives a1)

    check e (Type.substituteAlternatives a0 0 a1' _A)

    discardUpTo (Context.MarkerAlternatives a1)

-- ∀I
check e Type{ node = Type.Forall _ a domain _A } = do
    push (Context.Variable domain a)

    check e _A

    discardUpTo (Context.Variable domain a)

check Syntax{ node = Syntax.Operator l _ Syntax.Times r } _B@Type{ node = Type.Scalar scalar }
    | scalar `elem` ([ Monotype.Natural, Monotype.Integer, Monotype.Real ] :: [Monotype.Scalar])= do
    check l _B

    _Γ <- get

    check r (Context.solveType _Γ _B)
check Syntax{ node = Syntax.Operator l _ Syntax.Plus r } _B@Type{ node = Type.Scalar scalar }
    | scalar `elem` ([ Monotype.Natural, Monotype.Integer, Monotype.Real, Monotype.Text ] :: [Monotype.Scalar]) = do
    check l _B

    _Γ <- get

    check r (Context.solveType _Γ _B)
check Syntax{ node = Syntax.Operator l _ Syntax.Plus r } _B@Type{ node = Type.List _ } = do
    check l _B

    _Γ <- get

    check r (Context.solveType _Γ _B)

check Syntax{ node = Syntax.List elements } Type{ node = Type.List a } = do
    let process element = do
            _Γ <- get

            check element (Context.solveType _Γ a)

    traverse_ process elements

check e@Syntax{ node = Syntax.Record keyValues } _B@Type{ node = Type.Record (Type.Fields keyTypes fields) }
    | let mapValues = Map.fromList keyValues
    , let mapTypes  = Map.fromList keyTypes

    , let extraValues = Map.difference mapValues mapTypes
    , let extraTypes  = Map.difference mapTypes  mapValues

    , let both = Map.intersectionWith (,) mapValues mapTypes
    , not (Map.null both) = do
        let process (value, type_) = do
                _Γ <- get

                check value (Context.solveType _Γ type_)

        traverse_ process both

        let e' =
                Syntax
                    { node = Syntax.Record (Map.toList extraValues)
                    , location = Syntax.location e
                    }

        let _B' =
                Type
                    { node = Type.Record (Type.Fields (Map.toList extraTypes) fields)
                    , location = Type.location _B
                    }

        _Γ <- get

        check e' (Context.solveType _Γ _B')

check Syntax{ node = Syntax.Record keyValues } _B@Type{ node = Type.Scalar Monotype.JSON } = do
    let process (_, value) = do
            _Γ <- get

            check value (Context.solveType _Γ _B)

    traverse_ process keyValues
check Syntax{ node = Syntax.List elements } _B@Type{ node = Type.Scalar Monotype.JSON } = do
    traverse_ (`check` _B) elements
check Syntax{ node = Syntax.Scalar (Syntax.Text _) } _B@Type{ node = Type.Scalar Monotype.JSON } = do
    return ()
check Syntax{ node = Syntax.Scalar (Syntax.Natural _) } _B@Type{ node = Type.Scalar Monotype.JSON } = do
    return ()
check Syntax{ node = Syntax.Scalar (Syntax.Integer _) } _B@Type{ node = Type.Scalar Monotype.JSON } = do
    return ()
check Syntax{ node = Syntax.Scalar (Syntax.Real _) } _B@Type{ node = Type.Scalar Monotype.JSON } = do
    return ()
check Syntax{ node = Syntax.Scalar (Syntax.Bool _) } _B@Type{ node = Type.Scalar Monotype.JSON } = do
    return ()
check Syntax{ node = Syntax.Scalar Syntax.Null } _B@Type{ node = Type.Scalar Monotype.JSON } = do
    return ()

-- Sub
check e _B = do
    _A <- infer e

    _Θ <- get

    subtype (Context.solveType _Θ _A) (Context.solveType _Θ _B)

{-| This corresponds to the judgment:

    > Γ ⊢ A • e ⇒⇒ C ⊣ Δ

    … which infers the result type C when a function of type A is applied to an
    input argument e, under input context Γ, producing an updated context Δ.
-}
inferApplication
    :: (MonadState Status m, MonadError TypeInferenceError m)
    => Type Location
    -> Syntax Location (Type Location, Value)
    -> m (Type Location)
-- ∀App
inferApplication _A0@Type{ node = Type.Forall nameLocation a0 Domain.Type _A } e = do
    a1 <- fresh

    push (Context.UnsolvedType a1)

    let a1' = Type{ location = nameLocation, node = Type.UnsolvedType a1 }

    inferApplication (Type.substituteType a0 0 a1' _A) e
inferApplication _A0@Type{ node = Type.Forall _ a0 Domain.Fields _A } e = do
    a1 <- fresh

    push (Context.UnsolvedFields a1)

    let a1' = Type.Fields [] (Monotype.UnsolvedFields a1)

    inferApplication (Type.substituteFields a0 0 a1' _A) e
inferApplication _A0@Type{ node = Type.Forall _ a0 Domain.Alternatives _A } e = do
    a1 <- fresh

    push (Context.UnsolvedAlternatives a1)

    let a1' = Type.Alternatives [] (Monotype.UnsolvedAlternatives a1)

    inferApplication (Type.substituteAlternatives a0 0 a1' _A) e

-- ∃App
inferApplication _A0@Type{ node = Type.Exists _ a domain _A } e = do
    push (Context.Variable domain a)

    _B <- inferApplication _A e

    discardUpTo (Context.Variable domain a)

    return _B

-- αApp
inferApplication Type{ node = Type.UnsolvedType a, .. } e = do
    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedType a _Γ `orDie` MissingVariable a _Γ

    a1 <- fresh
    a2 <- fresh

    set (_ΓR <> (Context.SolvedType a (Monotype.Function (Monotype.UnsolvedType a1) (Monotype.UnsolvedType a2)) : Context.UnsolvedType a1 : Context.UnsolvedType a2 : _ΓL))

    check e Type{ node = Type.UnsolvedType a1, .. }

    return Type{ node = Type.UnsolvedType a2, .. }
inferApplication Type{ node = Type.Function _A _C } e = do
    check e _A

    return _C
inferApplication Type{ node = Type.VariableType a, ..} _ = do
    throwError (NotNecessarilyFunctionType location a)
inferApplication _A@Type{..} _ = do
    throwError (NotFunctionType location _A)

-- | Infer the `Type` of the given `Syntax` tree
typeOf
    :: Syntax Location (Type Location, Value)
    -> Either TypeInferenceError (Type Location)
typeOf = typeWith []

-- | Like `typeOf`, but accepts a custom type-checking `Context`
typeWith
    :: Context Location
    -> Syntax Location (Type Location, Value)
    -> Either TypeInferenceError (Type Location)
typeWith context syntax = do
    let initialStatus = Status{ count = 0, context }

    (_A, Status{ context = _Δ }) <- State.runStateT (infer syntax) initialStatus

    return (Context.complete _Δ _A)

-- | A data type holding all errors related to type inference
data TypeInferenceError
    = IllFormedAlternatives Location (Existential Monotype.Union) (Context Location)
    | IllFormedFields Location (Existential Monotype.Record) (Context Location)
    | IllFormedType Location (Type.Node Location) (Context Location)
    --
    | InvalidOperands Location (Type Location)
    --
    | MergeConcreteRecord Location (Type Location)
    | MergeInvalidHandler Location (Type Location)
    | MergeRecord Location (Type Location)
    --
    | MissingAllAlternatives (Existential Monotype.Union) (Context Location)
    | MissingAllFields (Existential Monotype.Record) (Context Location)
    | MissingOneOfAlternatives [Location] (Existential Monotype.Union) (Existential Monotype.Union) (Context Location)
    | MissingOneOfFields [Location] (Existential Monotype.Record) (Existential Monotype.Record) (Context Location)
    | MissingVariable (Existential Monotype) (Context Location)
    --
    | NotFunctionType Location (Type Location)
    | NotNecessarilyFunctionType Location Text
    --
    | NotAlternativesSubtype Location (Existential Monotype.Union) (Type.Union Location)
    | NotFieldsSubtype Location (Existential Monotype.Record) (Type.Record Location)
    | NotRecordSubtype Location (Type.Node Location) Location (Type.Node Location)
    | NotUnionSubtype Location (Type.Node Location) Location (Type.Node Location)
    | NotSubtype Location (Type.Node Location) Location (Type.Node Location)
    --
    | UnboundAlternatives Location Text
    | UnboundFields Location Text
    | UnboundTypeVariable Location Text
    | UnboundVariable Location Text Int
    --
    | RecordTypeMismatch (Type Location) (Type Location) (Map.Map Text (Type Location)) (Map.Map Text (Type Location))
    | UnionTypeMismatch (Type Location) (Type Location) (Map.Map Text (Type Location)) (Map.Map Text (Type Location))
    deriving (Eq, Show)

instance Exception TypeInferenceError where
    displayException (IllFormedAlternatives location a0 _Γ) = [__i|
        Internal error: Invalid context

        The following unsolved alternatives variable:

        #{insert (Context.UnsolvedAlternatives a0)}

        … is not well-formed within the following context:

        #{listToText _Γ}

        #{Location.renderError "" location}
        |]

    displayException (IllFormedFields location a0 _Γ) = [__i|
        Internal error: Invalid context

        The following unsolved fields variable:

        #{insert (Context.UnsolvedFields a0)}

        … is not well-formed within the following context:

        #{listToText _Γ}

        #{Location.renderError "" location}
        |]

    displayException (IllFormedType location _A _Γ) = [__i|
        Internal error: Invalid context

        The following type:

        #{insert _A}

        … is not well-formed within the following context:

        #{listToText _Γ}

        #{Location.renderError "" location}
        |]

    displayException (InvalidOperands location _L') = [__i|
        Invalid operands

        You cannot add values of type:

        #{insert _L'}

        #{Location.renderError "" location}
        |]

    displayException (MergeConcreteRecord location _R) = [__i|
        Must merge a concrete record

        The first argument to a merge expression must be a record where all fields are
        statically known.  However, you provided an argument of type:

        #{insert _R}

        #{Location.renderError "" location}

        … where not all fields could be inferred.
        |]

    displayException (MergeInvalidHandler location _A) = [__i|
        Invalid handler

        The merge keyword expects a record of handlers where all handlers are functions,
        but you provided a handler of the following type:

        #{insert _A}

        #{Location.renderError "" location}

        … which is not a function type.
        |]

    displayException (MergeRecord location _R) = [__i|
        Must merge a record

        The first argument to a merge expression must be a record, but you provided an
        expression of the following type:

        #{insert _R}

        #{Location.renderError "" location}

        … which is not a record type.
        |]

    displayException (MissingAllAlternatives p0 _Γ) = [__i|
        Internal error: Invalid context

        The following unsolved alternatives variable:

        #{insert (Context.UnsolvedAlternatives p0)}}

        … cannot be instantiated because the alternatives variable is missing from the
        context:

        #{listToText _Γ}
        |]

    displayException (MissingAllFields p0 _Γ) = [__i|
        Internal error: Invalid context

        The following unsolved fields variable:

        #{insert (Context.UnsolvedFields p0)}

        … cannot be instantiated because the fields variable is missing from the
        context:

        #{listToText _Γ}
        |]

    displayException (MissingOneOfAlternatives locations p0 p1 _Γ) = [__i|
        Internal error: Invalid context

        One of the following alternatives variables:

        #{listToText [Context.UnsolvedAlternatives p0, Context.UnsolvedAlternatives p1 ]}

        … is missing from the following context:

        #{listToText _Γ}

        #{locations'}
        |]
        where
            locations' = Text.unlines (map (Location.renderError "") locations)

    displayException (MissingOneOfFields locations p0 p1 _Γ) = [__i|
        Internal error: Invalid context

        One of the following fields variables:

        #{listToText [Context.UnsolvedFields p0, Context.UnsolvedFields p1 ]}

        … is missing from the following context:

        #{listToText _Γ}

        #{locations'}
        |]
        where
            locations' = Text.unlines (map (Location.renderError "") locations)

    displayException (MissingVariable a _Γ) = [__i|
        Internal error: Invalid context

        The following unsolved variable:

        #{insert (Context.UnsolvedType a)}

        … cannot be solved because the variable is missing from the context:

        #{listToText _Γ}
        |]

    displayException (NotFunctionType location _A) = [__i|
        Not a function type

        An expression of the following type:

        #{insert _A}

        #{Location.renderError "" location}

        … was invoked as if it were a function, but the above type is not a function
        type.
        |]

    displayException (NotNecessarilyFunctionType location a) = [__i|
        Not necessarily a function type

        The following type variable:

        #{insert a}

        … could potentially be any type and is not necessarily a function type.

        #{Location.renderError "" location}
        |]

    displayException (NotAlternativesSubtype location p0 u) = [__i|
        Not an alternatives subtype

        The following alternatives variable:

        #{insert p0}

        … cannot be instantiated to the following union type:

        #{insert (Type.Union u)}

        #{Location.renderError "" location}

        … because the same alternatives variable appears within that union type.
        |]

    displayException (NotFieldsSubtype location p0 r) = [__i|
        Not a fields subtype

        The following fields variable:

        #{insert p0}

        … cannot be instantiated to the following record type:

        #{insert (Type.Record r)}

        #{Location.renderError "" location}

        … because the same fields variable appears within that record type.
        |]

    displayException (NotRecordSubtype locA0 _A locB0 _B) = [__i|
        Not a record subtype

        The following type:

        #{insert _A}

        #{Location.renderError "" locA0}

        … cannot be a subtype of:

        #{insert _B}

        #{Location.renderError "" locB0}
        |]

    displayException (NotUnionSubtype locA0 _A locB0 _B) = [__i|
        Not a union subtype

        The following type:

        #{insert _A}

        #{Location.renderError "" locA0}

        … cannot be a subtype of:

        #{insert _B}

        #{Location.renderError "" locB0}
        |]

    displayException (NotSubtype locA0 _A locB0 _B) = [__i|
        Not a subtype

        The following type:

        #{insert _A}

        #{Location.renderError "" locA0}

        … cannot be a subtype of:

        #{insert _B}

        #{Location.renderError "" locB0}
        |]

    displayException (UnboundAlternatives location a) = [__i|
        Unbound alternatives variable: #{a}

        #{Location.renderError "" location}
        |]

    displayException (UnboundFields location a) = [__i|
        Unbound fields variable: #{a}

        #{Location.renderError "" location}
        |]

    displayException (UnboundTypeVariable location a) = [__i|
        Unbound type variable: #{a}

        #{Location.renderError "" location}
        |]

    displayException (UnboundVariable location x n) = [__i|
        Unbound variable: #{var}

        #{Location.renderError "" location }
        |]
        where
            var = prettyToText @(Syntax.Node () Void) (Syntax.Variable x n)

    displayException (RecordTypeMismatch _A0 _B0 extraA extraB) | extraB == mempty = [__i|
        Record type mismatch

        The following record type:

        #{insert _A0}

        #{Location.renderError "" (Type.location _A0)}

        … is not a subtype of the following record type:

        #{insert _B0}

        #{Location.renderError "" (Type.location _B0)}

        The former record has the following extra fields:

        #{listToText (Map.keys extraA)}
        |]

    displayException (RecordTypeMismatch _A0 _B0 extraA extraB) | extraA == mempty = [__i|
        Record type mismatch

        The following record type:

        #{insert _A0}

        #{Location.renderError "" (Type.location _A0)}

        … is not a subtype of the following record type:

        #{insert _B0}

        #{Location.renderError "" (Type.location _B0)}

        The latter record has the following extra fields:

        #{listToText (Map.keys extraB)}
        |]

    displayException (RecordTypeMismatch _A0 _B0 extraA extraB) = [__i|
        Record type mismatch

        The following record type:

        #{insert _A0}

        #{Location.renderError "" (Type.location _A0)}

        … is not a subtype of the following record type:

        #{insert _B0}

        #{Location.renderError "" (Type.location _B0)}

        The former record has the following extra fields:

        #{listToText (Map.keys extraA)}

        … while the latter record has the following extra fields:

        #{listToText (Map.keys extraB)}
        |]

    displayException (UnionTypeMismatch _A0 _B0 extraA extraB) | extraB == mempty = [__i|
        Union type mismatch

        The following union type:

        #{insert _A0}

        #{Location.renderError "" (Type.location _A0)}

        … is not a subtype of the following union type:

        #{insert _B0}

        #{Location.renderError "" (Type.location _B0)}

        The former union has the following extra alternatives:

        #{listToText (Map.keys extraA)}
        |]

    displayException (UnionTypeMismatch _A0 _B0 extraA extraB) | extraA == mempty = [__i|
        Union type mismatch

        The following union type:

        #{insert _A0}

        #{Location.renderError "" (Type.location _A0)}

        … is not a subtype of the following union type:

        #{insert _B0}

        #{Location.renderError "" (Type.location _B0)}

        The latter union has the following extra alternatives:

        #{listToText (Map.keys extraB)}
        |]

    displayException (UnionTypeMismatch _A0 _B0 extraA extraB) = [__i|
        Union type mismatch

        The following union type:

        #{insert _A0}

        #{Location.renderError "" (Type.location _A0)}

        … is not a subtype of the following union type:

        #{insert _B0}

        #{Location.renderError "" (Type.location _B0)}

        The former union has the following extra alternatives:

        #{listToText (Map.keys extraA)}

        … while the latter union has the following extra alternatives:

        #{listToText (Map.keys extraB)}
        |]

-- Helper functions for displaying errors

insert :: Pretty a => a -> Text
insert a = prettyToText ("  " <> Pretty.align (pretty a))

listToText :: Pretty a => [a] -> Text
listToText elements = Text.intercalate "\n" (map prettyEntry elements)
  where
    prettyEntry entry = prettyToText ("• " <> Pretty.align (pretty entry))

prettyToText :: Pretty a => a -> Text
prettyToText = Grace.Pretty.renderStrict True Grace.Pretty.defaultColumns
