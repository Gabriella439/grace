{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

{-| This module is based on the bidirectional type-checking algorithm from:

    Dunfield, Jana, and Neelakantan R. Krishnaswami. \"Complete and easy bidirectional typechecking for higher-rank polymorphism.\" ACM SIGPLAN Notices 48.9 (2013): 429-442.

    The main differences from the original algorithm are:

    * This uses `Control.Monad.State.Strict.StateT` to thread around
      `Context`s and manipulate them instead of explicit `Context` passing as
      in the original paper

    * This algorithm adds support for existential quantification

    * This algorithm adds support for row polymorphic and polymorphic variants
-}
module Grace.Infer
    ( -- * Type inference
      typeOf
    , typeWith
      -- * Errors related to type inference
    , TypeInferenceError(..)
    ) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (Exception(..))
import Control.Monad (when)
import Control.Monad.Except (MonadError(..))
import Control.Monad.State.Strict (MonadState)
import Data.Foldable (traverse_)
import Data.Sequence (ViewL(..))
import Data.Text (Text)
import Data.Void (Void)
import Grace.Context (Context, Entry)
import Grace.Existential (Existential)
import Grace.Location (Location(..))
import Grace.Monotype (Monotype)
import Grace.Pretty (Pretty(..))
import Grace.Syntax (Syntax)
import Grace.Type (Type(..))
import Grace.Value (Value)

import qualified Control.Monad as Monad
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Grace.Context as Context
import qualified Grace.Domain as Domain
import qualified Grace.Location as Location
import qualified Grace.Monotype as Monotype
import qualified Grace.Pretty
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type
import qualified Grace.Width as Width
import qualified Prettyprinter as Pretty

-- | Type-checking state
data Status = Status
    { count :: !Int
      -- ^ Used to generate fresh unsolved variables (e.g. α̂, β̂ from the
      --   original paper)
    , context :: Context Location
      -- ^ The type-checking context (e.g. Γ, Δ, Θ)
    }

orDie :: MonadError e m => Maybe a -> e -> m a
Just x  `orDie` _ = return x
Nothing `orDie` e = throwError e

-- | Generate a fresh existential variable (of any type)
fresh :: MonadState Status m => m (Existential a)
fresh = do
    Status{ count = n, .. } <- State.get

    State.put $! Status{ count = n + 1, .. }

    return (fromIntegral n)

-- Unlike the original paper, we don't explicitly thread the `Context` around.
-- Instead, we modify the ambient state using the following utility functions:

-- | Push a new `Context` `Entry` onto the stack
push :: MonadState Status m => Entry Location -> m ()
push entry = State.modify (\s -> s { context = entry : context s })

-- | Retrieve the current `Context`
get :: MonadState Status m => m (Context Location)
get = State.gets context

-- | Set the `Context` to a new value
set :: MonadState Status m => Context Location -> m ()
set context = State.modify (\s -> s{ context })

{-| This is used to temporarily add a `Context` entry that is discarded at the
    end of the entry's scope, along with any downstream entries that were
    created within that same scope
-}
scoped :: MonadState Status m => Entry Location -> m r -> m r
scoped entry k = do
    push entry

    r <- k

    State.modify (\s -> s{ context = Context.discardUpTo entry (context s) })

    return r

scopedUnsolvedType :: MonadState Status m => s -> (Type.Type s -> m a) -> m a
scopedUnsolvedType location k = do
    existential <- fresh

    scoped (Context.MarkerType existential) do
        push (Context.UnsolvedType existential)

        k Type.UnsolvedType{..}

scopedUnsolvedFields :: MonadState Status m => (Type.Record s -> m a) -> m a
scopedUnsolvedFields k = do
    a <- fresh

    scoped (Context.MarkerFields a) do
        push (Context.UnsolvedFields a)

        k (Type.Fields [] (Monotype.UnsolvedFields a))

scopedUnsolvedAlternatives
    :: MonadState Status m => (Type.Union s -> m a) -> m a
scopedUnsolvedAlternatives k = do
    a <- fresh

    scoped (Context.MarkerAlternatives a) do
        push (Context.UnsolvedAlternatives a)

        k (Type.Alternatives [] (Monotype.UnsolvedAlternatives a))

{-| This corresponds to the judgment:

    > Γ ⊢ A

    … which checks that under context Γ, the type A is well-formed
-}
wellFormedType
    :: MonadError TypeInferenceError m
    => Context Location -> Type Location -> m ()
wellFormedType _Γ type0 =
    case type0 of
        -- UvarWF
        Type.VariableType{..}
            | Context.Variable Domain.Type name `elem` _Γ -> do
                return ()
            | otherwise -> throwError (UnboundTypeVariable location name)

        -- ArrowWF
        Type.Function{..} -> do
            wellFormedType _Γ input
            wellFormedType _Γ output

        -- ForallWF
        Type.Forall{..} -> do
            wellFormedType (Context.Variable domain name : _Γ) type_

        -- ForallWF
        Type.Exists{..} -> do
            wellFormedType (Context.Variable domain name : _Γ) type_

        -- EvarWF / SolvedEvarWF
        _A@Type.UnsolvedType{..}
            | any predicate _Γ -> do
                return ()
            | otherwise -> do
                throwError (IllFormedType location _A _Γ)
          where
            predicate (Context.UnsolvedType a  ) = existential == a
            predicate (Context.SolvedType   a _) = existential == a
            predicate  _                         = False

        Type.Optional{..} -> do
            wellFormedType _Γ type_

        Type.List{..} -> do
            wellFormedType _Γ type_

        Type.Record{ fields = Type.Fields kAs Monotype.EmptyFields } -> do
            traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs

        Type.Record{ fields = Type.Fields kAs (Monotype.UnsolvedFields a0), .. }
            | any predicate _Γ -> do
                traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
            | otherwise -> do
                throwError (IllFormedFields location a0 _Γ)
          where
            predicate (Context.UnsolvedFields a1  ) = a0 == a1
            predicate (Context.SolvedFields   a1 _) = a0 == a1
            predicate  _                            = False

        Type.Record{ fields = Type.Fields kAs (Monotype.VariableFields a), .. }
            | Context.Variable Domain.Fields a `elem` _Γ -> do
                traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
            | otherwise -> do
                throwError (UnboundFields location a)

        Type.Union{ alternatives = Type.Alternatives kAs Monotype.EmptyAlternatives } -> do
            traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs

        Type.Union{ alternatives = Type.Alternatives kAs (Monotype.UnsolvedAlternatives a0), .. }
            | any predicate _Γ -> do
                traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
            | otherwise -> do
                throwError (IllFormedAlternatives location a0 _Γ)
          where
            predicate (Context.UnsolvedAlternatives a1  ) = a0 == a1
            predicate (Context.SolvedAlternatives   a1 _) = a0 == a1
            predicate  _                                  = False

        Type.Union{ alternatives = Type.Alternatives kAs (Monotype.VariableAlternatives a), .. }
            | Context.Variable Domain.Alternatives a `elem` _Γ -> do
                traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
            | otherwise -> do
                throwError (UnboundAlternatives location a)

        Type.Scalar{} -> do
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

    case (_A0, _B0) of
        -- <:Var
        (Type.VariableType{ name = a0 }, Type.VariableType{ name = a1 })
            | a0 == a1 -> do
                wellFormedType _Γ _A0

        -- <:Exvar
        (Type.UnsolvedType{ existential = a0 }, Type.UnsolvedType{ existential = a1 })
            | a0 == a1 && Context.UnsolvedType a0 `elem` _Γ -> do
                return ()

        -- InstantiateL
        (Type.UnsolvedType{ existential = a }, _)
            -- The `not (a `Type.typeFreeIn` _B)` is the "occurs check" which
            -- prevents a type variable from being defined in terms of itself
            -- (i.e. a type should not "occur" within itself).
            --
            -- Later on you'll see matching "occurs checks" for record types and
            -- union types so that Fields variables and Alternatives variables
            -- cannot refer to the record or union that they belong to,
            -- respectively.
            |   not (a `Type.typeFreeIn` _B0)
            &&  elem (Context.UnsolvedType a) _Γ -> do
                instantiateTypeL a _B0

        -- InstantiateR
        (_, Type.UnsolvedType{ existential = a})
            |   not (a `Type.typeFreeIn` _A0)
            &&  elem (Context.UnsolvedType a) _Γ -> do
                instantiateTypeR _A0 a

        -- <:→
        (Type.Function{ input = _A1, output = _A2 }, Type.Function{ input= _B1, output = _B2 }) -> do
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

        -- <:∃L
        (Type.Exists{..}, _) -> do
            scoped (Context.Variable domain name) do
                subtype type_ _B0

        -- <:∀R
        (_, Type.Forall{..}) -> do
            scoped (Context.Variable domain name) do
                subtype _A0 type_

        -- <:∃R
        (_, Type.Exists{ domain = Domain.Type, .. }) -> do
            scopedUnsolvedType nameLocation \a ->
                subtype _A0 (Type.substituteType name a type_)

        (_, Type.Exists{ domain = Domain.Fields, .. }) -> do
            scopedUnsolvedFields \a -> do
                subtype _A0 (Type.substituteFields name a type_)

        (_, Type.Exists{ domain = Domain.Alternatives, .. }) -> do
            scopedUnsolvedAlternatives \a -> do
                subtype _A0 (Type.substituteAlternatives name a type_)

        -- <:∀L
        (Type.Forall{ domain = Domain.Type, .. }, _) -> do
            scopedUnsolvedType nameLocation \a -> do
                subtype (Type.substituteType name a type_) _B0

        (Type.Forall{ domain = Domain.Fields, .. }, _) -> do
            scopedUnsolvedFields \a -> do
                subtype (Type.substituteFields name a type_) _B0

        (Type.Forall{ domain = Domain.Alternatives, .. }, _) -> do
            scopedUnsolvedAlternatives \a -> do
                subtype (Type.substituteAlternatives name a type_) _B0

        (Type.Scalar{ scalar = s0 }, Type.Scalar{ scalar = s1 })
            | s0 == s1 -> do
                return ()

        (Type.Optional{ type_ = _A }, Type.Optional{ type_ = _B }) -> do
            subtype _A _B

        (Type.List{ type_ = _A }, Type.List{ type_ = _B }) -> do
            subtype _A _B

        -- This is where you need to add any non-trivial subtypes.  For example,
        -- the following three rules specify that `Natural` is a subtype of
        -- `Integer`, which is in turn a subtype of `Real`.
        (Type.Scalar{ scalar = Monotype.Natural }, Type.Scalar{ scalar = Monotype.Integer }) -> do
            return ()

        (Type.Scalar{ scalar = Monotype.Natural }, Type.Scalar{ scalar = Monotype.Real }) -> do
            return ()

        (Type.Scalar{ scalar = Monotype.Integer }, Type.Scalar{ scalar = Monotype.Real }) -> do
            return ()

        -- Similarly, this is the rule that says that `T` is a subtype of
        -- `Optional T`.  If that feels unprincipled to you then delete this
        -- rule.
        (_, Type.Optional{..}) -> do
            subtype _A0 type_

        (Type.Scalar{ }, Type.Scalar{ scalar = Monotype.JSON }) -> do
            return ()

        (Type.List{ type_ = _A }, Type.Scalar{ scalar = Monotype.JSON }) -> do
            subtype _A _B0
            return ()

        (Type.Record{ fields = Type.Fields kAs Monotype.EmptyFields }, Type.Scalar{ scalar = Monotype.JSON }) -> do
            let process (_, _A) = do
                    _Γ <- get

                    subtype _A (Context.solveType _Γ _B0)

            traverse_ process kAs

        -- The type-checking code for records is the first place where we
        -- implement a non-trivial type that wasn't already covered by the
        -- paper, so we'll go into more detail here to explain the general
        -- type-checking principles of the paper.
        (_A@Type.Record{ fields = Type.Fields kAs0 fields0 }, _B@Type.Record{ fields = Type.Fields kBs0 fields1 }) -> do
            let mapA = Map.fromList kAs0
            let mapB = Map.fromList kBs0

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            let flexible  Monotype.EmptyFields          = False
                flexible (Monotype.VariableFields _   ) = False
                flexible (Monotype.UnsolvedFields _   ) = True

            let okayA = Map.null extraA
                    || (flexible fields1 && fields0 /= fields1)

            let okayB = Map.null extraB
                    || (flexible fields0 && fields0 /= fields1)

            -- First we check that there are no mismatches in the record types
            -- that cannot be resolved by just setting an unsolved Fields
            -- variable to the right type.
            --
            -- For example, `{ x: Bool }` can never be a subtype of
            -- `{ y: Text }`
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
        (_A@Type.Union{ alternatives = Type.Alternatives kAs0 alternatives0 }, _B@Type.Union{ alternatives = Type.Alternatives kBs0 alternatives1 }) -> do
            let mapA = Map.fromList kAs0
            let mapB = Map.fromList kBs0

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            let flexible  Monotype.EmptyAlternatives          = False
                flexible (Monotype.VariableAlternatives _   ) = False
                flexible (Monotype.UnsolvedAlternatives _   ) = True

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
                _ | null extraA && null extraB && alternatives0 == alternatives1 -> do
                        return ()

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

    case _A0 of
        -- InstLReach
        Type.UnsolvedType{..}
            | let _ΓL = _Γ
            , Just (_ΓR, _ΓM) <- Context.splitOnUnsolvedType existential _Γ' -> do
                set (_ΓR <> (Context.SolvedType existential (Monotype.UnsolvedType a) : _ΓM) <> (Context.UnsolvedType a : _ΓL))

        -- InstLSolve
        Type.UnsolvedType{..} -> do
            instLSolve (Monotype.UnsolvedType existential)
        Type.VariableType{..} -> do
            instLSolve (Monotype.VariableType name)
        Type.Scalar{..} -> do
            instLSolve (Monotype.Scalar scalar)

        -- InstLExt
        Type.Exists{ domain = Domain.Type, .. } -> do
            scopedUnsolvedType nameLocation \b -> do
                instantiateTypeR (Type.substituteType name b type_) a
        Type.Exists{ domain = Domain.Fields, .. } -> do
            scopedUnsolvedFields \b -> do
                instantiateTypeR (Type.substituteFields name b type_) a
        Type.Exists{ domain = Domain.Alternatives, .. } -> do
            scopedUnsolvedAlternatives \b -> do
                instantiateTypeR (Type.substituteAlternatives name b type_) a

        -- InstLArr
        Type.Function{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            a1 <- fresh
            a2 <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Function (Monotype.UnsolvedType a1) (Monotype.UnsolvedType a2)) : Context.UnsolvedType a1 : Context.UnsolvedType a2 : _ΓL))

            instantiateTypeR input a1

            _Θ <- get

            instantiateTypeL a2 (Context.solveType _Θ output)

        -- InstLAllR
        Type.Forall{..} -> do
            scoped (Context.Variable domain name) do
                instantiateTypeL a type_

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
        Type.Optional{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            -- To solve `a` against `Optional _A` we create a fresh unsolved
            -- variable named `a1`, …
            a1 <- fresh

            -- … solve `a` to `Optional a1`, taking care that `a1` comes before
            -- `a` within the context, (since `a` refers to `a1`)  …
            set (_ΓR <> (Context.SolvedType a (Monotype.Optional (Monotype.UnsolvedType a1)) : Context.UnsolvedType a1 : _ΓL))

            -- … and then solve `a1` against _A`
            instantiateTypeL a1 type_

        -- We solve an unsolved variable against `List` using the same
        -- principles described above for solving `Optional`
        Type.List{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            a1 <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.List (Monotype.UnsolvedType a1)) : Context.UnsolvedType a1 : _ΓL))

            instantiateTypeL a1 type_

        -- This is still the same one-layer-at-a-time principle, with a small
        -- twist.  In order to solve:
        --
        --     a = { r }
        --
        -- We replace `r` with a new unsolved Fields variable and then solve for
        -- that Fields variable.
        Type.Record{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            p <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Record (Monotype.Fields [] (Monotype.UnsolvedFields p))) : Context.UnsolvedFields p : _ΓL))

            instantiateFieldsL p (Type.location _A0) fields

        -- Same principle as for `Record`, but replacing the Field variable with
        -- an Alternatives variable
        Type.Union{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            p <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Union (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives p))) : Context.UnsolvedAlternatives p : _ΓL))

            instantiateAlternativesL p (Type.location _A0) alternatives

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

    case _A0 of
        -- InstRReach
        Type.UnsolvedType{..}
            | let _ΓL = _Γ
            , Just (_ΓR, _ΓM) <- Context.splitOnUnsolvedType existential _Γ' -> do
                set (_ΓR <> (Context.SolvedType existential (Monotype.UnsolvedType a) : _ΓM) <> (Context.UnsolvedType a : _ΓL))

        -- InstRSolve
        Type.UnsolvedType{..} -> do
            instRSolve (Monotype.UnsolvedType existential)
        Type.VariableType{..} -> do
            instRSolve (Monotype.VariableType name)
        Type.Scalar{..} -> do
            instRSolve (Monotype.Scalar scalar)

        -- InstRArr
        Type.Function{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            a1 <- fresh
            a2 <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Function (Monotype.UnsolvedType a1) (Monotype.UnsolvedType a2)) : Context.UnsolvedType a1 : Context.UnsolvedType a2 : _ΓL))

            instantiateTypeL a1 input

            _Θ <- get

            instantiateTypeR (Context.solveType _Θ output) a2

        -- InstRExtL
        Type.Exists{..} -> do
            scoped (Context.Variable domain name) do
                instantiateTypeL a type_

        -- InstRAllL
        Type.Forall{ domain = Domain.Type, .. } -> do
            scopedUnsolvedType nameLocation \b -> do
                instantiateTypeR (Type.substituteType name b type_) a
        Type.Forall{ domain = Domain.Fields, .. } -> do
            scopedUnsolvedFields \b -> do
                instantiateTypeR (Type.substituteFields name b type_) a
        Type.Forall{ domain = Domain.Alternatives, .. } -> do
            scopedUnsolvedAlternatives \b -> do
                instantiateTypeR (Type.substituteAlternatives name b type_) a

        Type.Optional{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            a1 <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Optional (Monotype.UnsolvedType a1)) : Context.UnsolvedType a1 : _ΓL))

            instantiateTypeR type_ a1

        Type.List{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            a1 <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.List (Monotype.UnsolvedType a1)) : Context.UnsolvedType a1 : _ΓL))

            instantiateTypeR type_ a1

        Type.Record{..}  -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            p <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Record (Monotype.Fields [] (Monotype.UnsolvedFields p))) : Context.UnsolvedFields p : _ΓL))

            instantiateFieldsR (Type.location _A0) fields p

        Type.Union{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            p <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Union (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives p))) : Context.UnsolvedAlternatives p : _ΓL))

            instantiateAlternativesR (Type.location _A0) alternatives p

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
    => Existential Monotype.Record
    -> Location
    -> Type.Record Location
    -> m ()
instantiateFieldsL p0 location fields@(Type.Fields kAs rest) = do
    when (p0 `Type.fieldsFreeIn` Type.Record{..}) do
        throwError (NotFieldsSubtype location p0 fields)

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
                Type.Record{ fields = Type.Fields [] rest, .. }

            set (_ΓR <> (Context.SolvedFields p0 (Monotype.Fields kbs rest) : bs <> _ΓL))

    let instantiate (_, _A, b) = do
            _Θ <- get

            instantiateTypeL b (Context.solveType _Θ _A)

    traverse_ instantiate kAbs

instantiateFieldsR
    :: (MonadState Status m, MonadError TypeInferenceError m)
    => Location
    -> Type.Record Location
    -> Existential Monotype.Record
    -> m ()
instantiateFieldsR location fields@(Type.Fields kAs rest) p0 = do
    when (p0 `Type.fieldsFreeIn` Type.Record{..}) do
        throwError (NotFieldsSubtype location p0 fields)

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
                Type.Record{ fields = Type.Fields [] rest, .. }

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
    => Existential Monotype.Union
    -> Location
    -> Type.Union Location
    -> m ()
instantiateAlternativesL p0 location alternatives@(Type.Alternatives kAs rest) = do
    when (p0 `Type.alternativesFreeIn` Type.Union{..}) do
        throwError (NotAlternativesSubtype location p0 alternatives)

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
                Type.Union{ alternatives = Type.Alternatives [] rest, .. }

            set (_ΓR <> (Context.SolvedAlternatives p0 (Monotype.Alternatives kbs rest) : bs <> _ΓL))

    let instantiate (_, _A, b) = do
            _Θ <- get

            instantiateTypeL b (Context.solveType _Θ _A)

    traverse_ instantiate kAbs

instantiateAlternativesR
    :: (MonadState Status m, MonadError TypeInferenceError m)
    => Location
    -> Type.Union Location
    -> Existential Monotype.Union
    -> m ()
instantiateAlternativesR location alternatives@(Type.Alternatives kAs rest) p0 = do
    when (p0 `Type.alternativesFreeIn` Type.Union{..}) do
        throwError (NotAlternativesSubtype location p0 alternatives)

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
                Type.Union{ alternatives = Type.Alternatives [] rest, .. }

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
    => Syntax Location (Type Location, Value) -> m (Type Location)
infer e0 = do
    let input ~> output = Type.Function{ location = Syntax.location e0, ..}

    let var name = Type.VariableType{ location = Syntax.location e0, name, .. }

    case e0 of
        -- Var
        Syntax.Variable{..} -> do
            _Γ <- get

            Context.lookup name index _Γ `orDie` UnboundVariable location name index

        -- →I⇒
        Syntax.Lambda{..} -> do
            a <- fresh
            b <- fresh

            let input = Type.UnsolvedType{ location = nameLocation, existential = a }

            let output = Type.UnsolvedType{ existential = b, .. }

            push (Context.UnsolvedType a)
            push (Context.UnsolvedType b)

            scoped (Context.Annotation name input) do
                check body output

            return Type.Function{..}

        -- →E
        Syntax.Application{..} -> do
            _A <- infer function

            _Θ <- get

            inferApplication (Context.solveType _Θ _A) argument

        -- Anno
        Syntax.Annotation{..} -> do
            _Γ <- get

            wellFormedType _Γ annotation

            check annotated annotation

            return annotation

        Syntax.Let{..} -> do
            let process Syntax.Binding{ annotation = Nothing, .. } = do
                    _A <- infer assignment

                    push (Context.Annotation name _A)
                process Syntax.Binding{ annotation = Just _A, .. } = do
                    check assignment _A

                    push (Context.Annotation name _A)

            traverse_ process bindings

            infer body

        Syntax.List{..} -> do
            case Seq.viewl elements of
                EmptyL -> do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    return Type.List{ type_ = Type.UnsolvedType{..}, .. }
                y :< ys -> do
                    type_ <- infer y

                    let process element = do
                            _Γ <- get

                            check element (Context.solveType _Γ type_)

                    traverse_ process ys

                    return Type.List{..}

        Syntax.Record{..} -> do
            let process (field, value) = do
                    type_ <- infer value

                    return (field, type_)

            fieldTypes <- traverse process fieldValues

            return Type.Record{ fields = Type.Fields fieldTypes Monotype.EmptyFields, .. }

        Syntax.Alternative{..} -> do
            existential <- fresh
            p           <- fresh

            push (Context.UnsolvedType existential)
            push (Context.UnsolvedAlternatives p)

            return
                (   Type.UnsolvedType{..}
                ~>  Type.Union
                        { alternatives = Type.Alternatives
                            [( name, Type.UnsolvedType{..})]
                            (Monotype.UnsolvedAlternatives p)
                        , ..
                        }
                )

        Syntax.Merge{..} -> do
            p <- fresh

            push (Context.UnsolvedFields p)

            let _R = Type.Record{ location = Syntax.location handlers , fields = Type.Fields [] (Monotype.UnsolvedFields p) }

            check handlers _R

            _Γ <- get

            let _R' = Context.solveType _Γ _R

            case _R' of
                Type.Record{ fields = Type.Fields keyTypes Monotype.EmptyFields } -> do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    let process (key, Type.Function{ location = _, ..}) = do
                            _ϴ <- get

                            let b' = Type.UnsolvedType{ location = Type.location output, .. }

                            subtype (Context.solveType _ϴ output) (Context.solveType _ϴ b')

                            return (key, input)
                        process (_, _A) = do
                            throwError (MergeInvalidHandler (Type.location _A) _A)

                    keyTypes' <- traverse process keyTypes

                    return
                        (   Type.Union
                                { alternatives =
                                    Type.Alternatives
                                        keyTypes'
                                        Monotype.EmptyAlternatives
                                , ..
                                }
                        ~>      Type.UnsolvedType{..}
                        )

                Type.Record{} -> do
                    throwError (MergeConcreteRecord (Type.location _R) _R)

                _ -> do
                    throwError (MergeRecord (Type.location _R) _R)

        Syntax.Field{..} -> do
            existential <- fresh
            p <- fresh

            push (Context.UnsolvedType existential)
            push (Context.UnsolvedFields p)

            check record Type.Record
                { fields =
                    Type.Fields
                        [(field, Type.UnsolvedType{..})]
                        (Monotype.UnsolvedFields p)
                , location = fieldLocation
                }

            return Type.UnsolvedType{ location = fieldLocation, ..}

        Syntax.If{..} -> do
            check predicate Type.Scalar{ scalar = Monotype.Bool, .. }

            _L0 <- infer ifTrue

            _Γ  <- get

            let _L1 = Context.solveType _Γ _L0

            check ifFalse _L1

            return _L1

        -- All the type inference rules for scalars go here.  This part is
        -- pretty self-explanatory: a scalar literal returns the matching
        -- scalar type.
        Syntax.Scalar{ scalar = Syntax.Bool _, .. } -> do
            return Type.Scalar{ scalar = Monotype.Bool, .. }

        Syntax.Scalar{ scalar = Syntax.Real _, .. } -> do
            return Type.Scalar{ scalar = Monotype.Real, .. }

        Syntax.Scalar{ scalar = Syntax.Integer _, .. } -> do
            return Type.Scalar{ scalar = Monotype.Integer, .. }

        Syntax.Scalar{ scalar = Syntax.Natural _, .. } -> do
            return Type.Scalar{ scalar = Monotype.Natural, .. }

        Syntax.Scalar{ scalar = Syntax.Text _, .. } -> do
            return Type.Scalar{ scalar = Monotype.Text, .. }

        Syntax.Scalar{ scalar = Syntax.Null, .. } -> do
            -- NOTE: You might think that you could just infer that `null`
            -- has type `forall (a : Type) . Optional a`.  This does not work
            -- because it will lead to data structures with impredicative types
            -- if you store a `null` inside of, say, a `List`.
            existential <- fresh

            push (Context.UnsolvedType existential)

            return Type.Optional{ type_ = Type.UnsolvedType{..}, .. }

        Syntax.Operator{ operator = Syntax.And, .. } -> do
            check left  Type.Scalar{ scalar = Monotype.Bool, location = operatorLocation }
            check right Type.Scalar{ scalar = Monotype.Bool, location = operatorLocation }

            return Type.Scalar{ scalar = Monotype.Bool, location = operatorLocation }

        Syntax.Operator{ operator = Syntax.Or, .. } -> do
            check left  Type.Scalar{ scalar = Monotype.Bool, location = operatorLocation }
            check right Type.Scalar{ scalar = Monotype.Bool, location = operatorLocation }

            return Type.Scalar{ scalar = Monotype.Bool, location = operatorLocation }

        Syntax.Operator{ operator = Syntax.Times, .. } -> do
            _L <- infer left
            _R <- infer right

            check left  _R
            check right _L

            _Γ <- get

            let _L' = Context.solveType _Γ _L

            case _L' of
                Type.Scalar{ scalar = Monotype.Natural } -> return _L
                Type.Scalar{ scalar = Monotype.Integer } -> return _L
                Type.Scalar{ scalar = Monotype.Real    } -> return _L
                _ -> do
                    throwError (InvalidOperands (Syntax.location left) _L')

        Syntax.Operator{ operator = Syntax.Plus, .. } -> do
            _L <- infer left
            _R <- infer right

            check left  _R
            check right _L

            _Γ <- get

            let _L' = Context.solveType _Γ _L

            case _L' of
                Type.Scalar{ scalar = Monotype.Natural } -> return _L
                Type.Scalar{ scalar = Monotype.Integer } -> return _L
                Type.Scalar{ scalar = Monotype.Real    } -> return _L
                Type.Scalar{ scalar = Monotype.Text    } -> return _L
                Type.List{}                              -> return _L

                _ -> do
                    throwError (InvalidOperands (Syntax.location left) _L')

        Syntax.Builtin{ builtin = Syntax.RealEqual, .. }-> do
            return
                (   Type.Scalar{ scalar = Monotype.Real, .. }
                ~>  (   Type.Scalar{ scalar = Monotype.Real, .. }
                    ~>  Type.Scalar{ scalar = Monotype.Bool, .. }
                    )
                )

        Syntax.Builtin{ builtin = Syntax.RealLessThan, .. } -> do
            return
                (   Type.Scalar{ scalar = Monotype.Real, .. }
                ~>  (   Type.Scalar{ scalar = Monotype.Real, .. }
                    ~>  Type.Scalar{ scalar = Monotype.Bool, .. }
                    )
                )

        Syntax.Builtin{ builtin = Syntax.RealNegate, .. } -> do
            return
                (   Type.Scalar{ scalar = Monotype.Real, .. }
                ~>  Type.Scalar{ scalar = Monotype.Real, .. }
                )

        Syntax.Builtin{ builtin = Syntax.RealShow, .. } -> do
            return
                (   Type.Scalar{ scalar = Monotype.Real, .. }
                ~>  Type.Scalar{ scalar = Monotype.Text, .. }
                )

        Syntax.Builtin{ builtin = Syntax.ListDrop, .. } -> do
            return Type.Forall
                { nameLocation = Syntax.location e0
                , name = "a"
                , domain = Domain.Type
                , type_ =
                       Type.Scalar{ scalar = Monotype.Natural, .. }
                    ~>  (   Type.List{ type_ = var "a", .. }
                        ~>  Type.List{ type_ = var "a", .. }
                        )
                , ..
                }

        Syntax.Builtin{ builtin = Syntax.ListHead, .. } -> do
            return Type.Forall
                { nameLocation = Syntax.location e0
                , name = "a"
                , domain = Domain.Type
                , type_ =
                    Type.Forall
                        { nameLocation = Syntax.location e0
                        , name = "b"
                        , domain = Domain.Alternatives
                        , type_ =
                                Type.List { type_ = var "a", .. }
                            ~>  Type.Union
                                    { alternatives =
                                        Type.Alternatives
                                            [ ("Some", var "a" )
                                            , ("None", Type.Record{ fields = Type.Fields [] Monotype.EmptyFields, .. })
                                            ]
                                            (Monotype.VariableAlternatives "b")
                                    , ..
                                    }
                        , ..
                        }
                , ..
                }

        Syntax.Builtin{ builtin = Syntax.ListEqual, .. } -> do
            return Type.Forall
                { nameLocation = Syntax.location e0
                , name = "a"
                , domain = Domain.Type
                , type_ =
                        (   var "a"
                        ~>  (var "a" ~> Type.Scalar{ scalar = Monotype.Bool, .. })
                        )
                    ~>  (   Type.List{ type_ = var "a", .. }
                        ~>  (   Type.List{ type_ = var "a", .. }
                            ~>  Type.Scalar{ scalar = Monotype.Bool, .. }
                            )
                        )
                , ..
                }

        Syntax.Builtin{ builtin = Syntax.ListFold, .. } -> do
            return Type.Forall
                { nameLocation = Syntax.location e0
                , name = "a"
                , domain = Domain.Type
                , type_ = Type.Forall
                    { nameLocation = Syntax.location e0
                    , name = "b"
                    , domain = Domain.Type
                    , type_ =
                            Type.Record
                                { fields =
                                    Type.Fields
                                        [ ("cons", var "a" ~> (var "b" ~> var "b"))
                                        , ("nil", var "b")
                                        ]
                                        Monotype.EmptyFields
                                , ..
                                }
                        ~>  (Type.List{ type_= var "a", .. } ~> var "b")
                    , ..
                    }
                , ..
                }


        Syntax.Builtin{ builtin = Syntax.ListIndexed, .. } -> do
            return Type.Forall
                { nameLocation = Syntax.location e0
                , name = "a"
                , domain = Domain.Type
                , type_ =
                        Type.List{ type_ = var "a", .. }
                    ~>  Type.List
                            { type_ =
                                Type.Record
                                    { fields =
                                        Type.Fields
                                            [ ("index", Type.Scalar{ scalar = Monotype.Natural, .. })
                                            , ("value", var "a")
                                            ]
                                            Monotype.EmptyFields
                                    , ..
                                    }
                            , ..
                            }
                , ..
                }

        Syntax.Builtin{ builtin = Syntax.ListLast, .. } -> do
            return Type.Forall
                { nameLocation = Syntax.location e0
                , name = "a"
                , domain = Domain.Type
                , type_ = Type.Forall
                    { nameLocation = Syntax.location e0
                    , name = "b"
                    , domain  = Domain.Alternatives
                    , type_ =
                            Type.List{ type_ = var "a", .. }
                        ~>  Type.Union
                                { alternatives =
                                    Type.Alternatives
                                        [ ("Some", var "a")
                                        , ("None", Type.Record{ fields = Type.Fields [] Monotype.EmptyFields, .. })
                                        ]
                                        (Monotype.VariableAlternatives "b")
                                , ..
                                }
                    , ..
                    }
                , ..
                }

        Syntax.Builtin{ builtin = Syntax.ListLength, .. } -> do
            return Type.Forall
                { nameLocation = Syntax.location e0
                , name = "a"
                , domain = Domain.Type
                , type_ =
                        Type.List{ type_ = var "a", .. }
                    ~>  Type.Scalar{ scalar = Monotype.Natural, .. }
                , ..
                }

        Syntax.Builtin{ builtin = Syntax.ListMap, .. } -> do
            return Type.Forall
                { nameLocation = Syntax.location e0
                , name = "a"
                , domain = Domain.Type
                , type_ = Type.Forall
                    { nameLocation = Syntax.location e0
                    , name = "b"
                    , domain = Domain.Type
                    , type_ =
                            (var "a" ~> var "b")
                        ~>  (   Type.List{ type_ = var "a", .. }
                            ~>  Type.List{ type_ = var "b", .. }
                            )
                    , ..
                    }
                , ..
                }

        Syntax.Builtin{ builtin = Syntax.IntegerAbs, .. } -> do
            return
                (   Type.Scalar{ scalar = Monotype.Integer, .. }
                ~>  Type.Scalar{ scalar = Monotype.Natural, .. }
                )

        Syntax.Builtin{ builtin = Syntax.IntegerEven, .. } -> do
            return
                (   Type.Scalar{ scalar = Monotype.Integer, .. }
                ~>  Type.Scalar{ scalar = Monotype.Bool, .. }
                )

        Syntax.Builtin{ builtin = Syntax.IntegerNegate, .. } -> do
            return
                (   Type.Scalar{ scalar = Monotype.Integer, .. }
                ~>  Type.Scalar{ scalar = Monotype.Integer, .. }
                )

        Syntax.Builtin{ builtin = Syntax.IntegerOdd, .. } -> do
            return
                (   Type.Scalar{ scalar = Monotype.Integer, .. }
                ~>  Type.Scalar{ scalar = Monotype.Bool, .. }
                )

        Syntax.Builtin{ builtin = Syntax.ListReverse, .. } -> do
            return Type.Forall
                { nameLocation = Syntax.location e0
                , name = "a"
                , domain = Domain.Type
                , type_ = Type.List{ type_ = var "a", .. } ~> Type.List{ type_ = var "a", .. }
                , ..
                }

        Syntax.Builtin{ builtin = Syntax.ListTake, .. } -> do
            return Type.Forall
                { nameLocation = Syntax.location e0
                , name = "a"
                , domain = Domain.Type
                , type_ =
                        Type.Scalar{ scalar = Monotype.Natural, .. }
                    ~>  (   Type.List{ type_ = var "a", .. }
                        ~>  Type.List{ type_ = var "a", .. }
                        )
                , ..
                }

        Syntax.Builtin{ builtin = Syntax.JSONFold, .. } -> do
            return Type.Forall
                { nameLocation = Syntax.location e0
                , name = "a"
                , domain = Domain.Type
                , type_ =
                        Type.Record
                            { fields = Type.Fields
                                [ ( "array", Type.List{ type_ = var "a", .. } ~> var "a")
                                , ("bool", Type.Scalar{ scalar = Monotype.Bool, .. } ~> var "a")
                                , ("real", Type.Scalar{ scalar = Monotype.Real, ..  } ~> var "a")
                                , ("integer", Type.Scalar{ scalar = Monotype.Integer, .. } ~> var "a")
                                , ("natural", Type.Scalar{ scalar = Monotype.Natural, .. } ~> var "a")
                                , ("null", var "a")
                                , ( "object"
                                  ,     Type.List
                                            { type_ = Type.Record
                                                { fields =
                                                    Type.Fields
                                                        [ ("key", Type.Scalar{ scalar = Monotype.Text, .. })
                                                        , ("value", var "a")
                                                        ]
                                                        Monotype.EmptyFields
                                                , ..
                                                }
                                            , ..
                                            }
                                    ~>  var "a"
                                  )
                                , ("string", Type.Scalar{ scalar = Monotype.Text, .. } ~> var "a")
                                ]
                                Monotype.EmptyFields
                            , ..
                            }
                    ~>  (   Type.Scalar{ scalar = Monotype.JSON, .. }
                        ~>  var "a"
                        )
                , ..
                }

        Syntax.Builtin{ builtin = Syntax.NaturalFold, .. } -> do
            return Type.Forall
                { nameLocation = Syntax.location e0
                , name = "a"
                , domain = Domain.Type
                , type_ =
                        Type.Scalar{ scalar = Monotype.Natural, .. }
                    ~>  ((var "a" ~> var "a") ~> (var "a" ~> var "a"))
                , ..
                }

        Syntax.Builtin{ builtin = Syntax.TextEqual, .. } -> do
            return
                (   Type.Scalar{ scalar = Monotype.Text, .. }
                ~>  (   Type.Scalar{ scalar = Monotype.Text, .. }
                    ~>  Type.Scalar{ scalar = Monotype.Bool, .. }
                    )
                )

        Syntax.Embed{ embedded = (type_, _) } -> do
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

-- →I
check Syntax.Lambda{ location = _, ..} Type.Function{..} = do
    scoped (Context.Annotation name input) do
        check body output

-- ∃I
check e Type.Exists{ domain = Domain.Type, .. } = do
    scopedUnsolvedType nameLocation \a -> do
        check e (Type.substituteType name a type_)
check e Type.Exists{ domain = Domain.Fields, .. } = do
    scopedUnsolvedFields \a -> do
        check e (Type.substituteFields name a type_)
check e Type.Exists{ domain = Domain.Alternatives, .. } = do
    scopedUnsolvedAlternatives \a -> do
        check e (Type.substituteAlternatives name a type_)

-- ∀I
check e Type.Forall{..} = do
    scoped (Context.Variable domain name) do
        check e type_

check Syntax.Operator{ operator = Syntax.Times, .. } _B@Type.Scalar{ scalar }
    | scalar `elem` ([ Monotype.Natural, Monotype.Integer, Monotype.Real ] :: [Monotype.Scalar])= do
    check left _B

    _Γ <- get

    check right (Context.solveType _Γ _B)
check Syntax.Operator{ operator = Syntax.Plus, .. } _B@Type.Scalar{ scalar }
    | scalar `elem` ([ Monotype.Natural, Monotype.Integer, Monotype.Real, Monotype.Text ] :: [Monotype.Scalar]) = do
    check left _B

    _Γ <- get

    check right (Context.solveType _Γ _B)
check Syntax.Operator{ operator = Syntax.Plus, .. } _B@Type.List{} = do
    check left _B

    _Γ <- get

    check right (Context.solveType _Γ _B)

check Syntax.List{ location = _, ..} Type.List{..} = do
    let process element = do
            _Γ <- get

            check element (Context.solveType _Γ type_)

    traverse_ process elements

check e@Syntax.Record{ fieldValues } _B@Type.Record{ fields = Type.Fields fieldTypes fields }
    | let mapValues = Map.fromList fieldValues
    , let mapTypes  = Map.fromList fieldTypes

    , let extraValues = Map.difference mapValues mapTypes
    , let extraTypes  = Map.difference mapTypes  mapValues

    , let both = Map.intersectionWith (,) mapValues mapTypes
    , not (Map.null both) = do
        let process (value, type_) = do
                _Γ <- get

                check value (Context.solveType _Γ type_)

        traverse_ process both

        let e' = Syntax.Record
                { fieldValues = Map.toList extraValues
                , location = Syntax.location e
                }

        let _B' = Type.Record
                { location = Type.location _B
                , fields = Type.Fields (Map.toList extraTypes) fields
                }

        _Γ <- get

        check e' (Context.solveType _Γ _B')

check Syntax.Record{..} _B@Type.Scalar{ scalar = Monotype.JSON } = do
    let process (_, value) = do
            _Γ <- get

            check value (Context.solveType _Γ _B)

    traverse_ process fieldValues
check Syntax.List{..} _B@Type.Scalar{ scalar = Monotype.JSON } = do
    traverse_ (`check` _B) elements
check Syntax.Scalar{ scalar = Syntax.Text _ } Type.Scalar{ scalar = Monotype.JSON } = do
    return ()
check Syntax.Scalar{ scalar = Syntax.Natural _ } Type.Scalar{ scalar = Monotype.JSON } = do
    return ()
check Syntax.Scalar{ scalar = Syntax.Integer _ } Type.Scalar{ scalar = Monotype.JSON } = do
    return ()
check Syntax.Scalar{ scalar = Syntax.Real _ } Type.Scalar{ scalar = Monotype.JSON } = do
    return ()
check Syntax.Scalar{ scalar = Syntax.Bool _ } Type.Scalar{ scalar = Monotype.JSON } = do
    return ()
check Syntax.Scalar{ scalar = Syntax.Null } Type.Scalar{ scalar = Monotype.JSON } = do
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
inferApplication Type.Forall{ domain = Domain.Type, .. } e = do
    a <- fresh

    push (Context.UnsolvedType a)

    let a' = Type.UnsolvedType{ location = nameLocation, existential = a}

    inferApplication (Type.substituteType name a' type_) e
inferApplication Type.Forall{ domain = Domain.Fields, .. } e = do
    a <- fresh

    push (Context.UnsolvedFields a)

    let a' = Type.Fields [] (Monotype.UnsolvedFields a)

    inferApplication (Type.substituteFields name a' type_) e
inferApplication Type.Forall{ domain = Domain.Alternatives, .. } e = do
    a <- fresh

    push (Context.UnsolvedAlternatives a)

    let a' = Type.Alternatives [] (Monotype.UnsolvedAlternatives a)

    inferApplication (Type.substituteAlternatives name a' type_) e

-- ∃App
inferApplication Type.Exists{..} e = do
    scoped (Context.Variable domain name) do
        inferApplication type_ e

-- αApp
inferApplication Type.UnsolvedType{ existential = a, .. } e = do
    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedType a _Γ `orDie` MissingVariable a _Γ

    a1 <- fresh
    a2 <- fresh

    set (_ΓR <> (Context.SolvedType a (Monotype.Function (Monotype.UnsolvedType a1) (Monotype.UnsolvedType a2)) : Context.UnsolvedType a1 : Context.UnsolvedType a2 : _ΓL))

    check e Type.UnsolvedType{ existential = a1, .. }

    return Type.UnsolvedType{ existential = a2, .. }
inferApplication Type.Function{..} e = do
    check e input

    return output
inferApplication Type.VariableType{..} _ = do
    throwError (NotNecessarilyFunctionType location name)
inferApplication _A _ = do
    throwError (NotFunctionType (location _A) _A)

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
    | IllFormedType Location (Type Location) (Context Location)
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
    | NotRecordSubtype Location (Type Location) Location (Type Location)
    | NotUnionSubtype Location (Type Location) Location (Type Location)
    | NotSubtype Location (Type Location) Location (Type Location)
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
    displayException (IllFormedAlternatives location a0 _Γ) =
        "Internal error: Invalid context\n\
        \\n\
        \The following unsolved alternatives variable:\n\
        \\n\
        \" <> insert (Context.UnsolvedAlternatives a0) <> "\n\
        \\n\
        \… is not well-formed within the following context:\n\
        \\n\
        \#{listToText _Γ}\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location)

    displayException (IllFormedFields location a0 _Γ) =
        "Internal error: Invalid context\n\
        \\n\
        \The following unsolved fields variable:\n\
        \\n\
        \" <> insert (Context.UnsolvedFields a0) <> "\n\
        \\n\
        \… is not well-formed within the following context:\n\
        \\n\
        \" <> listToText _Γ <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location)

    displayException (IllFormedType location _A _Γ) =
        "Internal error: Invalid context\n\
        \\n\
        \The following type:\n\
        \\n\
        \" <> insert _A <> "\n\
        \\n\
        \… is not well-formed within the following context:\n\
        \\n\
        \" <> listToText _Γ <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location)

    displayException (InvalidOperands location _L') =
        "Invalid operands\n\
        \\n\
        \You cannot add values of type:\n\
        \\n\
        \" <> insert _L' <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location)

    displayException (MergeConcreteRecord location _R) =
        "Must merge a concrete record\n\
        \\n\
        \The first argument to a merge expression must be a record where all fields are\n\
        \statically known.  However, you provided an argument of type:\n\
        \\n\
        \" <> insert _R <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location) <> "\n\
        \\n\
        \… where not all fields could be inferred."

    displayException (MergeInvalidHandler location _A) =
        "Invalid handler\n\
        \\n\
        \The merge keyword expects a record of handlers where all handlers are functions,\n\
        \but you provided a handler of the following type:\n\
        \\n\
        \" <> insert _A <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location) <> "\n\
        \\n\
        \… which is not a function type."

    displayException (MergeRecord location _R) =
        "Must merge a record\n\
        \\n\
        \The first argument to a merge expression must be a record, but you provided an\n\
        \expression of the following type:\n\
        \\n\
        \" <> insert _R <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location) <> "\n\
        \\n\
        \… which is not a record type."

    displayException (MissingAllAlternatives p0 _Γ) =
        "Internal error: Invalid context\n\
        \\n\
        \The following unsolved alternatives variable:\n\
        \\n\
        \" <> insert (Context.UnsolvedAlternatives p0) <> "\n\
        \\n\
        \… cannot be instantiated because the alternatives variable is missing from the\n\
        \context:\n\
        \\n\
        \" <> listToText _Γ

    displayException (MissingAllFields p0 _Γ) =
        "Internal error: Invalid context\n\
        \\n\
        \The following unsolved fields variable:\n\
        \\n\
        \" <> insert (Context.UnsolvedFields p0) <> "\n\
        \\n\
        \… cannot be instantiated because the fields variable is missing from the\n\
        \context:\n\
        \\n\
        \" <> listToText _Γ

    displayException (MissingOneOfAlternatives locations p0 p1 _Γ) =
        "Internal error: Invalid context\n\
        \\n\
        \One of the following alternatives variables:\n\
        \\n\
        \" <> listToText [Context.UnsolvedAlternatives p0, Context.UnsolvedAlternatives p1 ] <> "\n\
        \\n\
        \… is missing from the following context:\n\
        \\n\
        \" <> listToText _Γ <> "\n\
        \\n\
        \" <> locations'
        where
            locations' =
                Text.unpack (Text.unlines (map (Location.renderError "") locations))

    displayException (MissingOneOfFields locations p0 p1 _Γ) =
        "Internal error: Invalid context\n\
        \\n\
        \One of the following fields variables:\\n\
        \\n\
        \" <> listToText [Context.UnsolvedFields p0, Context.UnsolvedFields p1 ] <> "\n\
        \\n\
        \… is missing from the following context:\n\
        \\n\
        \" <> listToText _Γ <> "\n\
        \\n\
        \" <> locations'
        where
            locations' =
                Text.unpack (Text.unlines (map (Location.renderError "") locations))

    displayException (MissingVariable a _Γ) =
        "Internal error: Invalid context\n\
        \\n\
        \The following unsolved variable:\n\
        \\n\
        \" <> insert (Context.UnsolvedType a) <> "\n\
        \\n\
        \… cannot be solved because the variable is missing from the context:\n\
        \\n\
        \" <> listToText _Γ

    displayException (NotFunctionType location _A) =
        "Not a function type\n\
        \\n\
        \An expression of the following type:\n\
        \\n\
        \" <> insert _A <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location) <> "\n\
        \\n\
        \… was invoked as if it were a function, but the above type is not a function\n\
        \type."

    displayException (NotNecessarilyFunctionType location a) =
        "Not necessarily a function type\n\
        \\n\
        \The following type variable:\n\
        \\n\
        \" <> insert a <> "\n\
        \\n\
        \… could potentially be any type and is not necessarily a function type.\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location)

    displayException (NotAlternativesSubtype location p0 alternatives) =
        "Not an alternatives subtype\n\
        \\n\
        \The following alternatives variable:\n\
        \\n\
        \" <> insert p0 <> "\n\
        \\n\
        \… cannot be instantiated to the following union type:\n\
        \\n\
        \" <> insert (Type.Union location alternatives) <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location) <> "\n\
        \\n\
        \… because the same alternatives variable appears within that union type."

    displayException (NotFieldsSubtype location p0 fields) =
        "Not a fields subtype\n\
        \\n\
        \The following fields variable:\n\
        \\n\
        \" <> insert p0 <> "\n\
        \\n\
        \… cannot be instantiated to the following record type:\n\
        \\n\
        \" <> insert (Type.Record location fields) <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location) <> "\n\
        \\n\
        \… because the same fields variable appears within that record type."

    displayException (NotRecordSubtype locA0 _A locB0 _B) =
        "Not a record subtype\n\
        \\n\
        \The following type:\n\
        \\n\
        \" <> insert _A <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" locA0) <> "\n\
        \\n\
        \… cannot be a subtype of:\n\
        \\n\
        \" <> insert _B <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" locB0)

    displayException (NotUnionSubtype locA0 _A locB0 _B) =
        "Not a union subtype\n\
        \\n\
        \The following type:\n\
        \\n\
        \" <> insert _A <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" locA0) <> "\n\
        \\n\
        \… cannot be a subtype of:\n\
        \\n\
        \" <> insert _B <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" locB0)

    displayException (NotSubtype locA0 _A locB0 _B) =
        "Not a subtype\n\
        \\n\
        \The following type:\n\
        \\n\
        \" <> insert _A <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" locA0) <> "\n\
        \\n\
        \… cannot be a subtype of:\n\
        \\n\
        \" <> insert _B <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" locB0)

    displayException (UnboundAlternatives location a) =
        "Unbound alternatives variable: " <> Text.unpack a <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location)

    displayException (UnboundFields location a) =
        "Unbound fields variable: " <> Text.unpack a <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location)

    displayException (UnboundTypeVariable location a) =
        "Unbound type variable: " <> Text.unpack a <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location)

    displayException (UnboundVariable location name index) =
        "Unbound variable: " <> Text.unpack var <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location)
        where
            var = prettyToText @(Syntax.Syntax () Void) Syntax.Variable{ location = (), .. }

    displayException (RecordTypeMismatch _A0 _B0 extraA extraB) | extraB == mempty =
        "Record type mismatch\n\
        \\n\
        \The following record type:\n\
        \\n\
        \" <> insert _A0 <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _A0)) <> "\n\
        \\n\
        \… is not a subtype of the following record type:\n\
        \\n\
        \" <> insert _B0 <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _B0)) <> "\n\
        \\n\
        \The former record has the following extra fields:\n\
        \\n\
        \" <> listToText (Map.keys extraA)

    displayException (RecordTypeMismatch _A0 _B0 extraA extraB) | extraA == mempty =
        "Record type mismatch\n\
        \\n\
        \The following record type:\n\
        \\n\
        \" <> insert _A0 <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _A0)) <> "\n\
        \\n\
        \… is not a subtype of the following record type:\n\
        \\n\
        \" <> insert _B0 <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _B0)) <> "\n\
        \\n\
        \The latter record has the following extra fields:\n\
        \\n\
        \" <> listToText (Map.keys extraB)

    displayException (RecordTypeMismatch _A0 _B0 extraA extraB) =
        "Record type mismatch\n\
        \\n\
        \The following record type:\n\
        \\n\
        \" <> insert _A0 <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _A0)) <> "\n\
        \\n\
        \… is not a subtype of the following record type:\n\
        \\n\
        \" <> insert _B0 <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _B0)) <> "\n\
        \\n\
        \The former record has the following extra fields:\n\
        \\n\
        \" <> listToText (Map.keys extraA) <> "\n\
        \\n\
        \… while the latter record has the following extra fields:\n\
        \\n\
        \" <> listToText (Map.keys extraB)

    displayException (UnionTypeMismatch _A0 _B0 extraA extraB) | extraB == mempty =
        "Union type mismatch\n\
        \\n\
        \The following union type:\n\
        \\n\
        \" <> insert _A0 <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _A0)) <> "\n\
        \\n\
        \… is not a subtype of the following union type:\n\
        \\n\
        \" <> insert _B0 <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _B0)) <> "\n\
        \\n\
        \The former union has the following extra alternatives:\n\
        \\n\
        \" <> listToText (Map.keys extraA)

    displayException (UnionTypeMismatch _A0 _B0 extraA extraB) | extraA == mempty =
        "Union type mismatch\n\
        \\n\
        \The following union type:\n\
        \\n\
        \" <> insert _A0 <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _A0)) <> "\n\
        \\n\
        \… is not a subtype of the following union type:\n\
        \\n\
        \" <> insert _B0 <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _B0)) <> "\n\
        \\n\
        \The latter union has the following extra alternatives:\n\
        \\n\
        \" <> listToText (Map.keys extraB)

    displayException (UnionTypeMismatch _A0 _B0 extraA extraB) =
        "Union type mismatch\n\
        \\n\
        \The following union type:\n\
        \\n\
        \" <> insert _A0 <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _A0)) <> "\n\
        \\n\
        \… is not a subtype of the following union type:\n\
        \\n\
        \" <> insert _B0 <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _B0)) <> "\n\
        \\n\
        \The former union has the following extra alternatives:\n\
        \\n\
        \" <> listToText (Map.keys extraA) <> "\n\
        \\n\
        \… while the latter union has the following extra alternatives:\n\
        \\n\
        \" <> listToText (Map.keys extraB)

-- Helper functions for displaying errors

insert :: Pretty a => a -> String
insert a = Text.unpack (prettyToText ("  " <> Pretty.align (pretty a)))

listToText :: Pretty a => [a] -> String
listToText elements =
    Text.unpack (Text.intercalate "\n" (map prettyEntry elements))
  where
    prettyEntry entry = prettyToText ("• " <> Pretty.align (pretty entry))

prettyToText :: Pretty a => a -> Text
prettyToText = Grace.Pretty.renderStrict False Width.defaultWidth
