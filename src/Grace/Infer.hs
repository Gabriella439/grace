{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

{-| This module is based on the bidirectional type-checking algorithm from:

    Dunfield, Jana, and Neelakantan R. Krishnaswami. \"Complete and easy bidirectional typechecking for higher-rank polymorphism.\" ACM SIGPLAN Notices 48.9 (2013): 429-442.

    The main differences from the original algorithm are:

    * This uses `Control.Monad.State.Strict.StateT` to thread around
      `Context`s and manipulate them instead of explicit `Context` passing as
      in the original paper

    * This algorithm adds support for row polymorphism and polymorphic variants
-}
module Grace.Infer
    ( -- * Type inference
      typeOf
    , typeWith
      -- * Errors related to type inference
    , TypeInferenceError(..)
    ) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (Exception(..), MonadCatch, MonadThrow)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Strict (MonadState)
import Data.Foldable (traverse_)
import Data.Sequence (ViewL(..), (<|))
import Data.Text (Text)
import Data.Void (Void)
import Grace.Context (Context, Entry)
import Grace.Existential (Existential)
import Grace.HTTP (Manager)
import Grace.Input (Input)
import Grace.Location (Location(..))
import Grace.Monotype (Monotype)
import Grace.Pretty (Pretty(..))
import Grace.Syntax (Syntax)
import Grace.Type (Type(..))

import qualified Control.Exception.Safe as Exception
import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import qualified Control.Monad.State as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Grace.Context as Context
import qualified Grace.Domain as Domain
import qualified Grace.Import as Import
import qualified Grace.Location as Location
import qualified Grace.Monotype as Monotype
import qualified Grace.Pretty
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type
import qualified Prettyprinter as Pretty

-- | Type-checking state
data Status = Status
    { count :: !Int
      -- ^ Used to generate fresh unsolved variables (e.g. α̂, β̂ from the
      --   original paper)

    , context :: Context Location
      -- ^ The type-checking context (e.g. Γ, Δ, Θ)

    , manager :: Manager
      -- ^ Used to resolve HTTP(S) imports

    , input :: Input
      -- ^ The parent import, used to resolve relative imports
    }

orDie :: (Exception e, MonadThrow m) => Maybe a -> e -> m a
Just x  `orDie` _ = return x
Nothing `orDie` e = Exception.throwIO e

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

{-| @`wellFormed` context type@ checks that all type/fields/alternatives
    variables within @type@ are declared within the @context@
-}
wellFormed :: MonadThrow m => Context Location -> Type Location -> m ()
wellFormed context Type.VariableType{..}
    | Context.Variable Domain.Type name `elem` context = do
        return ()
    | otherwise = do
        Exception.throwIO (UnboundTypeVariable location name)

wellFormed context Type.Function{..} = do
    wellFormed context input
    wellFormed context output

wellFormed context Type.Forall{..} = do
    wellFormed (Context.Variable domain name : context) type_

wellFormed context _A@Type.UnsolvedType{..}
    | all mismatch context = do
        Exception.throwIO (IllFormedType location _A context)
    | otherwise = do
        return ()
  where
    mismatch (Context.UnsolvedType a  ) = existential /= a
    mismatch (Context.SolvedType   a _) = existential /= a
    mismatch  _                         = True

wellFormed context Type.Optional{..} = do
    wellFormed context type_

wellFormed context Type.List{..} = do
    wellFormed context type_

wellFormed context Type.Record{ location, fields = Type.Fields kAs remainingFields } =
    case remainingFields of
        Monotype.UnsolvedFields a₀
            | all mismatch context ->
                Exception.throwIO (IllFormedFields location a₀ context)
          where
            mismatch (Context.UnsolvedFields a₁  ) = a₀ /= a₁
            mismatch (Context.SolvedFields   a₁ _) = a₀ /= a₁
            mismatch  _                            = True

        Monotype.VariableFields a
            | Context.Variable Domain.Fields a `notElem` context ->
                Exception.throwIO (UnboundFields location a)

        _ -> do
            traverse_ (\(_, _A) -> wellFormed context _A) kAs

wellFormed context Type.Union{ location, alternatives = Type.Alternatives kAs remainingAlternatives } =
    case remainingAlternatives of
        Monotype.UnsolvedAlternatives a₀
            | all mismatch context ->
                Exception.throwIO (IllFormedAlternatives location a₀ context)
          where
            mismatch (Context.UnsolvedAlternatives a₁  ) = a₀ /= a₁
            mismatch (Context.SolvedAlternatives   a₁ _) = a₀ /= a₁
            mismatch  _                                  = True
        Monotype.VariableAlternatives a
            | Context.Variable Domain.Alternatives a `notElem` context ->
                Exception.throwIO (UnboundAlternatives location a)
        _ ->
            traverse_ (\(_, _A) -> wellFormed context _A) kAs

wellFormed _ Type.Scalar{} = do
    return ()

-- | @`subtype` sub super@ checks that @sub@ is a subtype of @super@
subtype
    :: (MonadState Status m, MonadCatch m)
    => Type Location -> Type Location -> m ()
subtype _A₀ _B₀ = do
    _Γ <- get

    case (_A₀, _B₀) of
        (Type.VariableType{ name = a₀ }, Type.VariableType{ name = a₁ })
            | a₀ == a₁ -> do
                wellFormed _Γ _A₀

        (Type.UnsolvedType{ existential = a₀ }, Type.UnsolvedType{ existential = a₁ })
            | a₀ == a₁ && Context.UnsolvedType a₀ `elem` _Γ -> do
                return ()

        (Type.UnsolvedType{ existential = a }, _)
            -- The @not (a `Type.typeFreeIn` _B)@ is the "occurs check" which
            -- prevents a type variable from being defined in terms of itself
            -- (i.e. a type should not "occur" within itself).
            --
            -- Later on you'll see matching "occurs checks" for record types and
            -- union types so that Fields variables and Alternatives variables
            -- cannot refer to the record or union that they belong to,
            -- respectively.
            |   not (a `Type.typeFreeIn` _B₀)
            &&  elem (Context.UnsolvedType a) _Γ -> do
                instantiateTypeL a _B₀

        (_, Type.UnsolvedType{ existential = a})
            |   not (a `Type.typeFreeIn` _A₀)
            &&  elem (Context.UnsolvedType a) _Γ -> do
                instantiateTypeR _A₀ a

        (Type.Function{ input = _A₁, output = _A₂ }, Type.Function{ input= _B₁, output = _B₂ }) -> do
            subtype _B₁ _A₁

            -- CAREFULLY NOTE: Pay really close attention to how we need to use
            -- `Context.solveType` any time we do something that either updates
            -- the context or potentially updates the context (like the above
            -- `subtype` command).  If you forget to do this then you will get
            -- bugs due to unsolved variables not getting solved correctly.
            --
            -- A much more reliable way to fix this problem would simply be to
            -- have every function (like `subtype`, `instantiateL`, …)
            -- apply `solveType` to its inputs.  For example, this very
            -- `subtype` function could begin by doing:
            --
            --     _Γ <- get
            --     let _A₀' = Context.solveType _Γ _A₀
            --     let _B₀' = Context.solveType _Γ _B₀
            --
            -- … and then use _A₀' and _B₀' for downstream steps.  If we did
            -- that at the beginning of each function then everything would
            -- "just work".
            --
            -- However, this would be more inefficient because we'd calling
            -- `solveType` wastefully over and over with the exact same context
            -- in many cases.  So, the tradeoff here is that we get improved
            -- performance if we're willing to remember to call `solveType` in
            -- the right places.
            _Θ <- get

            subtype (Context.solveType _Θ _A₂) (Context.solveType _Θ _B₂)

        (_, Type.Forall{..}) -> do
            scoped (Context.Variable domain name) do
                subtype _A₀ type_

        (Type.Forall{ domain = Domain.Type, .. }, _) -> do
            scopedUnsolvedType nameLocation \a -> do
                subtype (Type.substituteType name 0 a type_) _B₀

        (Type.Forall{ domain = Domain.Fields, .. }, _) -> do
            scopedUnsolvedFields \a -> do
                subtype (Type.substituteFields name 0 a type_) _B₀

        (Type.Forall{ domain = Domain.Alternatives, .. }, _) -> do
            scopedUnsolvedAlternatives \a -> do
                subtype (Type.substituteAlternatives name 0 a type_) _B₀

        (Type.Scalar{ scalar = s₀ }, Type.Scalar{ scalar = s₁ })
            | s₀ == s₁ -> do
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

        (Type.Scalar{ }, Type.Scalar{ scalar = Monotype.JSON }) -> do
            return ()

        (Type.List{ type_ = _A }, Type.Scalar{ scalar = Monotype.JSON }) -> do
            subtype _A _B₀

        (Type.Record{ fields = Type.Fields kAs Monotype.EmptyFields }, Type.Scalar{ scalar = Monotype.JSON }) -> do
            let process (_, _A) = do
                    _Γ <- get

                    subtype _A (Context.solveType _Γ _B₀)

            traverse_ process kAs

        -- The type-checking code for records is the first place where we
        -- implement a non-trivial type that wasn't already covered by the
        -- paper, so we'll go into more detail here to explain the general
        -- type-checking principles of the paper.
        (Type.Record{ fields = Type.Fields kAs₀ fields₀ }, Type.Record{ fields = Type.Fields kBs₀ fields₁ }) -> do
            let mapA = Map.fromList kAs₀
            let mapB = Map.fromList kBs₀

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            -- If record A is a subtype of record B, then all fields in A
            -- must be a subtype of the matching fields in record B
            let process _A₁ _B₁ = do
                    _Θ <- get

                    subtype
                        (Context.solveType _Θ _A₁)
                        (Context.solveType _Θ _B₁)

            sequence_ (Map.intersectionWith process mapA mapB)

            let isRequired _B₁ = do
                    context <- get

                    let assertOptional = do
                            existential <- fresh

                            push (Context.UnsolvedType existential)

                            let optional = Type.Optional{ .. }
                                  where
                                    location = Type.location _B₁

                                    type_ = Type.UnsolvedType{ existential, .. }

                            subtype _B₁ optional

                            return False

                    assertOptional `Exception.catch` \(_ :: TypeInferenceError) -> do
                        set context

                        return True

            let getRequired = do
                    m <- traverse isRequired extraB

                    return (Map.keys (Map.filter id m))

            -- Here is where we handle extra fields that were only present in
            -- the subtype or supertype.  They still might be okay if one or
            -- both record types has an unsolved fields variable or if extra
            -- fields in the supertype are `Optional`
            case (fields₀, fields₁) of
                -- Both records type have unsolved Fields variables.  Great!
                -- This is the most flexible case, since we can replace these
                -- unsolved variables with whatever fields we want to make the
                -- types match.
                --
                -- However, it's not as simple as setting each Fields variable
                -- to the extra fields from the opposing record type.  For
                -- example, if the two record types we're comparing are:
                --
                --     { x: Bool, p₀ } <: { y: Text, p₁ }
                --
                -- … then it's not correct to say:
                --
                --     p₀ = y: Text
                --     p₁ = x: Bool
                --
                -- … because that is not the most general solution for `p₀` and
                -- `p₁`!  The actual most general solution is:
                --
                --     p₀ = y: Text, p₂
                --     p₁ = x: Bool, p₂
                --
                -- … where `p₂` is a fresh Fields type variable representing the
                -- fact that both records could potentially have even more
                -- fields other than `x` and `y`.
                (Monotype.UnsolvedFields p₀, Monotype.UnsolvedFields p₁)
                    | p₀ /= p₁ -> do
                        p₂ <- fresh

                        _Γ₀ <- get

                        -- We have to insert p₂ before both p₀ and p₁ within the
                        -- context because the bidirectional type-checking algorithm
                        -- requires that the context is ordered and all variables
                        -- within the context can only reference prior variables
                        -- within the context.
                        --
                        -- Since `p₀` and `p₁` both have to reference `p₂`, then we
                        -- need to insert `p₂` right before `p₀` or `p₁`, whichever
                        -- one comes first
                        let p₀First = do
                                (_Γ', _Γ) <- Context.splitOnUnsolvedFields p₀ _Γ₀

                                Monad.guard (Context.UnsolvedFields p₁ `elem` _Γ')

                                let command =
                                        set (   _Γ'
                                            <>  ( Context.UnsolvedFields p₀
                                                : Context.UnsolvedFields p₂
                                                : _Γ
                                                )
                                            )

                                return command

                        let p₁First = do
                                (_Γ', _Γ) <- Context.splitOnUnsolvedFields p₁ _Γ₀

                                Monad.guard (Context.UnsolvedFields p₀ `elem` _Γ')

                                let command =
                                        set (   _Γ'
                                            <>  ( Context.UnsolvedFields p₁
                                                : Context.UnsolvedFields p₂
                                                : _Γ
                                                )
                                            )

                                return command

                        case p₀First <|> p₁First of
                            Nothing -> do
                                Exception.throwIO (MissingOneOfFields [Type.location _A₀, Type.location _B₀] p₀ p₁ _Γ)

                            Just setContext -> do
                                setContext

                        _Θ <- get

                        -- Now we solve for `p₀`.  This is basically saying:
                        --
                        -- p₀ = extraFieldsFromRecordB, p₂
                        instantiateFieldsL
                            p₀
                            (Type.location _B₀)
                            (Context.solveRecord _Θ
                                (Type.Fields (Map.toList extraB)
                                    (Monotype.UnsolvedFields p₂)
                                )
                            )

                        _Δ <- get

                        -- Similarly, solve for `p₁`.  This is basically saying:
                        --
                        -- p₁ = extraFieldsFromRecordA, p₂
                        instantiateFieldsR
                            (Type.location _A₀)
                            (Context.solveRecord _Δ
                                (Type.Fields (Map.toList extraA)
                                    (Monotype.UnsolvedFields p₂)
                                )
                            )
                            p₁

                -- If only one of the records has a Fields variable then the
                -- solution is simpler: just set the Fields variable to the
                -- extra fields from the opposing record.
                (Monotype.UnsolvedFields p₀, _) -> do
                    _Θ <- get

                    instantiateFieldsL
                        p₀
                        (Type.location _B₀)
                        (Context.solveRecord _Θ
                            (Type.Fields (Map.toList extraB) fields₁)
                        )

                (_, Monotype.UnsolvedFields p₁) -> do
                    requiredB <- getRequired

                    Monad.unless (null requiredB) do
                        Exception.throwIO (RecordTypeMismatch _A₀ _B₀ requiredB)

                    _Θ <- get

                    instantiateFieldsR
                        (Type.location _A₀)
                        (Context.solveRecord _Θ
                            (Type.Fields (Map.toList extraA) fields₀)
                        )
                        p₁

                _   | fields₀ == fields₁ -> do
                        requiredB <- getRequired

                        Monad.unless (null requiredB) do
                            Exception.throwIO (RecordTypeMismatch _A₀ _B₀ requiredB)

                    | otherwise -> do
                        requiredB <- getRequired

                        Exception.throwIO (RecordTypeMismatch _A₀ _B₀ requiredB)

        (_A@Type.Union{ alternatives = Type.Alternatives kAs₀ alternatives₀ }, _B@Type.Union{ alternatives = Type.Alternatives kBs₀ alternatives₁ }) -> do
            let mapA = Map.fromList kAs₀
            let mapB = Map.fromList kBs₀

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let process _A₁ _B₁ = do
                    _Θ <- get

                    subtype
                        (Context.solveType _Θ _A₁)
                        (Context.solveType _Θ _B₁)

            sequence_ (Map.intersectionWith process mapA mapB)

            case (alternatives₀, alternatives₁) of
                (Monotype.UnsolvedAlternatives p₀, Monotype.UnsolvedAlternatives p₁)
                    | p₀ == p₁ -> do
                        p₂ <- fresh

                        _Γ₀ <- get

                        let p₀First = do
                                (_Γ', _Γ) <- Context.splitOnUnsolvedAlternatives p₀ _Γ₀

                                Monad.guard (Context.UnsolvedAlternatives p₁ `elem` _Γ')

                                let command =
                                        set (   _Γ'
                                            <>  ( Context.UnsolvedAlternatives p₀
                                                : Context.UnsolvedAlternatives p₂
                                                : _Γ
                                                )
                                            )

                                return command

                        let p₁First = do
                                (_Γ', _Γ) <- Context.splitOnUnsolvedAlternatives p₁ _Γ₀

                                Monad.guard (Context.UnsolvedAlternatives p₀ `elem` _Γ')

                                let command =
                                        set (   _Γ'
                                            <>  ( Context.UnsolvedAlternatives p₁
                                                : Context.UnsolvedAlternatives p₂
                                                : _Γ
                                                )
                                            )

                                return command

                        case p₀First <|> p₁First of
                            Nothing -> do
                                Exception.throwIO (MissingOneOfAlternatives [Type.location _A₀, Type.location _B₀] p₀ p₁ _Γ)

                            Just setContext -> do
                                setContext

                        _Θ <- get

                        instantiateAlternativesL
                            p₀
                            (Type.location _B₀)
                            (Context.solveUnion _Θ
                                (Type.Alternatives (Map.toList extraB)
                                    (Monotype.UnsolvedAlternatives p₂)
                                )
                            )

                        _Δ <- get

                        instantiateAlternativesR
                            (Type.location _A₀)
                            (Context.solveUnion _Δ
                                (Type.Alternatives (Map.toList extraA)
                                    (Monotype.UnsolvedAlternatives p₂)
                                )
                            )
                            p₁

                (Monotype.UnsolvedAlternatives p₀, _)
                    | Map.null extraA -> do
                        _Θ <- get

                        instantiateAlternativesL
                            p₀
                            (Type.location _B₀)
                            (Context.solveUnion _Θ
                                (Type.Alternatives (Map.toList extraB)
                                    alternatives₁
                                )
                            )

                (_, Monotype.UnsolvedAlternatives p₁) -> do
                    _Θ <- get

                    instantiateAlternativesR
                        (Type.location _A₀)
                        (Context.solveUnion _Θ
                            (Type.Alternatives (Map.toList extraA)
                                alternatives₀
                            )
                        )
                        p₁

                _   | alternatives₀ == alternatives₁ && Map.null extraA -> do
                        return ()

                    | otherwise -> do
                        Exception.throwIO (UnionTypeMismatch _A₀ _B₀ (Map.keys extraA))


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
            Exception.throwIO (NotSubtype (Type.location _A₀) _A (Type.location _B₀) _B)

{-| This corresponds to the judgment:

    > Γ ⊢ α̂ :≦ A ⊣ Δ

    … which updates the context Γ to produce the new context Δ, by instantiating
    α̂ such that α̂ <: A.

    The @instantiate*@ family of functions should really be called @solve*@
    because their job is to solve an unsolved variable within the context.
    However, for consistency with the paper we still name them @instantiate*@.
-}
instantiateTypeL
    :: (MonadState Status m, MonadCatch m)
    => Existential Monotype -> Type Location -> m ()
instantiateTypeL a _A₀ = do
    _Γ₀ <- get

    (_Γ', _Γ) <- Context.splitOnUnsolvedType a _Γ₀ `orDie` MissingVariable a _Γ₀

    let instLSolve τ = do
            wellFormed _Γ _A₀

            set (_Γ' <> (Context.SolvedType a τ : _Γ))

    case _A₀ of
        -- InstLReach
        Type.UnsolvedType{..}
            | Just (_Γ', _ΓM) <- Context.splitOnUnsolvedType existential _Γ' -> do
                set (_Γ' <> (Context.SolvedType existential (Monotype.UnsolvedType a) : _ΓM) <> (Context.UnsolvedType a : _Γ))

        -- InstLSolve
        Type.UnsolvedType{..} -> do
            instLSolve (Monotype.UnsolvedType existential)
        Type.VariableType{..} -> do
            instLSolve (Monotype.VariableType name)
        Type.Scalar{..} -> do
            instLSolve (Monotype.Scalar scalar)

        -- InstLArr
        Type.Function{..} -> do
            a₁ <- fresh
            a₂ <- fresh

            set (_Γ' <> (Context.SolvedType a (Monotype.Function (Monotype.UnsolvedType a₁) (Monotype.UnsolvedType a₂)) : Context.UnsolvedType a₁ : Context.UnsolvedType a₂ : _Γ))

            instantiateTypeR input a₁

            _Θ <- get

            instantiateTypeL a₂ (Context.solveType _Θ output)

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
            -- To solve `a` against `Optional _A` we create a fresh unsolved
            -- variable named `a₁`, …
            a₁ <- fresh

            -- … solve `a` to `Optional a₁`, taking care that `a₁` comes before
            -- `a` within the context, (since `a` refers to `a₁`)  …
            set (_Γ' <> (Context.SolvedType a (Monotype.Optional (Monotype.UnsolvedType a₁)) : Context.UnsolvedType a₁ : _Γ))

            -- … and then solve `a₁` against _A`
            instantiateTypeL a₁ type_

        -- We solve an unsolved variable against `List` using the same
        -- principles described above for solving `Optional`
        Type.List{..} -> do
            a₁ <- fresh

            set (_Γ' <> (Context.SolvedType a (Monotype.List (Monotype.UnsolvedType a₁)) : Context.UnsolvedType a₁ : _Γ))

            instantiateTypeL a₁ type_

        Type.Record{ fields = Type.Fields fieldTypes remainingFields } -> do
            p <- fresh

            let process (field, type_) = do
                    existential <- fresh

                    let monotype = Monotype.UnsolvedType existential

                    let entry = Context.UnsolvedType existential

                    let instantiation = do
                            _Θ <- get

                            instantiateTypeL existential (Context.solveType _Θ type_)

                    return ((field, monotype), entry, instantiation)

            results <- traverse process fieldTypes

            let (fieldMonotypes, entries, instantiations) = unzip3 results

            let recordMonotype =
                    Monotype.Record
                        (Monotype.Fields fieldMonotypes (Monotype.UnsolvedFields p))

            set (_Γ' <> (Context.SolvedType a recordMonotype : Context.UnsolvedFields p : (entries <> _Γ)))

            instantiateFieldsL p (Type.location _A₀) (Type.Fields [] remainingFields)

            sequence_ instantiations

        Type.Union{ alternatives = Type.Alternatives alternativeTypes remainingAlternatives } -> do
            p <- fresh

            let process (alternative, type_) = do
                    existential <- fresh

                    let monotype = Monotype.UnsolvedType existential

                    let entry = Context.UnsolvedType existential

                    let instantiation = do
                            _Θ <- get

                            instantiateTypeL existential (Context.solveType _Θ type_)

                    return ((alternative, monotype), entry, instantiation)

            results <- traverse process alternativeTypes

            let (alternativeMonotypes, entries, instantiations) = unzip3 results

            let unionMonotype =
                    Monotype.Union
                        (Monotype.Alternatives alternativeMonotypes (Monotype.UnsolvedAlternatives p))

            set (_Γ' <> (Context.SolvedType a unionMonotype : Context.UnsolvedAlternatives p : (entries <> _Γ)))

            instantiateAlternativesL p (Type.location _A₀) (Type.Alternatives [] remainingAlternatives)

            sequence_ instantiations

{-| This corresponds to the judgment:

    > Γ ⊢ A ≦: α̂ ⊣ Δ

    … which updates the context Γ to produce the new context Δ, by instantiating
    α̂ such that A :< α̂.
-}
instantiateTypeR
    :: (MonadState Status m, MonadCatch m)
    => Type Location -> Existential Monotype -> m ()
instantiateTypeR _A₀ a = do
    _Γ₀ <- get

    (_Γ', _Γ) <- Context.splitOnUnsolvedType a _Γ₀ `orDie` MissingVariable a _Γ₀

    let instRSolve τ = do
            wellFormed _Γ _A₀

            set (_Γ' <> (Context.SolvedType a τ : _Γ))

    case _A₀ of
        -- InstRReach
        Type.UnsolvedType{..}
            | Just (_Γ', _ΓM) <- Context.splitOnUnsolvedType existential _Γ' -> do
                set (_Γ' <> (Context.SolvedType existential (Monotype.UnsolvedType a) : _ΓM) <> (Context.UnsolvedType a : _Γ))

        -- InstRSolve
        Type.UnsolvedType{..} -> do
            instRSolve (Monotype.UnsolvedType existential)
        Type.VariableType{..} -> do
            instRSolve (Monotype.VariableType name)
        Type.Scalar{..} -> do
            instRSolve (Monotype.Scalar scalar)

        -- InstRArr
        Type.Function{..} -> do
            a₁ <- fresh
            a₂ <- fresh

            set (_Γ' <> (Context.SolvedType a (Monotype.Function (Monotype.UnsolvedType a₁) (Monotype.UnsolvedType a₂)) : Context.UnsolvedType a₁ : Context.UnsolvedType a₂ : _Γ))

            instantiateTypeL a₁ input

            _Θ <- get

            instantiateTypeR (Context.solveType _Θ output) a₂

        -- InstRAllL
        Type.Forall{ domain = Domain.Type, .. } -> do
            scopedUnsolvedType nameLocation \b -> do
                instantiateTypeR (Type.substituteType name 0 b type_) a
        Type.Forall{ domain = Domain.Fields, .. } -> do
            scopedUnsolvedFields \b -> do
                instantiateTypeR (Type.substituteFields name 0 b type_) a
        Type.Forall{ domain = Domain.Alternatives, .. } -> do
            scopedUnsolvedAlternatives \b -> do
                instantiateTypeR (Type.substituteAlternatives name 0 b type_) a

        Type.Optional{..} -> do
            a₁ <- fresh

            set (_Γ' <> (Context.SolvedType a (Monotype.Optional (Monotype.UnsolvedType a₁)) : Context.UnsolvedType a₁ : _Γ))

            instantiateTypeR type_ a₁

        Type.List{..} -> do
            a₁ <- fresh

            set (_Γ' <> (Context.SolvedType a (Monotype.List (Monotype.UnsolvedType a₁)) : Context.UnsolvedType a₁ : _Γ))

            instantiateTypeR type_ a₁

        Type.Record{ fields = Type.Fields fieldTypes remainingFields } -> do
            p <- fresh

            let process (field, type_) = do
                    existential <- fresh

                    let monotype = Monotype.UnsolvedType existential

                    let entry = Context.UnsolvedType existential

                    let instantiation = do
                            _Θ <- get

                            instantiateTypeR (Context.solveType _Θ type_) existential

                    return ((field, monotype), entry, instantiation)

            results <- traverse process fieldTypes

            let (fieldMonotypes, entries, instantiations) = unzip3 results

            let recordMonotype =
                    Monotype.Record
                        (Monotype.Fields fieldMonotypes (Monotype.UnsolvedFields p))

            set (_Γ' <> (Context.SolvedType a recordMonotype : Context.UnsolvedFields p : (entries <> _Γ)))

            instantiateFieldsR (Type.location _A₀) (Type.Fields [] remainingFields) p

            sequence_ instantiations

        Type.Union{ alternatives = Type.Alternatives alternativeTypes remainingAlternatives } -> do
            p <- fresh

            let process (alternative, type_) = do
                    existential <- fresh

                    let monotype = Monotype.UnsolvedType existential

                    let entry = Context.UnsolvedType existential

                    let instantiation = do
                            _Θ <- get

                            instantiateTypeR (Context.solveType _Θ type_) existential

                    return ((alternative, monotype), entry, instantiation)

            results <- traverse process alternativeTypes

            let (alternativeMonotypes, entries, instantiations) = unzip3 results

            let unionMonotype =
                    Monotype.Union
                        (Monotype.Alternatives alternativeMonotypes (Monotype.UnsolvedAlternatives p))

            set (_Γ' <> (Context.SolvedType a unionMonotype : Context.UnsolvedAlternatives p : (entries <> _Γ)))

            instantiateAlternativesR (Type.location _A₀) (Type.Alternatives [] remainingAlternatives) p

            sequence_ instantiations

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
    :: (MonadState Status m, MonadThrow m)
    => Existential Monotype.Record -> Existential Monotype.Record -> m ()
equateFields p₀ p₁ = do
    _Γ₀ <- get

    let p₀First = do
            (_Γ', _Γ) <- Context.splitOnUnsolvedFields p₁ _Γ₀

            Monad.guard (Context.UnsolvedFields p₀ `elem` _Γ)

            return (set (_Γ' <> (Context.SolvedFields p₁ (Monotype.Fields [] (Monotype.UnsolvedFields p₀)) : _Γ)))

    let p₁First = do
            (_Γ', _Γ) <- Context.splitOnUnsolvedFields p₀ _Γ₀

            Monad.guard (Context.UnsolvedFields p₁ `elem` _Γ)

            return (set (_Γ' <> (Context.SolvedFields p₀ (Monotype.Fields [] (Monotype.UnsolvedFields p₁)) : _Γ)))

    case p₀First <|> p₁First of
        Nothing -> do
            Exception.throwIO (MissingOneOfFields [] p₀ p₁ _Γ₀)

        Just setContext -> do
            setContext

instantiateFieldsL
    :: (MonadState Status m, MonadCatch m)
    => Existential Monotype.Record
    -> Location
    -> Type.Record Location
    -> m ()
instantiateFieldsL p₀ location fields@(Type.Fields kAs rest) = do
    when (p₀ `Type.fieldsFreeIn` Type.Record{..}) do
        Exception.throwIO (NotFieldsSubtype location p₀ fields)

    let process (k, _A) = do
            b <- fresh

            return (k, _A, b)

    kAbs <- traverse process kAs

    let bs  = map (\(_, _, b) -> Context.UnsolvedType b      ) kAbs
    let kbs = map (\(k, _, b) -> (k, Monotype.UnsolvedType b)) kAbs

    _Γ₀ <- get

    (_Γ', _Γ) <- Context.splitOnUnsolvedFields p₀ _Γ₀ `orDie` MissingAllFields p₀ _Γ₀

    case rest of
        Monotype.UnsolvedFields p₁ -> do
            p₂ <- fresh

            set (_Γ' <> (Context.SolvedFields p₀ (Monotype.Fields kbs (Monotype.UnsolvedFields p₂)) : Context.UnsolvedFields p₂ : bs <> _Γ))

            equateFields p₁ p₂

        _ -> do
            wellFormed (bs <> _Γ)
                Type.Record{ fields = Type.Fields [] rest, .. }

            set (_Γ' <> (Context.SolvedFields p₀ (Monotype.Fields kbs rest) : bs <> _Γ))

    let instantiate (_, _A, b) = do
            _Θ <- get

            instantiateTypeL b (Context.solveType _Θ _A)

    traverse_ instantiate kAbs

instantiateFieldsR
    :: (MonadState Status m, MonadCatch m)
    => Location
    -> Type.Record Location
    -> Existential Monotype.Record
    -> m ()
instantiateFieldsR location fields@(Type.Fields kAs rest) p₀ = do
    when (p₀ `Type.fieldsFreeIn` Type.Record{..}) do
        Exception.throwIO (NotFieldsSubtype location p₀ fields)

    let process (k, _A) = do
            b <- fresh

            return (k, _A, b)

    kAbs <- traverse process kAs

    let bs  = map (\(_, _, b) -> Context.UnsolvedType b      ) kAbs
    let kbs = map (\(k, _, b) -> (k, Monotype.UnsolvedType b)) kAbs

    _Γ₀ <- get

    (_Γ', _Γ) <- Context.splitOnUnsolvedFields p₀ _Γ₀ `orDie` MissingAllFields p₀ _Γ₀

    case rest of
        Monotype.UnsolvedFields p₁ -> do
            p₂ <- fresh

            set (_Γ' <> (Context.SolvedFields p₀ (Monotype.Fields kbs (Monotype.UnsolvedFields p₂)) : Context.UnsolvedFields p₂ : bs <> _Γ))

            equateFields p₁ p₂

        _ -> do
            wellFormed (bs <> _Γ)
                Type.Record{ fields = Type.Fields [] rest, .. }

            set (_Γ' <> (Context.SolvedFields p₀ (Monotype.Fields kbs rest) : bs <> _Γ))

    let instantiate (_, _A, b) = do
            _Θ <- get

            instantiateTypeR (Context.solveType _Θ _A) b

    traverse_ instantiate kAbs

equateAlternatives
    :: (MonadState Status m, MonadThrow m)
    => Existential Monotype.Union-> Existential Monotype.Union -> m ()
equateAlternatives p₀ p₁ = do
    _Γ₀ <- get

    let p₀First = do
            (_Γ', _Γ) <- Context.splitOnUnsolvedAlternatives p₁ _Γ₀

            Monad.guard (Context.UnsolvedAlternatives p₀ `elem` _Γ)

            return (set (_Γ' <> (Context.SolvedAlternatives p₁ (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives p₀)) : _Γ)))

    let p₁First = do
            (_Γ', _Γ) <- Context.splitOnUnsolvedAlternatives p₀ _Γ₀

            Monad.guard (Context.UnsolvedAlternatives p₁ `elem` _Γ)

            return (set (_Γ' <> (Context.SolvedAlternatives p₀ (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives p₁)) : _Γ)))

    case p₀First <|> p₁First of
        Nothing -> do
            Exception.throwIO (MissingOneOfAlternatives [] p₀ p₁ _Γ₀)

        Just setContext -> do
            setContext

instantiateAlternativesL
    :: (MonadState Status m, MonadCatch m)
    => Existential Monotype.Union
    -> Location
    -> Type.Union Location
    -> m ()
instantiateAlternativesL p₀ location alternatives@(Type.Alternatives kAs rest) = do
    when (p₀ `Type.alternativesFreeIn` Type.Union{..}) do
        Exception.throwIO (NotAlternativesSubtype location p₀ alternatives)

    let process (k, _A) = do
            b <- fresh

            return (k, _A, b)

    kAbs <- traverse process kAs

    let bs  = map (\(_, _, b) -> Context.UnsolvedType b      ) kAbs
    let kbs = map (\(k, _, b) -> (k, Monotype.UnsolvedType b)) kAbs

    _Γ₀ <- get

    (_Γ', _Γ) <- Context.splitOnUnsolvedAlternatives p₀ _Γ₀ `orDie` MissingAllAlternatives p₀ _Γ₀

    case rest of
        Monotype.UnsolvedAlternatives p₁ -> do
            p₂ <- fresh

            set (_Γ' <> (Context.SolvedAlternatives p₀ (Monotype.Alternatives kbs (Monotype.UnsolvedAlternatives p₂)) : Context.UnsolvedAlternatives p₂ : bs <> _Γ))

            equateAlternatives p₁ p₂

        _ -> do
            wellFormed (bs <> _Γ)
                Type.Union{ alternatives = Type.Alternatives [] rest, .. }

            set (_Γ' <> (Context.SolvedAlternatives p₀ (Monotype.Alternatives kbs rest) : bs <> _Γ))

    let instantiate (_, _A, b) = do
            _Θ <- get

            instantiateTypeL b (Context.solveType _Θ _A)

    traverse_ instantiate kAbs

instantiateAlternativesR
    :: (MonadState Status m, MonadCatch m)
    => Location
    -> Type.Union Location
    -> Existential Monotype.Union
    -> m ()
instantiateAlternativesR location alternatives@(Type.Alternatives kAs rest) p₀ = do
    when (p₀ `Type.alternativesFreeIn` Type.Union{..}) do
        Exception.throwIO (NotAlternativesSubtype location p₀ alternatives)

    let process (k, _A) = do
            b <- fresh

            return (k, _A, b)

    kAbs <- traverse process kAs

    let bs  = map (\(_, _, b) -> Context.UnsolvedType b      ) kAbs
    let kbs = map (\(k, _, b) -> (k, Monotype.UnsolvedType b)) kAbs

    _Γ₀ <- get

    (_Γ', _Γ) <- Context.splitOnUnsolvedAlternatives p₀ _Γ₀ `orDie` MissingAllAlternatives p₀ _Γ₀

    case rest of
        Monotype.UnsolvedAlternatives p₁ -> do
            p₂ <- fresh

            set (_Γ' <> (Context.SolvedAlternatives p₀ (Monotype.Alternatives kbs (Monotype.UnsolvedAlternatives p₂)) : Context.UnsolvedAlternatives p₂ : bs <> _Γ))

            equateAlternatives p₁ p₂

        _ -> do
            wellFormed (bs <> _Γ)
                Type.Union{ alternatives = Type.Alternatives [] rest, .. }

            set (_Γ' <> (Context.SolvedAlternatives p₀ (Monotype.Alternatives kbs rest) : bs <> _Γ))

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
    :: (MonadState Status m, MonadCatch m, MonadIO m)
    => Syntax Location Input
    -> m (Type Location, Syntax Location Void)
infer e₀ = do
    let input ~> output = Type.Function{ location = Syntax.location e₀, ..}

    let var name = Type.VariableType{ location = Syntax.location e₀, name, .. }

    case e₀ of
        -- Var
        Syntax.Variable{..} -> do
            _Γ <- get

            type_ <- Context.lookup name index _Γ `orDie` UnboundVariable location name index
            return (type_, Syntax.Variable{..})

        -- →I⇒
        Syntax.Lambda{ location, nameBinding, body } -> do
            (entries, input) <- case nameBinding of
                Syntax.NameBinding{ nameLocation, name, annotation = Nothing } -> do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    let input =
                            Type.UnsolvedType
                                { existential, location = nameLocation }

                    return ([Context.Annotation name input], input)

                Syntax.NameBinding{ name, annotation = Just input } -> do
                    return ([Context.Annotation name input], input)

                Syntax.FieldNamesBinding{ fieldNamesLocation, fieldNames } -> do
                    let process Syntax.FieldName{ fieldNameLocation, name, annotation = Nothing } = do
                            existential <- fresh

                            push (Context.UnsolvedType existential)

                            let type_ =
                                    Type.UnsolvedType
                                        { existential
                                        , location = fieldNameLocation
                                        }

                            return (name, type_)
                        process Syntax.FieldName{ name, annotation = Just type_ } = do
                            return (name, type_)

                    fieldTypes <- traverse process fieldNames

                    let entries = do
                            (name, type_) <- fieldTypes

                            return (Context.Annotation name type_)

                    existential <- fresh

                    push (Context.UnsolvedFields existential)

                    let input = Type.Record
                            { location = fieldNamesLocation
                            , fields =
                                Type.Fields fieldTypes (Monotype.UnsolvedFields existential)
                            }

                    return (entries, input)

            output <- do
                existential <- fresh

                push (Context.UnsolvedType existential)

                return Type.UnsolvedType{ location = Syntax.location body, .. }

            let done = do
                    newBody <- check body output

                    let newNameBinding = case nameBinding of
                            Syntax.NameBinding{..} -> Syntax.NameBinding{ annotation = Nothing, .. }
                            Syntax.FieldNamesBinding{..} -> Syntax.FieldNamesBinding{..}

                    _Γ <- get

                    return (Type.Function{..}, Syntax.Lambda{ nameBinding = newNameBinding, body = solveSyntax _Γ newBody, .. })

            foldr scoped done entries

        -- →E
        Syntax.Application{..} -> do
            (_A, newFunction) <- infer function

            _Θ <- get

            (type_, newArgument) <- inferApplication (Context.solveType _Θ _A) argument

            _Γ <- get

            return (type_, Syntax.Application{ function = solveSyntax _Γ newFunction, argument = solveSyntax _Γ newArgument, .. })

        -- Anno
        Syntax.Annotation{..} -> do
            _Γ <- get

            wellFormed _Γ annotation

            newAnnotated <- check annotated annotation

            _Θ <- get

            return (annotation, Syntax.Annotation{ annotated = solveSyntax _Θ newAnnotated, .. })

        Syntax.Let{..} -> do
            b <- fresh

            push (Context.UnsolvedType b)

            let cons Syntax.Binding{ nameLocation = nameLocation₀, nameBindings = nameBindings₀, ..} action newBindings = do
                    let annotatedAssignment = case annotation of
                            Nothing -> assignment
                            Just _A₀ -> Syntax.Annotation
                                { annotated = assignment
                                , annotation = _A₀
                                , location = nameLocation₀
                                }

                    let toLambda (nameBinding : nameBindings) =
                            Syntax.Lambda{ location = nameLocation₀, body = toLambda nameBindings, .. }
                        toLambda [] =
                            annotatedAssignment

                    (_A, newAssignment) <- infer (toLambda nameBindings₀)

                    let newBinding = Syntax.Binding
                            { annotation = Nothing
                            , nameBindings = []
                            , assignment = newAssignment
                            , nameLocation = nameLocation₀
                            , ..
                            }

                    scoped (Context.Annotation name _A) do
                        action (newBinding : newBindings)

            let nil newBindings = do
                    let output = Type.UnsolvedType{ location = Syntax.location body, existential = b }

                    newBody <- check body output

                    _Γ <- get

                    return (output, solveSyntax _Γ Syntax.Let{ bindings = NonEmpty.fromList (reverse newBindings), body = newBody, .. })

            foldr cons nil bindings []

        Syntax.List{..} -> do
            case Seq.viewl elements of
                EmptyL -> do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    return (Type.List{ type_ = Type.UnsolvedType{..}, .. }, Syntax.List{ elements = Seq.empty, .. })
                y :< ys -> do
                    (type_, newY) <- infer y

                    let process element = do
                            _Γ <- get

                            check element (Context.solveType _Γ type_)

                    newYs <- traverse process ys

                    _Γ <- get

                    return (Type.List{..}, Syntax.List{ elements = fmap (solveSyntax _Γ) (newY <| newYs), .. })

        Syntax.Record{..} -> do
            let process (field, value) = do
                    (type_, newValue) <- infer value

                    return (field, type_, newValue)

            fieldTypeValues <- traverse process fieldValues

            let fieldTypes = do
                    (field, type_, _) <- fieldTypeValues

                    return (field, type_)

            _Γ <- get

            let newFieldValues = do
                    (field, _, newValue) <- fieldTypeValues

                    return (field, solveSyntax _Γ newValue)

            return (Type.Record{ fields = Type.Fields fieldTypes Monotype.EmptyFields, .. }, Syntax.Record{ fieldValues = newFieldValues, .. })

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
                , Syntax.Alternative{..}
                )

        Syntax.Merge{..} -> do
            p <- fresh

            push (Context.UnsolvedFields p)

            let _R = Type.Record{ location = Syntax.location handlers , fields = Type.Fields [] (Monotype.UnsolvedFields p) }

            newHandlers <- check handlers _R

            _Γ <- get

            let _R' = Context.solveType _Γ _R

            case _R' of
                Type.Record{ fields = Type.Fields (List.sortBy (Ord.comparing fst) -> [("null", nullHandlerType), ("some", someHandlerType)]) Monotype.EmptyFields } -> do
                    a <- fresh
                    b <- fresh

                    push (Context.UnsolvedType a)
                    push (Context.UnsolvedType b)

                    subtype nullHandlerType Type.UnsolvedType{ existential = b, .. }
                    subtype someHandlerType Type.Function{ input = Type.UnsolvedType{ existential = a, .. }, output = Type.UnsolvedType{ existential = b, .. }, .. }
                    return
                        (   Type.Optional
                                { type_ =
                                    Type.UnsolvedType{ existential = a, .. }
                                , ..
                                }
                        ~>  Type.UnsolvedType{  existential = b, .. }
                        , Syntax.Merge{ handlers = solveSyntax _Γ newHandlers, .. }
                        )


                Type.Record{ fields = Type.Fields keyTypes Monotype.EmptyFields } -> do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    let process (key, Type.Function{ location = _, ..}) = do
                            _ϴ <- get

                            let b' = Type.UnsolvedType{ location = Type.location output, .. }

                            subtype (Context.solveType _ϴ output) (Context.solveType _ϴ b')

                            return (key, input)
                        process (_, _A) = do
                            Exception.throwIO (MergeInvalidHandler (Type.location _A) _A)

                    keyTypes' <- traverse process keyTypes

                    _Γ <- get

                    return
                        (   Type.Union
                                { alternatives =
                                    Type.Alternatives
                                        keyTypes'
                                        Monotype.EmptyAlternatives
                                , ..
                                }
                        ~>      Type.UnsolvedType{..}
                        , Syntax.Merge{ handlers = solveSyntax _Γ newHandlers, .. }
                        )

                Type.Record{} -> do
                    Exception.throwIO (MergeConcreteRecord (Type.location _R) _R)

                _ -> do
                    Exception.throwIO (MergeRecord (Type.location _R) _R)

        Syntax.Field{..} -> do
            existential <- fresh
            p <- fresh

            push (Context.UnsolvedType existential)
            push (Context.UnsolvedFields p)

            newRecord <- check record Type.Record
                { fields =
                    Type.Fields
                        [(field, Type.UnsolvedType{..})]
                        (Monotype.UnsolvedFields p)
                , location = fieldLocation
                }

            _Γ <- get

            return (Type.UnsolvedType{ location = fieldLocation, ..}, Syntax.Field{ record = solveSyntax _Γ newRecord, ..})

        Syntax.If{..} -> do
            newPredicate <- check predicate Type.Scalar{ scalar = Monotype.Bool, .. }

            (_L₀, newIfTrue) <- infer ifTrue

            _Γ  <- get

            let _L₁ = Context.solveType _Γ _L₀

            newIfFalse <- check ifFalse _L₁

            _Γ <- get

            return (_L₁, Syntax.If{ predicate = solveSyntax _Γ newPredicate, ifTrue = solveSyntax _Γ newIfTrue, ifFalse = solveSyntax _Γ newIfFalse, .. })

        Syntax.Text{ chunks = Syntax.Chunks text₀ rest, .. } -> do
            let process (interpolation, text) = do
                    newInterpolation <- check interpolation Type.Scalar{ scalar = Monotype.Text, .. }

                    return (newInterpolation, text)

            newRest <- traverse process rest

            return (Type.Scalar{ scalar = Monotype.Text, .. }, Syntax.Text{ chunks = Syntax.Chunks text₀ newRest, .. })

        Syntax.Prompt{ arguments, .. } -> do
            newArguments <- check arguments Type.Record
                { fields =
                    Type.Fields
                        [ ("text", Type.Optional{ type_ = Type.Scalar{ scalar = Monotype.Text, .. }, .. })
                        , ("model", Type.Optional{ type_ = Type.Scalar{ scalar = Monotype.Text, .. }, .. })
                        , ("code", Type.Optional{ type_ = Type.Scalar{ scalar = Monotype.Bool, .. }, .. })
                        , ("search", Type.Optional{ type_ = Type.Scalar{ scalar = Monotype.Bool, .. }, .. })
                        ]
                        Monotype.EmptyFields
                , ..
                }

            type_ <- case schema of
                Just t -> do
                    return t
                Nothing -> do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    return Type.UnsolvedType{..}

            _Γ <- get

            return (type_, Syntax.Prompt{ arguments = solveSyntax _Γ newArguments, schema = Just type_, .. })

        -- All the type inference rules for scalars go here.  This part is
        -- pretty self-explanatory: a scalar literal returns the matching
        -- scalar type.
        Syntax.Scalar{ scalar = Syntax.Bool bool, .. } -> do
            return (Type.Scalar{ scalar = Monotype.Bool, .. }, Syntax.Scalar{ scalar = Syntax.Bool bool, .. })

        Syntax.Scalar{ scalar = Syntax.Real real, .. } -> do
            return (Type.Scalar{ scalar = Monotype.Real, .. }, Syntax.Scalar{ scalar = Syntax.Real real, .. })

        Syntax.Scalar{ scalar = Syntax.Integer integer, .. } -> do
            return (Type.Scalar{ scalar = Monotype.Integer, .. }, Syntax.Scalar{ scalar = Syntax.Integer integer, .. })

        Syntax.Scalar{ scalar = Syntax.Natural natural, .. } -> do
            return (Type.Scalar{ scalar = Monotype.Natural, .. }, Syntax.Scalar{ scalar = Syntax.Natural natural, .. })

        Syntax.Scalar{ scalar = Syntax.Null, .. } -> do
            -- NOTE: You might think that you could just infer that `null`
            -- has type `forall (a : Type) . Optional a`.  This does not work
            -- because it will lead to data structures with impredicative types
            -- if you store a `null` inside of, say, a `List`.
            existential <- fresh

            push (Context.UnsolvedType existential)

            return (Type.Optional{ type_ = Type.UnsolvedType{..}, .. }, Syntax.Scalar{ scalar = Syntax.Null, .. })

        Syntax.Operator{ operator = Syntax.And, .. } -> do
            newLeft <- check left  Type.Scalar{ scalar = Monotype.Bool, location = operatorLocation }
            newRight <- check right Type.Scalar{ scalar = Monotype.Bool, location = operatorLocation }

            _Γ <- get

            return (Type.Scalar{ scalar = Monotype.Bool, location = operatorLocation }, Syntax.Operator{ operator = Syntax.And, left = solveSyntax _Γ newLeft, right = solveSyntax _Γ newRight, .. })

        Syntax.Operator{ operator = Syntax.Or, .. } -> do
            newLeft <- check left  Type.Scalar{ scalar = Monotype.Bool, location = operatorLocation }
            newRight <- check right Type.Scalar{ scalar = Monotype.Bool, location = operatorLocation }

            _Γ <- get

            return (Type.Scalar{ scalar = Monotype.Bool, location = operatorLocation }, Syntax.Operator{ operator = Syntax.Or, left = solveSyntax _Γ newLeft, right = solveSyntax _Γ newRight, .. })

        Syntax.Operator{ operator = Syntax.Equal, .. } -> do
            (_L, newLeft) <- infer left
            (_R, newRight) <- infer right

            _ <- check left  _R
            _ <- check right _L

            _Γ <- get

            let _L' = Context.solveType _Γ _L
            let _R' = Context.solveType _Γ _R

            let newEquals = Syntax.Operator{ operator = Syntax.Equal, left = solveSyntax _Γ newLeft, right = solveSyntax _Γ newRight, .. }

            let isEquatable Type.VariableType{ } =
                    False
                isEquatable Type.UnsolvedType{ } =
                    False
                isEquatable Type.Forall{ } =
                    False
                isEquatable Type.Function{ } =
                    False
                isEquatable Type.Scalar{ } =
                     True
                isEquatable Type.Optional{ type_ } =
                    isEquatable type_
                isEquatable Type.List{ type_ } =
                    isEquatable type_
                isEquatable Type.Record{ fields = Type.Fields fieldTypes Monotype.EmptyFields } =
                    all (isEquatable . snd) fieldTypes
                isEquatable Type.Record{ } =
                    False
                isEquatable Type.Union{ alternatives = Type.Alternatives alternativeTypes Monotype.EmptyAlternatives } =
                    all (isEquatable . snd) alternativeTypes
                isEquatable Type.Union{ } =
                    False

            let bool = Type.Scalar{ scalar = Monotype.Bool, location = operatorLocation }

            if isEquatable _L' && isEquatable _R'
                then return (bool, newEquals)
                else Exception.throwIO (InvalidOperands "compare" (Syntax.location left) _L')

        Syntax.Operator{ operator = Syntax.NotEqual, .. } -> do
            (_L, newLeft) <- infer left
            (_R, newRight) <- infer right

            _ <- check left  _R
            _ <- check right _L

            _Γ <- get

            let _L' = Context.solveType _Γ _L
            let _R' = Context.solveType _Γ _R

            let newNotEquals = Syntax.Operator{ operator = Syntax.NotEqual, left = solveSyntax _Γ newLeft, right = solveSyntax _Γ newRight, .. }

            let isEquatable Type.VariableType{ } =
                    False
                isEquatable Type.UnsolvedType{ } =
                    False
                isEquatable Type.Forall{ } =
                    False
                isEquatable Type.Function{ } =
                    False
                isEquatable Type.Scalar{ } =
                     True
                isEquatable Type.Optional{ type_ } =
                    isEquatable type_
                isEquatable Type.List{ type_ } =
                    isEquatable type_
                isEquatable Type.Record{ fields = Type.Fields fieldTypes Monotype.EmptyFields } =
                    all (isEquatable . snd) fieldTypes
                isEquatable Type.Record{ } =
                    False
                isEquatable Type.Union{ alternatives = Type.Alternatives alternativeTypes Monotype.EmptyAlternatives } =
                    all (isEquatable . snd) alternativeTypes
                isEquatable Type.Union{ } =
                    False

            let bool = Type.Scalar{ scalar = Monotype.Bool, location = operatorLocation }

            if isEquatable _L' && isEquatable _R'
                then return (bool, newNotEquals)
                else Exception.throwIO (InvalidOperands "compare" (Syntax.location left) _L')

        Syntax.Operator{ operator = Syntax.LessThan, .. } -> do
            let real =
                    Type.Scalar
                        { scalar = Monotype.Real
                        , location = operatorLocation
                        }

            newLeft  <- check left  real
            newRight <- check right real

            let bool =
                    Type.Scalar
                        { scalar = Monotype.Bool
                        , location = operatorLocation
                        }

            _Γ <- get

            let newLessThan =
                    Syntax.Operator
                        { operator = Syntax.LessThan
                        , left = solveSyntax _Γ newLeft
                        , right = solveSyntax _Γ newRight
                        , ..
                        }

            return (bool, newLessThan)

        Syntax.Operator{ operator = Syntax.LessThanOrEqual, .. } -> do
            let real =
                    Type.Scalar
                        { scalar = Monotype.Real
                        , location = operatorLocation
                        }

            newLeft  <- check left  real
            newRight <- check right real

            let bool =
                    Type.Scalar
                        { scalar = Monotype.Bool
                        , location = operatorLocation
                        }

            _Γ <- get

            let newLessThanOrEqual =
                    Syntax.Operator
                        { operator = Syntax.LessThanOrEqual
                        , left = solveSyntax _Γ newLeft
                        , right = solveSyntax _Γ newRight
                        , ..
                        }

            return (bool, newLessThanOrEqual)

        Syntax.Operator{ operator = Syntax.GreaterThan, .. } -> do
            let real =
                    Type.Scalar
                        { scalar = Monotype.Real
                        , location = operatorLocation
                        }

            newLeft  <- check left  real
            newRight <- check right real

            let bool =
                    Type.Scalar
                        { scalar = Monotype.Bool
                        , location = operatorLocation
                        }

            _Γ <- get

            let newGreaterThan =
                    Syntax.Operator
                        { operator = Syntax.GreaterThan
                        , left = solveSyntax _Γ newLeft
                        , right = solveSyntax _Γ newRight
                        , ..
                        }

            return (bool, newGreaterThan)

        Syntax.Operator{ operator = Syntax.GreaterThanOrEqual, .. } -> do
            let real =
                    Type.Scalar
                        { scalar = Monotype.Real
                        , location = operatorLocation
                        }

            newLeft  <- check left  real
            newRight <- check right real

            let bool =
                    Type.Scalar
                        { scalar = Monotype.Bool
                        , location = operatorLocation
                        }

            _Γ <- get

            let newGreaterThanOrEqual =
                    Syntax.Operator
                        { operator = Syntax.GreaterThanOrEqual
                        , left = solveSyntax _Γ newLeft
                        , right = solveSyntax _Γ newRight
                        , ..
                        }

            return (bool, newGreaterThanOrEqual)

        Syntax.Operator{ operator = Syntax.Times, .. } -> do
            (_L, newLeft) <- infer left
            (_R, newRight) <- infer right

            _ <- check left  _R
            _ <- check right _L

            _Γ <- get

            let _L' = Context.solveType _Γ _L

            let newTimes = Syntax.Operator{ operator = Syntax.Times, left = solveSyntax _Γ newLeft, right = solveSyntax _Γ newRight, .. }

            case _L' of
                Type.Scalar{ scalar = Monotype.Natural } -> return (_L, newTimes)
                Type.Scalar{ scalar = Monotype.Integer } -> return (_L, newTimes)
                Type.Scalar{ scalar = Monotype.Real    } -> return (_L, newTimes)
                _ -> do
                    Exception.throwIO (InvalidOperands "multiply" (Syntax.location left) _L')

        Syntax.Operator{ operator = Syntax.Plus, .. } -> do
            (_L, newLeft) <- infer left
            (_R, newRight)  <- infer right

            _ <- check left  _R
            _ <- check right _L

            _Γ <- get

            let _L' = Context.solveType _Γ _L

            let newPlus = Syntax.Operator{ operator = Syntax.Plus, left = solveSyntax _Γ newLeft, right = solveSyntax _Γ newRight, .. }

            case _L' of
                Type.Scalar{ scalar = Monotype.Natural } -> return (_L, newPlus)
                Type.Scalar{ scalar = Monotype.Integer } -> return (_L, newPlus)
                Type.Scalar{ scalar = Monotype.Real    } -> return (_L, newPlus)
                Type.Scalar{ scalar = Monotype.Text    } -> return (_L, newPlus)
                Type.List{}                              -> return (_L, newPlus)

                _ -> do
                    Exception.throwIO (InvalidOperands "add" (Syntax.location left) _L')

        Syntax.Builtin{ builtin = Syntax.Some, .. }-> do
            return
                (   Type.Forall
                        { nameLocation = Syntax.location e₀
                        , name = "a"
                        , domain = Domain.Type
                        , type_ = var "a" ~> Type.Optional{ type_ = var "a", .. }
                        , ..
                        }
                , Syntax.Builtin{ builtin = Syntax.Some, .. }
                )

        Syntax.Builtin{ builtin = Syntax.RealNegate, .. } -> do
            return
                (   Type.Scalar{ scalar = Monotype.Real, .. }
                ~>  Type.Scalar{ scalar = Monotype.Real, .. }
                , Syntax.Builtin{ builtin = Syntax.RealNegate, .. }
                )

        Syntax.Builtin{ builtin = Syntax.RealShow, .. } -> do
            return
                (   Type.Scalar{ scalar = Monotype.Real, .. }
                ~>  Type.Scalar{ scalar = Monotype.Text, .. }
                , Syntax.Builtin{ builtin = Syntax.RealShow, .. }
                )

        Syntax.Builtin{ builtin = Syntax.ListDrop, .. } -> do
            return
                ( Type.Forall
                    { nameLocation = Syntax.location e₀
                    , name = "a"
                    , domain = Domain.Type
                    , type_ =
                           Type.Scalar{ scalar = Monotype.Natural, .. }
                        ~>  (   Type.List{ type_ = var "a", .. }
                            ~>  Type.List{ type_ = var "a", .. }
                            )
                    , ..
                    }
                , Syntax.Builtin{ builtin = Syntax.ListDrop, .. }
                )

        Syntax.Builtin{ builtin = Syntax.ListHead, .. } -> do
            return
                ( Type.Forall
                    { nameLocation = Syntax.location e₀
                    , name = "a"
                    , domain = Domain.Type
                    , type_ =
                        Type.Forall
                            { nameLocation = Syntax.location e₀
                            , name = "b"
                            , domain = Domain.Alternatives
                            , type_ =
                                    Type.List { type_ = var "a", .. }
                                ~>  Type.Optional{ type_ = var "a", .. }
                            , ..
                            }
                    , ..
                    }
                , Syntax.Builtin{ builtin = Syntax.ListHead, .. }
                )

        Syntax.Builtin{ builtin = Syntax.ListFold, .. } -> do
            return
                ( Type.Forall
                    { nameLocation = Syntax.location e₀
                    , name = "a"
                    , domain = Domain.Type
                    , type_ = Type.Forall
                        { nameLocation = Syntax.location e₀
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
                , Syntax.Builtin{ builtin = Syntax.ListFold, .. }
                )


        Syntax.Builtin{ builtin = Syntax.ListIndexed, .. } -> do
            return
                ( Type.Forall
                    { nameLocation = Syntax.location e₀
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
                , Syntax.Builtin{ builtin = Syntax.ListIndexed, .. }
                )

        Syntax.Builtin{ builtin = Syntax.ListLast, .. } -> do
            return
                ( Type.Forall
                    { nameLocation = Syntax.location e₀
                    , name = "a"
                    , domain = Domain.Type
                    , type_ = Type.Forall
                        { nameLocation = Syntax.location e₀
                        , name = "b"
                        , domain  = Domain.Alternatives
                        , type_ =
                                Type.List{ type_ = var "a", .. }
                            ~>  Type.Optional{ type_ = var "a", .. }
                        , ..
                        }
                    , ..
                    }
                , Syntax.Builtin{ builtin = Syntax.ListLast, .. }
                )

        Syntax.Builtin{ builtin = Syntax.ListLength, .. } -> do
            return
                ( Type.Forall
                    { nameLocation = Syntax.location e₀
                    , name = "a"
                    , domain = Domain.Type
                    , type_ =
                            Type.List{ type_ = var "a", .. }
                        ~>  Type.Scalar{ scalar = Monotype.Natural, .. }
                    , ..
                    }
                , Syntax.Builtin{ builtin = Syntax.ListLength, .. }
                )

        Syntax.Builtin{ builtin = Syntax.ListMap, .. } -> do
            return
                ( Type.Forall
                    { nameLocation = Syntax.location e₀
                    , name = "a"
                    , domain = Domain.Type
                    , type_ = Type.Forall
                        { nameLocation = Syntax.location e₀
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
                , Syntax.Builtin{ builtin = Syntax.ListMap, .. }
                )

        Syntax.Builtin{ builtin = Syntax.IntegerAbs, .. } -> do
            return
                (   Type.Scalar{ scalar = Monotype.Integer, .. }
                ~>  Type.Scalar{ scalar = Monotype.Natural, .. }
                , Syntax.Builtin{ builtin = Syntax.IntegerAbs, .. }
                )

        Syntax.Builtin{ builtin = Syntax.IntegerEven, .. } -> do
            return
                (   Type.Scalar{ scalar = Monotype.Integer, .. }
                ~>  Type.Scalar{ scalar = Monotype.Bool, .. }
                , Syntax.Builtin{ builtin = Syntax.IntegerEven, .. }
                )

        Syntax.Builtin{ builtin = Syntax.IntegerNegate, .. } -> do
            return
                (   Type.Scalar{ scalar = Monotype.Integer, .. }
                ~>  Type.Scalar{ scalar = Monotype.Integer, .. }
                , Syntax.Builtin{ builtin = Syntax.IntegerNegate, .. }
                )

        Syntax.Builtin{ builtin = Syntax.IntegerOdd, .. } -> do
            return
                (   Type.Scalar{ scalar = Monotype.Integer, .. }
                ~>  Type.Scalar{ scalar = Monotype.Bool, .. }
                , Syntax.Builtin{ builtin = Syntax.IntegerOdd, .. }
                )

        Syntax.Builtin{ builtin = Syntax.ListReverse, .. } -> do
            return
                ( Type.Forall
                    { nameLocation = Syntax.location e₀
                    , name = "a"
                    , domain = Domain.Type
                    , type_ = Type.List{ type_ = var "a", .. } ~> Type.List{ type_ = var "a", .. }
                    , ..
                    }
                , Syntax.Builtin{ builtin = Syntax.ListReverse, .. }
                )

        Syntax.Builtin{ builtin = Syntax.ListTake, .. } -> do
            return
                ( Type.Forall
                    { nameLocation = Syntax.location e₀
                    , name = "a"
                    , domain = Domain.Type
                    , type_ =
                            Type.Scalar{ scalar = Monotype.Natural, .. }
                        ~>  (   Type.List{ type_ = var "a", .. }
                            ~>  Type.List{ type_ = var "a", .. }
                            )
                    , ..
                    }
                , Syntax.Builtin{ builtin = Syntax.ListTake, .. }
                )

        Syntax.Builtin{ builtin = Syntax.JSONFold, .. } -> do
            return
                ( Type.Forall
                    { nameLocation = Syntax.location e₀
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
                , Syntax.Builtin{ builtin = Syntax.JSONFold, .. }
                )

        Syntax.Builtin{ builtin = Syntax.NaturalFold, .. } -> do
            return
                ( Type.Forall
                    { nameLocation = Syntax.location e₀
                    , name = "a"
                    , domain = Domain.Type
                    , type_ =
                            Type.Scalar{ scalar = Monotype.Natural, .. }
                        ~>  ((var "a" ~> var "a") ~> (var "a" ~> var "a"))
                    , ..
                    }
                , Syntax.Builtin{ builtin = Syntax.NaturalFold, .. }
                )

        Syntax.Embed{..} -> do
            _Γ <- get

            Status{ manager, input, .. } <- State.get

            let absolute = input <> embedded

            Import.referentiallySane input absolute

            State.put Status{ manager, input = absolute, .. }

            syntax <- liftIO (Import.resolve manager absolute)

            result <- infer syntax

            State.modify (\s -> s{ Grace.Infer.input = input })

            return result

{-| This corresponds to the judgment:

    > Γ ⊢ e ⇐ A ⊣ Δ

    … which checks that e has type A under input context Γ, producing an updated
    context Δ.
-}
check
    :: (MonadState Status m, MonadCatch m, MonadIO m)
    => Syntax Location Input
    -> Type Location
    -> m (Syntax Location Void)
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
check Syntax.Lambda{ nameBinding = nameBinding@Syntax.NameBinding{..}, ..} Type.Function{ location = _, ..} = do
    scoped (Context.Annotation name input) do
        newBody <- check body output

        return Syntax.Lambda{ body = newBody, .. }

-- ∀I
check e Type.Forall{..} = do
    scoped (Context.Variable domain name) do
        check e type_

check Syntax.Scalar{ scalar = Syntax.Null, .. } Type.Optional{ } = do
    return Syntax.Scalar{ scalar = Syntax.Null, .. }

check Syntax.Application{ function = Syntax.Builtin{ builtin = Syntax.Some }, argument = e } Type.Optional{ type_ } = do
    check e type_

check e _B@Type.Optional{ type_ } = do
    let normal = do
            (_A₀, newE) <- infer e

            _Θ <- get

            subtype (Context.solveType _Θ _A₀) (Context.solveType _Θ _B)

            return newE

    normal `Exception.catch` \(typeInferenceError :: TypeInferenceError) -> do
        newE <- check e type_ `Exception.catch` \(_ :: TypeInferenceError) -> do
            Exception.throwIO typeInferenceError

        let location = Syntax.location e

        return Syntax.Application{ function = Syntax.Builtin{ builtin = Syntax.Some, .. }, argument = newE, .. }

check Syntax.Operator{ operator = Syntax.Times, .. } _B@Type.Scalar{ scalar }
    | scalar `elem` ([ Monotype.Natural, Monotype.Integer, Monotype.Real ] :: [Monotype.Scalar])= do
    newLeft <- check left _B

    _Γ <- get

    newRight <- check right (Context.solveType _Γ _B)

    return Syntax.Operator{ operator = Syntax.Times, left = newLeft, right = newRight, .. }

check Syntax.Operator{ operator = Syntax.Plus, .. } _B@Type.Scalar{ scalar }
    | scalar `elem` ([ Monotype.Natural, Monotype.Integer, Monotype.Real, Monotype.Text ] :: [Monotype.Scalar]) = do
    newLeft <- check left _B

    _Γ <- get

    newRight <- check right (Context.solveType _Γ _B)

    return Syntax.Operator{ operator = Syntax.Plus, left = newLeft, right = newRight, .. }

check Syntax.Operator{ operator = Syntax.Plus, .. } _B@Type.List{} = do
    newLeft <- check left _B

    _Γ <- get

    newRight <- check right (Context.solveType _Γ _B)

    return Syntax.Operator{ operator = Syntax.Plus, left = newLeft, right = newRight, .. }

check Syntax.List{..} Type.List{ location = _, .. } = do
    let process element = do
            _Γ <- get

            check element (Context.solveType _Γ type_)

    newElements <- traverse process elements

    return Syntax.List{ elements = newElements, .. }

check e@Syntax.Record{ fieldValues } _B@Type.Record{ fields = Type.Fields fieldTypes fields }
    | let mapValues = Map.fromList fieldValues
    , let mapTypes  = Map.fromList fieldTypes

    , let extraValues = Map.difference mapValues mapTypes
    , let extraTypes  = Map.difference mapTypes  mapValues

    , let both = Map.intersectionWith (,) mapValues mapTypes
    , not (Map.null both) = do
        let process (field, (value, type_)) = do
                _Γ <- get

                newValue <- check value (Context.solveType _Γ type_)

                return (field, newValue)

        newFieldValues₀ <- traverse process (Map.toList both)

        let e' = Syntax.Record
                { fieldValues = Map.toList extraValues
                , location = Syntax.location e
                }

        let _B' = Type.Record
                { location = Type.location _B
                , fields = Type.Fields (Map.toList extraTypes) fields
                }

        _Γ <- get

        result <- check e' (Context.solveType _Γ _B')

        case result of
            Syntax.Record{ fieldValues = newFieldValues₁, .. } ->
                return Syntax.Record{ fieldValues = newFieldValues₀ <> newFieldValues₁, .. }
            other ->
                return other

check Syntax.Record{..} _B@Type.Scalar{ scalar = Monotype.JSON } = do
    let process (field, value) = do
            _Γ <- get

            newValue <- check value (Context.solveType _Γ _B)

            return (field, newValue)

    newFieldValues <- traverse process fieldValues

    return Syntax.Record{ fieldValues = newFieldValues, .. }
check Syntax.List{..} _B@Type.Scalar{ scalar = Monotype.JSON } = do
    newElements <- traverse (`check` _B) elements

    return Syntax.List{ elements = newElements, .. }

check Syntax.Text{ chunks = Syntax.Chunks text₀ rest, .. } Type.Scalar{ scalar = Monotype.Text } = do
    let process (interpolation, text) = do
            newInterpolation <- check interpolation Type.Scalar{ scalar = Monotype.Text, .. }

            return (newInterpolation, text)

    newRest <- traverse process rest

    return Syntax.Text{ chunks = Syntax.Chunks text₀ newRest, .. }
check e@Syntax.Text{ } Type.Scalar{ scalar = Monotype.JSON, .. } = do
    check e Type.Scalar{ scalar = Monotype.Text, .. }

check Syntax.Prompt{..} _B = do
    newArguments <- check arguments Type.Record
        { fields =
            Type.Fields
                [ ("text", Type.Optional{ type_ = Type.Scalar{ scalar = Monotype.Text, .. }, .. })
                , ("model", Type.Optional{ type_ = Type.Scalar{ scalar = Monotype.Text, .. }, .. })
                , ("code", Type.Optional{ type_ = Type.Scalar{ scalar = Monotype.Bool, .. }, .. })
                , ("search", Type.Optional{ type_ = Type.Scalar{ scalar = Monotype.Bool, .. }, .. })
                ]
                Monotype.EmptyFields
        , ..
        }

    return Syntax.Prompt{ arguments = newArguments, schema = Just _B, .. }

check Syntax.Scalar{ scalar = Syntax.Natural natural, ..} Type.Scalar{ scalar = Monotype.JSON } = do
    return Syntax.Scalar{ scalar = Syntax.Natural natural, .. }
check Syntax.Scalar{ scalar = Syntax.Integer integer, .. } Type.Scalar{ scalar = Monotype.JSON } = do
    return Syntax.Scalar{ scalar = Syntax.Integer integer, .. }
check Syntax.Scalar{ scalar = Syntax.Real real, .. } Type.Scalar{ scalar = Monotype.JSON } = do
    return Syntax.Scalar{ scalar = Syntax.Real real, .. }
check Syntax.Scalar{ scalar = Syntax.Bool bool, .. } Type.Scalar{ scalar = Monotype.JSON } = do
    return Syntax.Scalar{ scalar = Syntax.Bool bool, .. }
check Syntax.Scalar{ scalar = Syntax.Null, .. } Type.Scalar{ scalar = Monotype.JSON } = do
    return Syntax.Scalar{ scalar = Syntax.Null, .. }

check Syntax.Embed{ embedded } _B = do
    _Γ <- get

    Status{ manager, input, .. } <- State.get

    let absolute = input <> embedded

    Import.referentiallySane input absolute

    State.put Status{ manager, input = absolute, .. }

    syntax <- liftIO (Import.resolve manager absolute)

    result <- check syntax _B

    State.modify (\s -> s{ Grace.Infer.input = input })

    return result

-- Sub
check e _B = do
    (_A, newE) <- infer e

    _Θ <- get

    subtype (Context.solveType _Θ _A) (Context.solveType _Θ _B)

    return newE

{-| This corresponds to the judgment:

    > Γ ⊢ A • e ⇒⇒ C ⊣ Δ

    … which infers the result type C when a function of type A is applied to an
    input argument e, under input context Γ, producing an updated context Δ.
-}
inferApplication
    :: (MonadState Status m, MonadCatch m, MonadIO m)
    => Type Location
    -> Syntax Location Input
    -> m (Type Location, Syntax Location Void)
-- ∀App
inferApplication Type.Forall{ domain = Domain.Type, .. } e = do
    a <- fresh

    push (Context.UnsolvedType a)

    let a' = Type.UnsolvedType{ location = nameLocation, existential = a}

    inferApplication (Type.substituteType name 0 a' type_) e
inferApplication Type.Forall{ domain = Domain.Fields, .. } e = do
    a <- fresh

    push (Context.UnsolvedFields a)

    let a' = Type.Fields [] (Monotype.UnsolvedFields a)

    inferApplication (Type.substituteFields name 0 a' type_) e
inferApplication Type.Forall{ domain = Domain.Alternatives, .. } e = do
    a <- fresh

    push (Context.UnsolvedAlternatives a)

    let a' = Type.Alternatives [] (Monotype.UnsolvedAlternatives a)

    inferApplication (Type.substituteAlternatives name 0 a' type_) e

-- αApp
inferApplication Type.UnsolvedType{ existential = a, .. } e = do
    _Γ <- get

    (_Γ', _Γ) <- Context.splitOnUnsolvedType a _Γ `orDie` MissingVariable a _Γ

    a₁ <- fresh
    a₂ <- fresh

    set (_Γ' <> (Context.SolvedType a (Monotype.Function (Monotype.UnsolvedType a₁) (Monotype.UnsolvedType a₂)) : Context.UnsolvedType a₁ : Context.UnsolvedType a₂ : _Γ))

    newE <- check e Type.UnsolvedType{ existential = a₁, .. }

    return (Type.UnsolvedType{ existential = a₂, .. }, newE)
inferApplication Type.Function{..} e = do
    newE <- check e input

    return (output, newE)
inferApplication Type.VariableType{..} _ = do
    Exception.throwIO (NotNecessarilyFunctionType location name)
inferApplication _A _ = do
    Exception.throwIO (NotFunctionType (location _A) _A)

-- | Infer the `Type` of the given `Syntax` tree
typeOf
    :: (MonadCatch m, MonadIO m)
    => Input
    -> Manager
    -> Syntax Location Input
    -> m (Type Location, Syntax Location Void)
typeOf input manager = typeWith input manager []

-- | Like `typeOf`, but accepts a custom type-checking `Context`
typeWith
    :: (MonadCatch m, MonadIO m)
    => Input
    -> Manager
    -> Context Location
    -> Syntax Location Input
    -> m (Type Location, Syntax Location Void)
typeWith input manager context syntax = do
    let initialStatus = Status{ count = 0, context, manager, input }

    ((_A, elaborated), Status{ context = _Δ }) <- State.runStateT (infer syntax) initialStatus

    return (Context.complete _Δ _A, Lens.transform (Lens.over Syntax.types (Context.complete _Δ)) elaborated)

solveSyntax :: Context s -> Syntax s a -> Syntax s a
solveSyntax _Γ = Lens.transform (Lens.over Syntax.types (Context.solveType _Γ))

-- | A data type holding all errors related to type inference
data TypeInferenceError
    = IllFormedAlternatives Location (Existential Monotype.Union) (Context Location)
    | IllFormedFields Location (Existential Monotype.Record) (Context Location)
    | IllFormedType Location (Type Location) (Context Location)
    --
    | InvalidOperands Text Location (Type Location)
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
    | NotSubtype Location (Type Location) Location (Type Location)
    --
    | UnboundAlternatives Location Text
    | UnboundFields Location Text
    | UnboundTypeVariable Location Text
    | UnboundVariable Location Text Int
    --
    | RecordTypeMismatch (Type Location) (Type Location) [Text]
    | UnionTypeMismatch (Type Location) (Type Location) [Text]
    deriving (Eq, Show)

instance Exception TypeInferenceError where
    displayException (IllFormedAlternatives location a₀ _Γ) =
        "Internal error: Invalid context\n\
        \\n\
        \The following unsolved alternatives variable:\n\
        \\n\
        \" <> insert (Context.UnsolvedAlternatives a₀) <> "\n\
        \\n\
        \… is not well-formed within the following context:\n\
        \\n\
        \#{listToText _Γ}\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location)

    displayException (IllFormedFields location a₀ _Γ) =
        "Internal error: Invalid context\n\
        \\n\
        \The following unsolved fields variable:\n\
        \\n\
        \" <> insert (Context.UnsolvedFields a₀) <> "\n\
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

    displayException (InvalidOperands action location _L') =
        "Invalid operands\n\
        \\n\
        \You cannot " <> Text.unpack action <> " values of type:\n\
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

    displayException (MissingAllAlternatives p₀ _Γ) =
        "Internal error: Invalid context\n\
        \\n\
        \The following unsolved alternatives variable:\n\
        \\n\
        \" <> insert (Context.UnsolvedAlternatives p₀) <> "\n\
        \\n\
        \… cannot be instantiated because the alternatives variable is missing from the\n\
        \context:\n\
        \\n\
        \" <> listToText _Γ

    displayException (MissingAllFields p₀ _Γ) =
        "Internal error: Invalid context\n\
        \\n\
        \The following unsolved fields variable:\n\
        \\n\
        \" <> insert (Context.UnsolvedFields p₀) <> "\n\
        \\n\
        \… cannot be instantiated because the fields variable is missing from the\n\
        \context:\n\
        \\n\
        \" <> listToText _Γ

    displayException (MissingOneOfAlternatives locations p₀ p₁ _Γ) =
        "Internal error: Invalid context\n\
        \\n\
        \One of the following alternatives variables:\n\
        \\n\
        \" <> listToText [Context.UnsolvedAlternatives p₀, Context.UnsolvedAlternatives p₁ ] <> "\n\
        \\n\
        \… is missing from the following context:\n\
        \\n\
        \" <> listToText _Γ <> "\n\
        \\n\
        \" <> locations'
        where
            locations' =
                Text.unpack (Text.unlines (map (Location.renderError "") locations))

    displayException (MissingOneOfFields locations p₀ p₁ _Γ) =
        "Internal error: Invalid context\n\
        \\n\
        \One of the following fields variables:\\n\
        \\n\
        \" <> listToText [Context.UnsolvedFields p₀, Context.UnsolvedFields p₁ ] <> "\n\
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

    displayException (NotAlternativesSubtype location p₀ alternatives) =
        "Not an alternatives subtype\n\
        \\n\
        \The following alternatives variable:\n\
        \\n\
        \" <> insert p₀ <> "\n\
        \\n\
        \… cannot be instantiated to the following union type:\n\
        \\n\
        \" <> insert (Type.Union location alternatives) <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location) <> "\n\
        \\n\
        \… because the same alternatives variable appears within that union type."

    displayException (NotFieldsSubtype location p₀ fields) =
        "Not a fields subtype\n\
        \\n\
        \The following fields variable:\n\
        \\n\
        \" <> insert p₀ <> "\n\
        \\n\
        \… cannot be instantiated to the following record type:\n\
        \\n\
        \" <> insert (Type.Record location fields) <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location) <> "\n\
        \\n\
        \… because the same fields variable appears within that record type."

    displayException (NotSubtype locA₀ _A locB₀ _B) =
        "Not a subtype\n\
        \\n\
        \The following type:\n\
        \\n\
        \" <> insert _A <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" locA₀) <> "\n\
        \\n\
        \… cannot be a subtype of:\n\
        \\n\
        \" <> insert _B <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" locB₀)

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
            var = Grace.Pretty.toSmart @(Syntax.Syntax () Void) Syntax.Variable{ location = (), .. }

    displayException (RecordTypeMismatch _A₀ _B₀ extraB) | null extraB =
        "Record type mismatch\n\
        \\n\
        \The following record type:\n\
        \\n\
        \" <> insert _A₀ <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _A₀)) <> "\n\
        \\n\
        \… is not a subtype of the following record type:\n\
        \\n\
        \" <> insert _B₀ <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _B₀))

    displayException (RecordTypeMismatch _A₀ _B₀ extraB) =
        "Record type mismatch\n\
        \\n\
        \The following record type:\n\
        \\n\
        \" <> insert _A₀ <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _A₀)) <> "\n\
        \\n\
        \… is not a subtype of the following record type:\n\
        \\n\
        \" <> insert _B₀ <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _B₀)) <> "\n\
        \\n\
        \The latter record has the following extra fields:\n\
        \\n\
        \" <> listToText extraB

    displayException (UnionTypeMismatch _A₀ _B₀ extraA) | null extraA =
        "Union type mismatch\n\
        \\n\
        \The following union type:\n\
        \\n\
        \" <> insert _A₀ <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _A₀)) <> "\n\
        \\n\
        \… is not a subtype of the following union type:\n\
        \\n\
        \" <> insert _B₀ <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _B₀))

    displayException (UnionTypeMismatch _A₀ _B₀ extraA) =
        "Union type mismatch\n\
        \\n\
        \The following union type:\n\
        \\n\
        \" <> insert _A₀ <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _A₀)) <> "\n\
        \\n\
        \… is not a subtype of the following union type:\n\
        \\n\
        \" <> insert _B₀ <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" (Type.location _B₀)) <> "\n\
        \\n\
        \The former union has the following extra alternatives:\n\
        \\n\
        \" <> listToText extraA

-- Helper functions for displaying errors

insert :: Pretty a => a -> String
insert a = Text.unpack (Grace.Pretty.toSmart ("  " <> Pretty.align (pretty a)))

listToText :: Pretty a => [a] -> String
listToText elements =
    Text.unpack (Text.intercalate "\n" (map prettyEntry elements))
  where
    prettyEntry entry =
        Grace.Pretty.toSmart ("• " <> Pretty.align (pretty entry))
