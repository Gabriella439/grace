{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

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

      -- * Types
    , HTTP(..)

      -- * Errors related to type inference
    , TypeInferenceError(..)
    ) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (Exception(..), MonadCatch, MonadThrow)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Strict (MonadState)
import Data.Foldable (for_, traverse_)
import Data.Sequence (ViewL(..), (<|))
import Data.Text (Text)
import Data.Void (Void)
import Grace.Context (Context, Entry)
import Grace.Decode (FromGrace(..))
import Grace.Existential (Existential)
import Grace.HTTP (HTTP(..))
import Grace.Input (Input)
import Grace.Location (Location(..))
import Grace.Monotype (Monotype)
import Grace.Pretty (Pretty(..))
import Grace.Prompt (Prompt(..))
import Grace.Syntax (Syntax)
import Grace.Type (Type(..))

import qualified Control.Exception.Safe as Exception
import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import qualified Control.Monad.State as State
import qualified Data.HashMap.Strict.InsOrd as Map
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
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

        k Type.UnsolvedType{ location, existential }

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

{-| @wellFormed context type@ checks that all type/fields/alternatives
    variables within @type@ are declared within the @context@
-}
wellFormed :: MonadThrow m => Context Location -> Type Location -> m ()
wellFormed context Type.VariableType{ location, name }
    | Context.Variable Domain.Type name `elem` context = do
        return ()
    | otherwise = do
        Exception.throwIO (UnboundTypeVariable location name)

wellFormed context Type.Function{ input, output } = do
    wellFormed context input
    wellFormed context output

wellFormed context Type.Forall{ name, domain, type_ } = do
    wellFormed (Context.Variable domain name : context) type_

wellFormed context _A@Type.UnsolvedType{ location, existential }
    | all mismatch context = do
        Exception.throwIO (IllFormedType location _A context)
    | otherwise = do
        return ()
  where
    mismatch (Context.UnsolvedType a  ) = existential /= a
    mismatch (Context.SolvedType   a _) = existential /= a
    mismatch  _                         = True

wellFormed context Type.Optional{ type_ } = do
    wellFormed context type_

wellFormed context Type.List{ type_ } = do
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

-- A field is required if and only if it is a subtype of @Optional T@ for some
-- type @T@
--
-- The reason we don't just check if @superType₁@ is @Optional { }@ is because
-- it might be an unsolved variable.  Checking if it is a subtype of
-- @Optional a?@ is the most robust way to check.
isFieldRequired
    :: (MonadState Status m, MonadCatch m) => Type Location -> m Bool
isFieldRequired fieldType = do
    context <- get

    let assertOptional = do
            existential <- fresh

            push (Context.UnsolvedType existential)

            let optional = Type.Optional{ location, type_ }
                  where
                    location = Type.location fieldType

                    type_ = Type.UnsolvedType
                        { existential
                        , location
                        }

            subtype fieldType optional

            return False

    assertOptional `Exception.catch` \(_ :: TypeInferenceError) -> do
        set context

        return True

-- | @subtype sub super@ checks that @sub@ is a subtype of @super@
subtype
    :: (MonadState Status m, MonadCatch m)
    => Type Location -> Type Location -> m ()
subtype subType₀ superType₀ = do
    context₀ <- get

    case (subType₀, superType₀) of
        (Type.VariableType{ name = subName }, Type.VariableType{ name = superName })
            | subName == superName -> do
                wellFormed context₀ subType₀

        (Type.UnsolvedType{ existential = subExistential }, Type.UnsolvedType{ existential = superExistential })
            | subExistential == superExistential
            , Context.UnsolvedType subExistential `elem` context₀ -> do
                return ()

        (Type.UnsolvedType{ existential = subExistential }, _)
            -- The @not (subExistential `Type.typeFreeIn` superType₀)@ is the
            -- "occurs check" which prevents a type variable from being defined
            -- in terms of itself (i.e. a type should not "occur" within
            -- itself).
            --
            -- Later on you'll see matching "occurs checks" for record types
            -- and union types so that Fields variables and Alternatives
            -- variables cannot refer to the record or union that they belong
            -- to, respectively.
            | not (subExistential `Type.typeFreeIn` superType₀)
            , elem (Context.UnsolvedType subExistential) context₀ -> do
                instantiateTypeL subExistential superType₀

        (_, Type.UnsolvedType{ existential = superExistential })
            | not (superExistential `Type.typeFreeIn` subType₀)
            , elem (Context.UnsolvedType superExistential) context₀ -> do
                instantiateTypeR subType₀ superExistential

        (Type.Function{ input = subInput, output = subOutput }, Type.Function{ input = superInput, output = superOutput }) -> do
            subtype superInput subInput

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
            --     context <- get
            --     let subType₀'   = Context.solveType context subType₀
            --     let superType₀' = Context.solveType context superType₀
            --
            -- … and then use subType₀' and superType₀' for downstream steps.
            -- If we did that at the beginning of each function then everything
            -- would "just work".
            --
            -- However, this would be more inefficient because we'd calling
            -- `solveType` wastefully over and over with the exact same context
            -- in many cases.  So, the tradeoff here is that we get improved
            -- performance if we're willing to remember to call `solveType` in
            -- the right places.
            _Θ <- get

            subtype (Context.solveType _Θ subOutput) (Context.solveType _Θ superOutput)

        (_, Type.Forall{ name, domain, type_ }) -> do
            scoped (Context.Variable domain name) do
                subtype subType₀ type_

        (Type.Forall{ nameLocation, name, domain = Domain.Type, type_ }, _) -> do
            scopedUnsolvedType nameLocation \unsolved -> do
                subtype (Type.substituteType name 0 unsolved type_) superType₀

        (Type.Forall{ name, domain = Domain.Fields, type_ }, _) -> do
            scopedUnsolvedFields \unsolved -> do
                subtype (Type.substituteFields name 0 unsolved type_) superType₀

        (Type.Forall{ name, domain = Domain.Alternatives, type_ }, _) -> do
            scopedUnsolvedAlternatives \unsolved -> do
                subtype (Type.substituteAlternatives name 0 unsolved type_) superType₀

        (Type.Scalar{ scalar = subScalar }, Type.Scalar{ scalar = superScalar })
            | subScalar == superScalar -> do
                return ()

        (Type.Optional{ type_ = subType₁ }, Type.Optional{ type_ = superType₁ }) -> do
            subtype subType₁ superType₁

        (Type.List{ type_ = subType₁ }, Type.List{ type_ = superType₁ }) -> do
            subtype subType₁ superType₁

        (Type.Scalar{ }, Type.Scalar{ scalar = Monotype.JSON }) -> do
            return ()

        (Type.List{ type_ = subType₁ }, Type.Scalar{ scalar = Monotype.JSON }) -> do
            subtype subType₁ superType₀

        (Type.Optional{ type_ = subType₁ }, Type.Scalar{ scalar = Monotype.JSON }) -> do
            subtype subType₁ superType₀

        (Type.Record{ fields = Type.Fields fieldTypes Monotype.EmptyFields }, Type.Scalar{ scalar = Monotype.JSON }) -> do
            for_ fieldTypes \(_, type_) -> do
                context <- get

                subtype type_ (Context.solveType context superType₀)

        (Type.Record{ fields = Type.Fields fieldTypes (Monotype.UnsolvedFields existential) }, Type.Scalar{ scalar = Monotype.JSON }) -> do
            instantiateFieldsL existential (Type.location superType₀) (Type.Fields [] Monotype.EmptyFields)

            for_ fieldTypes \(_, type_) -> do
                context <- get

                subtype type_ (Context.solveType context superType₀)

        (Type.Record{ fields = Type.Fields subFieldTypesList subRemainingFields }, Type.Record{ fields = Type.Fields superFieldTypesList superRemainingFields }) -> do
            let subFieldTypes   = Map.fromList subFieldTypesList
            let superFieldTypes = Map.fromList superFieldTypesList

            let subExtraFieldTypes   = Map.difference subFieldTypes   superFieldTypes
            let superExtraFieldTypes = Map.difference superFieldTypes subFieldTypes

            -- All fields in the record subtype must be subtypes of any
            -- matching fields in the record supertype
            let subtypeField subType₁ superType₁ = do
                    context <- get

                    subtype
                        (Context.solveType context subType₁)
                        (Context.solveType context superType₁)

            sequence_ (Map.intersectionWith subtypeField subFieldTypes superFieldTypes)

            let getRequiredFields = do
                    m <- traverse isFieldRequired superExtraFieldTypes

                    return (Map.keys (Map.filter id m))

            -- Here is where we handle extra fields that were only present in
            -- the subtype or supertype.  They still might be okay if one or
            -- both record types has an unsolved fields variable or if extra
            -- fields in the supertype are `Optional`
            case (subRemainingFields, superRemainingFields) of
                _   | subRemainingFields == superRemainingFields -> do
                        requiredB <- getRequiredFields

                        Monad.unless (null requiredB) do
                            Exception.throwIO (RecordTypeMismatch subType₀ superType₀ requiredB)

                -- Both records type have unsolved Fields variables.  Great!
                -- This is the most flexible case, since we can replace these
                -- unsolved variables with whatever fields we want to make the
                -- record types line up.
                --
                -- However, it's not as simple as setting each Fields variable
                -- to the extra fields from the opposing record type.  For
                -- example, if the two record types we're comparing are:
                --
                -- > { x: Bool, p₀ } <: { y: Text, p₁ }
                --
                -- … then it's not correct to say:
                --
                -- > p₀ = y: Text
                -- > p₁ = x: Bool
                --
                -- … because that is not the most general solution for @p₀@ and
                -- @p₁@!  The actual most general solution is:
                --
                --     p₀ = y: Text, p₂
                --     p₁ = x: Bool, p₂
                --
                -- … where @p₂@ is a fresh Fields type variable representing the
                -- fact that both records could potentially have even more
                -- fields other than @x@ and @y@.
                (Monotype.UnsolvedFields p₀, Monotype.UnsolvedFields p₁) -> do
                    p₂ <- fresh

                    context₁ <- get

                    -- We have to insert p₂ before both p₀ and p₁ within the
                    -- context because the bidirectional type-checking algorithm
                    -- requires that the context is ordered and all variables
                    -- within the context can only reference prior variables
                    -- within the context.
                    --
                    -- Since @p₀@ and @p₁@ both have to reference @p₂@, then we
                    -- need to insert @p₂@ right before @p₀@ or @p₁@, whichever
                    -- one comes first
                    let p₀First = do
                            (contextAfter, contextBefore) <- Context.splitOnUnsolvedFields p₀ context₁

                            Monad.guard (Context.UnsolvedFields p₁ `elem` contextAfter)

                            let command =
                                    set (   contextAfter
                                        <>  ( Context.UnsolvedFields p₀
                                            : Context.UnsolvedFields p₂
                                            : contextBefore
                                            )
                                        )

                            return command

                    let p₁First = do
                            (contextAfter, contextBefore) <- Context.splitOnUnsolvedFields p₁ context₁

                            Monad.guard (Context.UnsolvedFields p₀ `elem` contextAfter)

                            let command =
                                    set (   contextAfter
                                        <>  ( Context.UnsolvedFields p₁
                                            : Context.UnsolvedFields p₂
                                            : contextBefore
                                            )
                                        )

                            return command

                    case p₀First <|> p₁First of
                        Nothing -> do
                            Exception.throwIO (MissingOneOfFields [Type.location subType₀, Type.location superType₀] p₀ p₁ context₁)

                        Just setContext -> do
                            setContext

                    context₂ <- get

                    -- Now we solve for @p₀@.  This is basically saying:
                    --
                    -- > p₀ = extraFieldsFromRecordB, p₂
                    instantiateFieldsL
                        p₀
                        (Type.location superType₀)
                        (Context.solveRecord context₂
                            (Type.Fields (Map.toList superExtraFieldTypes)
                                (Monotype.UnsolvedFields p₂)
                            )
                        )

                    context₃ <- get

                    -- Similarly, solve for @p₁@.  This is basically saying:
                    --
                    -- > p₁ = extraFieldsFromRecordA, p₂
                    instantiateFieldsR
                        (Type.location subType₀)
                        (Context.solveRecord context₃
                            (Type.Fields (Map.toList subExtraFieldTypes)
                                (Monotype.UnsolvedFields p₂)
                            )
                        )
                        p₁

                -- If only the record subtype has a Fields variable then the
                -- solution is simpler: just set the Fields variable to the
                -- extra fields from the opposing record.
                --
                -- Carefully note that it's okay if the record supertype has
                -- extra required fields.  A record with fewer fields can be
                -- a subtype of a record with a greater number o fields.
                (Monotype.UnsolvedFields p₀, _) -> do
                    context₁ <- get

                    instantiateFieldsL
                        p₀
                        (Type.location superType₀)
                        (Context.solveRecord context₁
                            (Type.Fields (Map.toList superExtraFieldTypes) superRemainingFields)
                        )

                -- If only the record supertype has a Fields variable then
                -- things are slightly trickier because we *don't* allow the
                -- record subtype to have extra required fields.
                (_, Monotype.UnsolvedFields p₁) -> do
                    requiredFields <- getRequiredFields

                    Monad.unless (null requiredFields) do
                        Exception.throwIO (RecordTypeMismatch subType₀ superType₀ requiredFields)

                    context₁ <- get

                    instantiateFieldsR
                        (Type.location subType₀)
                        (Context.solveRecord context₁
                            (Type.Fields (Map.toList subExtraFieldTypes) subRemainingFields)
                        )
                        p₁

                _   | otherwise -> do
                        requiredB <- getRequiredFields

                        Exception.throwIO (RecordTypeMismatch subType₀ superType₀ requiredB)

        (_A@Type.Union{ alternatives = Type.Alternatives subAlternativeTypesList subRemainingAlternatives }, _B@Type.Union{ alternatives = Type.Alternatives superAlternativesTypesList superRemainingAlternatives }) -> do
            let subAlternativeTypes   = Map.fromList subAlternativeTypesList
            let superAlternativeTypes = Map.fromList superAlternativesTypesList

            let subExtraAlternativeTypes   = Map.difference subAlternativeTypes superAlternativeTypes
            let superExtraAlternativeTypes = Map.difference superAlternativeTypes subAlternativeTypes

            let subtypeAlternative subtype₁ supertype₁ = do
                    context <- get

                    subtype
                        (Context.solveType context subtype₁)
                        (Context.solveType context supertype₁)

            sequence_ (Map.intersectionWith subtypeAlternative subAlternativeTypes superAlternativeTypes)

            case (subRemainingAlternatives, superRemainingAlternatives) of
                _   | subRemainingAlternatives == superRemainingAlternatives && Map.null subExtraAlternativeTypes -> do
                        return ()

                (Monotype.UnsolvedAlternatives p₀, Monotype.UnsolvedAlternatives p₁) -> do
                    p₂ <- fresh

                    context₁ <- get

                    let p₀First = do
                            (contextAfter, contextBefore) <- Context.splitOnUnsolvedAlternatives p₀ context₁

                            Monad.guard (Context.UnsolvedAlternatives p₁ `elem` contextAfter)

                            let command =
                                    set (   contextAfter
                                        <>  ( Context.UnsolvedAlternatives p₀
                                            : Context.UnsolvedAlternatives p₂
                                            : contextBefore
                                            )
                                        )

                            return command

                    let p₁First = do
                            (contextAfter, contextBefore) <- Context.splitOnUnsolvedAlternatives p₁ context₁

                            Monad.guard (Context.UnsolvedAlternatives p₀ `elem` contextAfter)

                            let command =
                                    set (   contextAfter
                                        <>  ( Context.UnsolvedAlternatives p₁
                                            : Context.UnsolvedAlternatives p₂
                                            : contextBefore
                                            )
                                        )

                            return command

                    case p₀First <|> p₁First of
                        Nothing -> do
                            Exception.throwIO (MissingOneOfAlternatives [Type.location subType₀, Type.location superType₀] p₀ p₁ context₁)

                        Just setContext -> do
                            setContext

                    context₂ <- get

                    instantiateAlternativesL
                        p₀
                        (Type.location superType₀)
                        (Context.solveUnion context₂
                            (Type.Alternatives (Map.toList superExtraAlternativeTypes)
                                (Monotype.UnsolvedAlternatives p₂)
                            )
                        )

                    context₃ <- get

                    instantiateAlternativesR
                        (Type.location subType₀)
                        (Context.solveUnion context₃
                            (Type.Alternatives (Map.toList subExtraAlternativeTypes)
                                (Monotype.UnsolvedAlternatives p₂)
                            )
                        )
                        p₁

                (Monotype.UnsolvedAlternatives p₀, _)
                    | Map.null subExtraAlternativeTypes -> do
                        context₁ <- get

                        instantiateAlternativesL
                            p₀
                            (Type.location superType₀)
                            (Context.solveUnion context₁
                                (Type.Alternatives (Map.toList superExtraAlternativeTypes)
                                    superRemainingAlternatives
                                )
                            )

                (_, Monotype.UnsolvedAlternatives p₁) -> do
                    context₁ <- get

                    instantiateAlternativesR
                        (Type.location subType₀)
                        (Context.solveUnion context₁
                            (Type.Alternatives (Map.toList subExtraAlternativeTypes)
                                subRemainingAlternatives
                            )
                        )
                        p₁

                _   | otherwise -> do
                        Exception.throwIO (UnionTypeMismatch subType₀ superType₀ (Map.keys subExtraAlternativeTypes))

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
            Exception.throwIO (NotSubtype (Type.location subType₀) _A (Type.location superType₀) _B)

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
        Syntax.Variable{ location, name } -> do
            _Γ <- get

            inferred <- Context.lookup name _Γ `orDie` UnboundVariable location name

            return (inferred, Syntax.Variable{ location, name })

        -- →I⇒
        Syntax.Lambda{ location, nameBinding, body } -> do
            (input, entries, newNameBinding) <- case nameBinding of
                Syntax.NameBinding{ nameLocation, name, annotation = Nothing, assignment = Nothing } -> do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    let input = Type.UnsolvedType
                            { location = nameLocation
                            , existential
                            }

                    let entries = [Context.Annotation name input]

                    let newNameBinding = Syntax.NameBinding
                            { nameLocation
                            , name
                            , annotation = Nothing
                            , assignment = Nothing
                            }

                    return (input, entries, newNameBinding)

                Syntax.NameBinding{ nameLocation, name, annotation = Just input, assignment = Nothing } -> do
                    let entries = [Context.Annotation name input]

                    let newNameBinding = Syntax.NameBinding
                            { nameLocation
                            , name
                            , annotation = Just input
                            , assignment = Nothing
                            }

                    return (input, entries, newNameBinding)

                Syntax.NameBinding{ nameLocation, name, annotation = Nothing, assignment = Just assignment } -> do
                    (input₀, newAssignment) <- infer assignment

                    let input₁ = Type.Optional
                            { location = Syntax.location assignment
                            , type_ = input₀
                            }

                    let entries = [Context.Annotation name input₀]

                    let newNameBinding = Syntax.NameBinding
                            { nameLocation
                            , name
                            , annotation = Nothing
                            , assignment = Just newAssignment
                            }

                    return (input₁, entries, newNameBinding)

                Syntax.NameBinding{ nameLocation, name, annotation = Just input₀, assignment = Just assignment } -> do
                    newAssignment <- check assignment input₀

                    context <- get

                    let input₁ = Context.solveType context input₀

                    let input₂ = Type.Optional
                            { location = Syntax.location assignment
                            , type_ = input₁
                            }

                    let entries = [Context.Annotation name input₁]

                    let newNameBinding = Syntax.NameBinding
                            { nameLocation
                            , name
                            , annotation = Just input₁
                            , assignment = Just newAssignment
                            }

                    return (input₂, entries, newNameBinding)

                Syntax.FieldNamesBinding{ fieldNamesLocation, fieldNames } -> do
                    let process Syntax.FieldName{ fieldNameLocation, name, annotation = Nothing, assignment = Nothing } = do
                            existential <- fresh

                            push (Context.UnsolvedType existential)

                            let annotation = Type.UnsolvedType
                                    { location = fieldNameLocation
                                    , existential
                                    }

                            let fieldType = (name, annotation)

                            let entry = Context.Annotation name annotation

                            let newFieldName = Syntax.FieldName
                                    { fieldNameLocation
                                    , name
                                    , annotation = Nothing
                                    , assignment = Nothing
                                    }

                            return (fieldType, entry, newFieldName)

                        process Syntax.FieldName{ fieldNameLocation, name, annotation = Just annotation, assignment = Nothing } = do
                            let fieldType = (name, annotation)

                            let entry = Context.Annotation name annotation

                            let newFieldName = Syntax.FieldName
                                    { fieldNameLocation
                                    , name
                                    , annotation = Just annotation
                                    , assignment = Nothing
                                    }

                            return (fieldType, entry, newFieldName)

                        process Syntax.FieldName{ fieldNameLocation, name, annotation = Nothing, assignment = Just assignment } = do
                            (annotation₀, newAssignment) <- infer assignment

                            let annotation₁ = Type.Optional
                                    { location = Syntax.location assignment
                                    , type_ = annotation₀
                                    }

                            let fieldType = (name, annotation₁)

                            let entry = Context.Annotation name annotation₀

                            let newFieldName = Syntax.FieldName
                                    { fieldNameLocation
                                    , name
                                    , annotation = Nothing
                                    , assignment = Just newAssignment
                                    }

                            return (fieldType, entry, newFieldName)

                        process Syntax.FieldName{ fieldNameLocation, name, annotation = Just annotation₀, assignment = Just assignment } = do
                            let annotation₁ = Type.Optional
                                    { location = Syntax.location assignment
                                    , type_ = annotation₀
                                    }

                            let fieldType = (name, annotation₁)

                            let entry = Context.Annotation name annotation₀

                            context <- get

                            newAssignment <- check assignment (Context.solveType context annotation₀)

                            let newFieldName = Syntax.FieldName
                                    { fieldNameLocation
                                    , name
                                    , annotation = Just annotation₀
                                    , assignment = Just newAssignment
                                    }

                            return (fieldType, entry, newFieldName)

                    tuples <- traverse process fieldNames

                    let fieldTypes = do
                            (fieldType, _, _) <- tuples

                            return fieldType

                    let entries = do
                            (_, entry, _) <- tuples

                            return entry

                    let newFieldNames = do
                            (_, _, newFieldName) <- tuples

                            return newFieldName

                    existential <- fresh

                    push (Context.UnsolvedFields existential)

                    let annotation = Type.Record
                            { location = fieldNamesLocation
                            , fields =
                                Type.Fields fieldTypes (Monotype.UnsolvedFields existential)
                            }

                    let newNameBinding = Syntax.FieldNamesBinding
                            { fieldNamesLocation
                            , fieldNames = newFieldNames
                            }

                    return (annotation, entries, newNameBinding)

            output <- do
                existential <- fresh

                push (Context.UnsolvedType existential)

                return Type.UnsolvedType
                    { location = Syntax.location body
                    , existential
                    }

            let done = do
                    newBody <- check body output

                    context <- get

                    let inferred = Type.Function
                            { location
                            , input = Context.solveType context input
                            , output
                            }

                    let newLambda = Syntax.Lambda
                            { location
                            , nameBinding = newNameBinding
                            , body = newBody
                            }

                    -- TODO: Only `solveSyntax` `newNameBinding`
                    return (inferred, solveSyntax context newLambda)

            foldr scoped done entries

        Syntax.Application{ location, function, argument } -> do
            (functionType, newFunction) <- infer function

            context₀ <- get

            (inferred, newArgument) <- inferApplication (Context.solveType context₀ functionType) argument

            context₁ <- get

            let syntax = Syntax.Application
                    { location
                    , function = solveSyntax context₁ newFunction
                    , argument = newArgument
                    }

            return (inferred, syntax)

        Syntax.Annotation{ annotated = annotated₀, annotation, location } -> do
            context <- get

            wellFormed context annotation

            annotated₁ <- check annotated₀ annotation

            case annotated₁ of
                Syntax.Annotation{ annotated = annotated₂ } -> do
                    return (annotation, Syntax.Annotation{ annotated = annotated₂, annotation, location })
                _ -> do
                    return (annotation, Syntax.Annotation{ annotated = annotated₁, annotation, location })

        Syntax.Let{ location, bindings, body } -> do
            b <- fresh

            push (Context.UnsolvedType b)

            let cons Syntax.Binding{ nameLocation = nameLocation₀, name, nameBindings = nameBindings₀, annotation, assignment } action newBindings = do
                    let annotatedAssignment = case annotation of
                            Nothing ->
                                assignment
                            Just type_ ->
                                Syntax.Annotation
                                    { annotated = assignment
                                    , annotation = type_
                                    , location = nameLocation₀
                                    }

                    let toLambda (nameBinding : nameBindings) =
                            Syntax.Lambda
                                { location = nameLocation₀
                                , nameBinding
                                , body = toLambda nameBindings
                                }
                        toLambda [] =
                            annotatedAssignment

                    (assignmentType, newAssignment) <- infer (toLambda nameBindings₀)

                    let newBinding = Syntax.Binding
                            { name
                            , nameLocation = nameLocation₀
                            , nameBindings = []
                            , annotation = Nothing
                            , assignment = newAssignment
                            }

                    scoped (Context.Annotation name assignmentType) do
                        action (newBinding : newBindings)

            let nil newBindings = do
                    let output = Type.UnsolvedType
                            { location = Syntax.location body
                            , existential = b
                            }

                    newBody <- check body output

                    context <- get

                    return (output, solveSyntax context Syntax.Let{ location, bindings = NonEmpty.fromList (reverse newBindings), body = newBody })

            foldr cons nil bindings []

        Syntax.List{ location, elements = elements₀ } -> do
            case Seq.viewl elements₀ of
                EmptyL -> do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    return (Type.List{ location, type_ = Type.UnsolvedType{..} }, Syntax.List{ location, elements = Seq.empty })

                element₀ :< elements -> do
                    (type_, newElement₀) <- infer element₀

                    let process element = do
                            context <- get

                            check element (Context.solveType context type_)

                    newElements <- traverse process elements

                    context <- get

                    let inferred = Type.List{ location, type_ }

                    let newList = Syntax.List
                            { location
                            , elements =
                                fmap (solveSyntax context) (newElement₀ <| newElements)
                            }

                    return (inferred, newList)

        Syntax.Record{ location, fieldValues } -> do
            let process (field, value) = do
                    (type_, newValue) <- infer value

                    return (field, type_, newValue)

            fieldTypeValues <- traverse process fieldValues

            let fieldTypes = do
                    (field, type_, _) <- fieldTypeValues

                    return (field, type_)

            context <- get

            let newFieldValues = do
                    (field, _, newValue) <- fieldTypeValues

                    return (field, solveSyntax context newValue)

            let inferred = Type.Record
                    { location
                    , fields = Type.Fields fieldTypes Monotype.EmptyFields
                    }

            let newRecord = Syntax.Record
                    { location
                    , fieldValues = newFieldValues
                    }

            return (inferred, newRecord)

        Syntax.Alternative{ location, name } -> do
            existential  <- fresh
            alternatives <- fresh

            push (Context.UnsolvedType existential)
            push (Context.UnsolvedAlternatives alternatives)

            let unsolved = Type.UnsolvedType{ location, existential }

            let inferred =
                        unsolved
                    ~>  Type.Union
                          { location
                          , alternatives = Type.Alternatives
                              [(name, unsolved)]
                              (Monotype.UnsolvedAlternatives alternatives)
                          }

            let newAlternative = Syntax.Alternative{ location, name }

            return (inferred, newAlternative)

        Syntax.Fold{ location, handlers } -> do
            fields <- fresh

            push (Context.UnsolvedFields fields)

            let recordType = Type.Record
                    { location = Syntax.location handlers
                    , fields = Type.Fields [] (Monotype.UnsolvedFields fields)
                    }

            newHandlers <- check handlers recordType

            context <- get

            case Context.solveType context recordType  of
                Type.Record{ fields = Type.Fields (List.sortBy (Ord.comparing fst) -> [("false", falseHandler), ("true", trueHandler)]) Monotype.EmptyFields } -> do
                    let bool = falseHandler

                    subtype trueHandler bool

                    return
                        ( Type.Function
                            { location
                            , input = Type.Scalar
                                { location
                                , scalar = Monotype.Bool
                                }
                            , output = bool
                            }
                        , Syntax.Fold{ handlers = solveSyntax context newHandlers, .. }
                        )

                Type.Record{ fields = Type.Fields (List.sortBy (Ord.comparing fst) -> [("succ", succHandler), ("zero", zeroHandler)]) Monotype.EmptyFields } -> do
                    let natural = zeroHandler

                    subtype succHandler Type.Function
                        { location
                        , input = natural
                        , output = natural
                        }

                    return
                        ( Type.Function
                            { location
                            , input = Type.Scalar
                                { location
                                , scalar = Monotype.Natural
                                }
                            , output = natural
                            }
                        , Syntax.Fold{ handlers = solveSyntax context newHandlers, .. }
                        )

                Type.Record{ fields = Type.Fields (List.sortBy (Ord.comparing fst) -> [("null", nullHandler), ("some", someHandler)]) Monotype.EmptyFields } -> do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    let element = Type.UnsolvedType
                            { location = Type.location someHandler
                            , existential
                            }

                    let optional = nullHandler

                    subtype someHandler Type.Function
                        { location
                        , input = element
                        , output = optional
                        }

                    return
                        ( Type.Function
                              { location
                              , input = Type.Optional
                                  { location
                                  , type_ = element
                                  }
                              , output = optional
                              }
                        , Syntax.Fold{ handlers = solveSyntax context newHandlers, .. }
                        )

                Type.Record{ fields = Type.Fields (List.sortBy (Ord.comparing fst) -> [("cons", consHandler), ("nil", nilHandler)]) Monotype.EmptyFields } -> do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    let element = Type.UnsolvedType
                            { location = Type.location consHandler
                            , existential
                            }

                    let list = nilHandler

                    subtype consHandler Type.Function
                        { location
                        , input = element
                        , output = Type.Function
                            { location
                            , input = list
                            , output = list
                            }
                        }

                    return
                        ( Type.Function
                            { location
                            , input = Type.List
                                { location
                                , type_ = element
                                }
                            , output = list
                            }
                        , Syntax.Fold{ handlers = solveSyntax context newHandlers, .. }
                        )

                Type.Record{ fields = Type.Fields (List.sortBy (Ord.comparing fst) -> [("array", arrayHandler), ("bool", boolHandler), ("integer", integerHandler), ("natural", naturalHandler), ("null", nullHandler), ("object", objectHandler), ("real", realHandler), ("string", stringHandler)]) Monotype.EmptyFields } -> do
                    let json = nullHandler

                    subtype nullHandler json

                    subtype boolHandler Type.Function
                        { location
                        , input = Type.Scalar
                            { location
                            , scalar = Monotype.Bool
                            }
                        , output = json
                        }

                    subtype stringHandler Type.Function
                        { location
                        , input = Type.Scalar
                            { location
                            , scalar = Monotype.Text
                            }
                        , output = json
                        }

                    subtype realHandler Type.Function
                        { location
                        , input = Type.Scalar
                            { location
                            , scalar = Monotype.Real
                            }
                        , output = json
                        }

                    subtype integerHandler Type.Function
                        { location
                        , input = Type.Scalar
                            { location
                            , scalar = Monotype.Integer
                            }
                        , output = json
                        }

                    subtype naturalHandler Type.Function
                        { location
                        , input = Type.Scalar
                            { location
                            , scalar = Monotype.Natural
                            }
                        , output = json
                        }

                    subtype arrayHandler Type.Function
                        { location
                        , input = Type.List{ location, type_ = json }
                        , output = json
                        }

                    subtype objectHandler Type.Function
                        { location
                        , input = Type.List
                            { location
                            , type_ = Type.Record
                                { location
                                , fields = Type.Fields
                                    [ ("key", Type.Scalar{ location, scalar = Monotype.Text })
                                    , ("value", json)
                                    ]
                                    Monotype.EmptyFields
                                }
                            }
                        , output = json
                        }

                    return
                        ( Type.Function
                            { location
                            , input = Type.Scalar
                                { location
                                , scalar = Monotype.JSON
                                }
                            , output = json
                            }
                        , Syntax.Fold{ handlers = solveSyntax context newHandlers, .. }
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
                            Exception.throwIO (FoldInvalidHandler (Type.location _A) _A)

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
                        , Syntax.Fold{ handlers = solveSyntax _Γ newHandlers, .. }
                        )

                Type.Record{} -> do
                    Exception.throwIO (FoldConcreteRecord (Type.location recordType) recordType)

                _ -> do
                    Exception.throwIO (FoldRecord (Type.location recordType) recordType)

        Syntax.Project{ location, larger, smaller } -> do
            let processField Syntax.Field{ fieldLocation, field } = do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    return (field, Type.UnsolvedType{ location = fieldLocation, .. })

            case smaller of
                Syntax.Single{ single } -> do
                    p <- fresh

                    push (Context.UnsolvedFields p)

                    let Syntax.Field{ fieldLocation } = single

                    fieldType@(_, type_) <- processField single

                    newLarger <- check larger Type.Record
                        { fields =
                            Type.Fields [fieldType] (Monotype.UnsolvedFields p)
                        , location = fieldLocation
                        }

                    _Γ <- get

                    return (type_, Syntax.Project{ larger = solveSyntax _Γ newLarger, .. })

                Syntax.Multiple{ multipleLocation, multiple } -> do
                    p <- fresh

                    push (Context.UnsolvedFields p)

                    fieldTypes <- traverse processField multiple

                    newLarger <- check larger Type.Record
                        { fields =
                            Type.Fields fieldTypes (Monotype.UnsolvedFields p)
                        , location = multipleLocation
                        }

                    let type_ = Type.Record
                            { fields =
                                Type.Fields fieldTypes Monotype.EmptyFields
                            , location = multipleLocation
                            }

                    _Γ <- get

                    return (type_, Syntax.Project{ larger = solveSyntax _Γ newLarger, .. })

                Syntax.Index{ } -> do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    let element = Type.UnsolvedType{ location, existential }

                    let listType = Type.List{ location, type_ = element }

                    newLarger <- check larger listType

                    let optional = Type.Optional{ location, type_ = element }

                    return (optional, Syntax.Project{ location, larger = newLarger, .. })
                Syntax.Slice{ } -> do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    let element = Type.UnsolvedType{ location, existential }

                    let listType = Type.List{ location, type_ = element }

                    newLarger <- check larger listType

                    return (listType, Syntax.Project{ location, larger = newLarger, .. })

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
            newArguments <- check arguments (fmap (\_ -> location) (expected @Prompt))

            newSchema <- case schema of
                Just t -> do
                    return t
                Nothing -> do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    return Type.UnsolvedType{..}

            context <- get

            return (newSchema, Syntax.Prompt{ arguments = solveSyntax context newArguments, schema = Just newSchema, .. })

        Syntax.HTTP{ location, arguments, schema } -> do
            let input = fmap (\_ -> location) (expected @HTTP)

            newArguments <- check arguments input

            let newSchema = case schema of
                    Just output -> output
                    Nothing -> Type.Scalar{ scalar = Monotype.JSON, .. }

            context <- get

            return (newSchema, Syntax.HTTP{ arguments = solveSyntax context newArguments, schema = Just newSchema, .. })

        Syntax.Read{ location, arguments, schema } -> do
            let input = fmap (\_ -> location) (expected @Text)

            newArguments <- check arguments input

            let newSchema = case schema of
                    Just output -> output
                    Nothing -> Type.Scalar{ scalar = Monotype.JSON, .. }

            context <- get

            return (newSchema, Syntax.Read{ arguments = solveSyntax context newArguments, schema = Just newSchema, .. })

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

        Syntax.Scalar{ scalar = Syntax.Key key, .. } -> do
            return (Type.Scalar{ scalar = Monotype.Key, .. }, Syntax.Scalar{ scalar = Syntax.Key key, .. })

        Syntax.Operator{ operator = Syntax.And, .. } -> do
            let bool = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Bool
                    }

            newLeft  <- check left  bool
            newRight <- check right bool

            context₁ <- get

            let newOperator = Syntax.Operator
                    { location
                    , left = solveSyntax context₁ newLeft
                    , operatorLocation
                    , operator = Syntax.And
                    , right = solveSyntax context₁ newRight
                    }

            return (bool, newOperator)

        Syntax.Operator{ operator = Syntax.Or, .. } -> do
            let bool = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Bool
                    }

            newLeft  <- check left  bool
            newRight <- check right bool

            context₁ <- get

            let newOperator = Syntax.Operator
                    { location
                    , left = solveSyntax context₁ newLeft
                    , operatorLocation
                    , operator = Syntax.Or
                    , right = solveSyntax context₁ newRight
                    }

            return (bool, newOperator)

        Syntax.Operator{ operator = Syntax.Equal, .. } -> do
            (_L, newLeft ) <- infer left
            (_R, newRight) <- infer right

            _ <- check left  _R
            _ <- check right _L

            context₁ <- get

            let _L' = Context.solveType context₁ _L
            let _R' = Context.solveType context₁ _R

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

            let bool = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Bool
                    }

            let newOperator = Syntax.Operator
                    { location
                    , left = solveSyntax context₁ newLeft
                    , operatorLocation
                    , operator = Syntax.Equal
                    , right = solveSyntax context₁ newRight
                    }

            if isEquatable _L' && isEquatable _R'
                then return (bool, newOperator)
                else Exception.throwIO (InvalidOperands "compare" (Syntax.location left) (Syntax.location right))

        Syntax.Operator{ operator = Syntax.NotEqual, .. } -> do
            (_L, newLeft ) <- infer left
            (_R, newRight) <- infer right

            _ <- check left  _R
            _ <- check right _L

            context₁ <- get

            let _L' = Context.solveType context₁ _L
            let _R' = Context.solveType context₁ _R

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

            let bool = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Bool
                    }

            let newOperator = Syntax.Operator
                    { location
                    , left = solveSyntax context₁ newLeft
                    , operatorLocation
                    , operator = Syntax.NotEqual
                    , right = solveSyntax context₁ newRight
                    }

            if isEquatable _L' && isEquatable _R'
                then return (bool, newOperator)
                else Exception.throwIO (InvalidOperands "compare" (Syntax.location left) (Syntax.location right))

        Syntax.Operator{ operator = Syntax.LessThan, .. } -> do
            let real = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Real
                    }

            newLeft  <- check left  real
            newRight <- check right real

            let bool = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Bool
                    }

            context₁ <- get

            let newOperator = Syntax.Operator
                        { location
                        , left = solveSyntax context₁ newLeft
                        , operatorLocation
                        , operator = Syntax.LessThan
                        , right = solveSyntax context₁ newRight
                        }

            return (bool, newOperator)

        Syntax.Operator{ operator = Syntax.LessThanOrEqual, .. } -> do
            let real = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Real
                    }

            newLeft  <- check left  real
            newRight <- check right real

            let bool = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Bool
                    }

            context₁ <- get

            let newOperator = Syntax.Operator
                        { location
                        , left = solveSyntax context₁ newLeft
                        , operatorLocation
                        , operator = Syntax.LessThanOrEqual
                        , right = solveSyntax context₁ newRight
                        }

            return (bool, newOperator)

        Syntax.Operator{ operator = Syntax.GreaterThan, .. } -> do
            let real = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Real
                    }

            newLeft  <- check left  real
            newRight <- check right real

            let bool = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Bool
                    }

            context₁ <- get

            let newOperator = Syntax.Operator
                        { location
                        , left = solveSyntax context₁ newLeft
                        , operatorLocation
                        , operator = Syntax.GreaterThan
                        , right = solveSyntax context₁ newRight
                        }

            return (bool, newOperator)

        Syntax.Operator{ operator = Syntax.GreaterThanOrEqual, .. } -> do
            let real = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Real
                    }

            newLeft  <- check left  real
            newRight <- check right real

            let bool = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Bool
                    }

            context₁ <- get

            let newOperator = Syntax.Operator
                        { location
                        , left = solveSyntax context₁ newLeft
                        , operatorLocation
                        , operator = Syntax.GreaterThanOrEqual
                        , right = solveSyntax context₁ newRight
                        }

            return (bool, newOperator)

        Syntax.Operator{ operator = Syntax.Times, .. } -> do
            context₁ <- get

            let natural = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Natural
                    }

            let integer = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Integer
                    }

            let real = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Real
                    }

            let naturalArguments = do
                    newLeft  <- check left  natural
                    newRight <- check right natural

                    context₂ <- get

                    let newOperator = Syntax.Operator
                            { location
                            , left = solveSyntax context₂ newLeft
                            , operatorLocation
                            , operator = Syntax.Times
                            , right = solveSyntax context₂ newRight
                            }

                    return (natural, newOperator)

            let integerArguments = do
                    newLeft  <- check left  integer
                    newRight <- check right integer

                    context₂ <- get

                    let newOperator = Syntax.Operator
                            { location
                            , left = solveSyntax context₂ newLeft
                            , operatorLocation
                            , operator = Syntax.Times
                            , right = solveSyntax context₂ newRight
                            }

                    return (integer, newOperator)

            let realArguments = do
                    newLeft  <- check left  real
                    newRight <- check right real

                    context₂ <- get

                    let newOperator = Syntax.Operator
                            { location
                            , left = solveSyntax context₂ newLeft
                            , operatorLocation
                            , operator = Syntax.Times
                            , right = solveSyntax context₂ newRight
                            }

                    return (real, newOperator)

            naturalArguments `Exception.catch` \(_ :: TypeInferenceError) -> do
                set context₁

                integerArguments `Exception.catch` \(_ :: TypeInferenceError) -> do
                    set context₁

                    realArguments `Exception.catch` \(_ :: TypeInferenceError) -> do
                        Exception.throwIO (InvalidOperands "multiply" (Syntax.location left) (Syntax.location right))

        Syntax.Operator{ operator = Syntax.Plus, .. } -> do
            context₁ <- get

            let natural = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Natural
                    }

            let integer = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Integer
                    }

            let real = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Real
                    }

            let text = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Text
                    }

            let naturalArguments = do
                    newLeft  <- check left  natural
                    newRight <- check right natural

                    context₂ <- get

                    let newOperator = Syntax.Operator
                            { location
                            , left = solveSyntax context₂ newLeft
                            , operatorLocation
                            , operator = Syntax.Plus
                            , right = solveSyntax context₂ newRight
                            }

                    return (natural, newOperator)

            let integerArguments = do
                    newLeft  <- check left  integer
                    newRight <- check right integer

                    context₂ <- get

                    let newOperator = Syntax.Operator
                            { location
                            , left = solveSyntax context₂ newLeft
                            , operatorLocation
                            , operator = Syntax.Plus
                            , right = solveSyntax context₂ newRight
                            }

                    return (integer, newOperator)

            let realArguments = do
                    newLeft  <- check left  real
                    newRight <- check right real

                    context₂ <- get

                    let newOperator = Syntax.Operator
                            { location
                            , left = solveSyntax context₂ newLeft
                            , operatorLocation
                            , operator = Syntax.Plus
                            , right = solveSyntax context₂ newRight
                            }

                    return (real, newOperator)

            let textArguments = do
                    newLeft  <- check left  text
                    newRight <- check right text

                    context₂ <- get

                    let newOperator = Syntax.Operator
                            { location
                            , left = solveSyntax context₂ newLeft
                            , operatorLocation
                            , operator = Syntax.Plus
                            , right = solveSyntax context₂ newRight
                            }

                    return (text, newOperator)

            let listArguments = do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    let element = Type.UnsolvedType
                            { location = operatorLocation
                            , existential
                            }

                    let list = Type.List
                            { location = operatorLocation
                            , type_ = element
                            }

                    newLeft  <- check left  list
                    newRight <- check right list

                    context₂ <- get

                    let newOperator = Syntax.Operator
                            { location
                            , left = solveSyntax context₂ newLeft
                            , operatorLocation
                            , operator = Syntax.Plus
                            , right = solveSyntax context₂ newRight
                            }

                    return (list, newOperator)

            listArguments `Exception.catch` \(_ :: TypeInferenceError) -> do
                set context₁

                textArguments `Exception.catch` \(_ :: TypeInferenceError) -> do
                    set context₁

                    naturalArguments `Exception.catch` \(_ :: TypeInferenceError) -> do
                        set context₁

                        integerArguments `Exception.catch` \(_ :: TypeInferenceError) -> do
                            set context₁

                            realArguments `Exception.catch` \(_ :: TypeInferenceError) -> do
                                Exception.throwIO (InvalidOperands "add" (Syntax.location left) (Syntax.location right))

        Syntax.Operator{ operator = Syntax.Minus, .. } -> do
            context₁ <- get

            let natural = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Natural
                    }

            let integer = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Integer
                    }

            let real = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Real
                    }

            let naturalArguments = do
                    newLeft  <- check left  natural
                    newRight <- check right natural

                    context₂ <- get

                    let newOperator = Syntax.Operator
                            { location
                            , left = solveSyntax context₂ newLeft
                            , operatorLocation
                            , operator = Syntax.Minus
                            , right = solveSyntax context₂ newRight
                            }

                    return (integer, newOperator)

            let integerArguments = do
                    newLeft  <- check left  integer
                    newRight <- check right integer

                    context₂ <- get

                    let newOperator = Syntax.Operator
                            { location
                            , left = solveSyntax context₂ newLeft
                            , operatorLocation
                            , operator = Syntax.Minus
                            , right = solveSyntax context₂ newRight
                            }

                    return (integer, newOperator)

            let realArguments = do
                    newLeft  <- check left  real
                    newRight <- check right real

                    context₂ <- get

                    let newOperator = Syntax.Operator
                            { location
                            , left = solveSyntax context₂ newLeft
                            , operatorLocation
                            , operator = Syntax.Minus
                            , right = solveSyntax context₂ newRight
                            }

                    return (real, newOperator)

            naturalArguments `Exception.catch` \(_ :: TypeInferenceError) -> do
                set context₁

                integerArguments `Exception.catch` \(_ :: TypeInferenceError) -> do
                    set context₁

                    realArguments `Exception.catch` \(_ :: TypeInferenceError) -> do
                        Exception.throwIO (InvalidOperands "subtract" (Syntax.location left) (Syntax.location right))

        Syntax.Operator{ operator = Syntax.Modulus, .. } -> do
            let natural = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Natural
                    }

            let integer = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Integer
                    }

            let real = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Real
                    }

            newRight <- case right of
                Syntax.Scalar{ scalar = Syntax.Natural 0 } -> do
                    Exception.throwIO (ZeroDivisor (Syntax.location right))
                Syntax.Scalar{ scalar = Syntax.Natural n, location = l } -> do
                    return Syntax.Scalar{ scalar = Syntax.Natural n, location = l }
                _ -> do
                    Exception.throwIO (NeedConcreteDivisor (Syntax.location right))

            context₁ <- get

            let naturalArgument = do
                    newLeft <- check left natural

                    let type_ = Type.Record
                            { location = operatorLocation
                            , fields = Type.Fields
                                [ ("quotient", natural)
                                , ("remainder", natural)
                                ]
                                Monotype.EmptyFields
                            }

                    context₂ <- get

                    let newOperator = Syntax.Operator
                            { location
                            , left = solveSyntax context₂ newLeft
                            , operatorLocation
                            , operator = Syntax.Modulus
                            , right = solveSyntax context₂ newRight
                            }

                    return (type_, newOperator)

            let integerArgument = do
                    newLeft <- check left integer

                    let type_ = Type.Record
                            { location = operatorLocation
                            , fields = Type.Fields
                                [ ("quotient", integer)
                                , ("remainder", natural)
                                ]
                                Monotype.EmptyFields
                            }

                    context₂ <- get

                    let newOperator = Syntax.Operator
                            { location
                            , left = solveSyntax context₂ newLeft
                            , operatorLocation
                            , operator = Syntax.Modulus
                            , right = solveSyntax context₂ newRight
                            }

                    return (type_, newOperator)

            let realArgument = do
                    newLeft <- check left real

                    let type_ = Type.Record
                            { location = operatorLocation
                            , fields = Type.Fields
                                [ ("quotient", integer)
                                , ("remainder", real)
                                ]
                                Monotype.EmptyFields
                            }

                    context₂ <- get

                    let newOperator = Syntax.Operator
                            { location
                            , left = solveSyntax context₂ newLeft
                            , operatorLocation
                            , operator = Syntax.Modulus
                            , right = solveSyntax context₂ newRight
                            }

                    return (type_, newOperator)

            naturalArgument `Exception.catch` \(_ :: TypeInferenceError) -> do
                set context₁

                integerArgument `Exception.catch` \(_ :: TypeInferenceError) -> do
                    set context₁

                    realArgument `Exception.catch` \(_ :: TypeInferenceError) -> do
                        Exception.throwIO (InvalidOperands "divide" (Syntax.location left) (Syntax.location right))

        Syntax.Operator{ operator = Syntax.Divide, .. } -> do
            let natural = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Natural
                    }

            let integer = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Integer
                    }

            let real = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Real
                    }

            newRight <- case right of
                Syntax.Scalar{ scalar = Syntax.Natural 0 } -> do
                    Exception.throwIO (ZeroDivisor (Syntax.location right))
                Syntax.Scalar{ scalar = Syntax.Integer 0 } -> do
                    Exception.throwIO (ZeroDivisor (Syntax.location right))
                Syntax.Scalar{ scalar = Syntax.Real 0 } -> do
                    Exception.throwIO (ZeroDivisor (Syntax.location right))
                Syntax.Scalar{ scalar = Syntax.Natural n, location = l } -> do
                    return Syntax.Scalar{ scalar = Syntax.Real (fromIntegral n), location = l }
                Syntax.Scalar{ scalar = Syntax.Integer n, location = l } -> do
                    return Syntax.Scalar{ scalar = Syntax.Real (fromInteger n), location = l }
                Syntax.Scalar{ scalar = Syntax.Real n, location = l } -> do
                    return Syntax.Scalar{ scalar = Syntax.Real n, location = l }
                _ -> do
                    Exception.throwIO (NeedConcreteDivisor (Syntax.location right))

            context₁ <- get

            let naturalArgument = do
                    newLeft <- check left natural

                    context₂ <- get

                    let newOperator = Syntax.Operator
                            { location
                            , left = solveSyntax context₂ newLeft
                            , operatorLocation
                            , operator = Syntax.Divide
                            , right = solveSyntax context₂ newRight
                            }

                    return (real, newOperator)

            let integerArgument = do
                    newLeft <- check left integer

                    context₂ <- get

                    let newOperator = Syntax.Operator
                            { location
                            , left = solveSyntax context₂ newLeft
                            , operatorLocation
                            , operator = Syntax.Divide
                            , right = solveSyntax context₂ newRight
                            }

                    return (real, newOperator)

            let realArgument = do
                    newLeft <- check left real

                    context₂ <- get

                    let newOperator = Syntax.Operator
                            { location
                            , left = solveSyntax context₂ newLeft
                            , operatorLocation
                            , operator = Syntax.Divide
                            , right = solveSyntax context₂ newRight
                            }

                    return (real, newOperator)

            naturalArgument `Exception.catch` \(_ :: TypeInferenceError) -> do
                set context₁

                integerArgument `Exception.catch` \(_ :: TypeInferenceError) -> do
                    set context₁

                    realArgument `Exception.catch` \(_ :: TypeInferenceError) -> do
                        Exception.throwIO (InvalidOperands "divide" (Syntax.location left) (Syntax.location right))

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

        Syntax.Builtin{ builtin = Syntax.Show, .. } -> do
            return
                (   Type.Scalar{ scalar = Monotype.JSON, .. }
                ~>  Type.Scalar{ scalar = Monotype.Text, .. }
                , Syntax.Builtin{ builtin = Syntax.Show, .. }
                )

        Syntax.Builtin{ builtin = Syntax.YAML, .. } -> do
            return
                (   Type.Scalar{ scalar = Monotype.JSON, .. }
                ~>  Type.Scalar{ scalar = Monotype.Text, .. }
                , Syntax.Builtin{ builtin = Syntax.YAML, .. }
                )

        Syntax.Builtin{ builtin = Syntax.Indexed, .. } -> do
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
                , Syntax.Builtin{ builtin = Syntax.Indexed, .. }
                )

        Syntax.Builtin{ builtin = Syntax.Length, .. } -> do
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
                , Syntax.Builtin{ builtin = Syntax.Length, .. }
                )

        Syntax.Builtin{ builtin = Syntax.Map, .. } -> do
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
                , Syntax.Builtin{ builtin = Syntax.Map, .. }
                )

        Syntax.Builtin{ builtin = Syntax.Abs, .. } -> do
            return
                (   Type.Scalar{ scalar = Monotype.Integer, .. }
                ~>  Type.Scalar{ scalar = Monotype.Natural, .. }
                , Syntax.Builtin{ builtin = Syntax.Abs, .. }
                )

        Syntax.Builtin{ builtin = Syntax.Reveal, .. } -> do
            return
                (   Type.Scalar{ scalar = Monotype.Key, .. }
                ~>  Type.Scalar{ scalar = Monotype.Text, .. }
                , Syntax.Builtin{ builtin = Syntax.Reveal, .. }
                )

        Syntax.Embed{..} -> do
            _Γ <- get

            Status{ input, .. } <- State.get

            let absolute = input <> embedded

            Import.referentiallySane input absolute

            State.put Status{ input = absolute, .. }

            syntax <- liftIO (Import.resolve absolute)

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

check Syntax.Lambda{ nameBinding = Syntax.NameBinding{ name, nameLocation, annotation = Nothing, assignment = Nothing }, ..} Type.Function{ location = _, .. } = do
    scoped (Context.Annotation name input) do
        newBody <- check body output

        let newNameBinding = Syntax.NameBinding
                { nameLocation
                , name
                , annotation = Nothing
                , assignment = Nothing
                }

        return Syntax.Lambda{ body = newBody, nameBinding = newNameBinding, .. }

check Syntax.Lambda{ nameBinding = Syntax.NameBinding{ name, nameLocation, annotation = Just annotation, assignment = Nothing }, .. } Type.Function{ location = _, ..} = do
    subtype annotation input

    scoped (Context.Annotation name input) do
        newBody <- check body output

        context <- get

        let newNameBinding = Syntax.NameBinding
                { nameLocation
                , name
                , annotation = Just (Context.solveType context annotation)
                , assignment = Nothing
                }

        return Syntax.Lambda{ body = newBody, nameBinding = newNameBinding, .. }

check Syntax.Lambda{ nameBinding = Syntax.NameBinding{ nameLocation, name, annotation = Nothing, assignment = Just assignment }, .. } Type.Function{ location = _, input = input₀, output } = do
    existential <- fresh

    push (Context.UnsolvedType existential)

    let input₁ = Type.UnsolvedType
            { location = Syntax.location assignment
            , existential
            }

    let optional = Type.Optional
            { location = Syntax.location assignment
            , type_ = input₁
            }

    subtype input₀ optional

    newAssignment <- check assignment input₁

    scoped (Context.Annotation name input₁) do
        newBody <- check body output

        let newNameBinding = Syntax.NameBinding
                { nameLocation
                , name
                , annotation = Nothing
                , assignment = Just newAssignment
                }

        return Syntax.Lambda{ body = newBody, nameBinding = newNameBinding, .. }

check Syntax.Lambda{ nameBinding = Syntax.NameBinding{ name, nameLocation, annotation = Just annotation, assignment = Just assignment }, .. } Type.Function{ location = _, ..} = do
    newAssignment <- check assignment annotation

    context₀ <- get

    subtype (Context.solveType context₀ annotation) (Context.solveType context₀ input)

    scoped (Context.Annotation name input) do
        context₁ <- get

        newBody <- check body (Context.solveType context₁ output)

        context₂ <- get

        let newNameBinding = Syntax.NameBinding
                { nameLocation
                , name
                , annotation = Just (Context.solveType context₂ annotation)
                , assignment = Just newAssignment
                }

        return Syntax.Lambda{ body = newBody, nameBinding = newNameBinding, .. }

check e Type.Forall{..} = do
    scoped (Context.Variable domain name) do
        check e type_

check Syntax.Application{ location = location₀, function = Syntax.Alternative{ location = location₁, name }, argument } annotation@Type.Union{ alternatives = Type.Alternatives alternativeTypes remainingAlternatives } = do
    existential <- fresh

    push (Context.UnsolvedAlternatives existential)

    case lookup name alternativeTypes of
        Just innerType₁ -> do
            newArgument <- check argument innerType₁

            return Syntax.Application
                { location = location₀
                , function = Syntax.Alternative
                    { location = location₀
                    , name
                    }
                , argument = newArgument
                }

        Nothing -> do
            (innerType₀, newArgument) <- infer argument

            let alternatives = Type.Alternatives
                    [ (name, innerType₀) ]
                    (Monotype.UnsolvedAlternatives existential)

            let actual = Type.Union{ location = location₁, alternatives }

            case remainingAlternatives of
                Monotype.UnsolvedAlternatives p -> do
                    instantiateAlternativesR location₁ alternatives p

                    return Syntax.Application
                        { location = location₀
                        , function = Syntax.Alternative
                            { location = location₀
                            , name
                            }
                        , argument = newArgument
                        }

                _ -> do
                    Exception.throwIO (UnionTypeMismatch actual annotation [ name ])

check Syntax.Scalar{ scalar = Syntax.Null, .. } Type.Optional{ } = do
    return Syntax.Scalar{ scalar = Syntax.Null, .. }

check Syntax.Application{ location = location₀, function = Syntax.Builtin{ location = location₁, builtin = Syntax.Some }, argument = e } Type.Optional{ type_ } = do
    newE <- check e type_
    return Syntax.Application{ location = location₀, function = Syntax.Builtin{ location = location₁, builtin = Syntax.Some }, argument = newE }

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

check Syntax.Operator{ operator = Syntax.Minus, .. } _B@Type.Scalar{ scalar }
    | scalar `elem` ([ Monotype.Integer, Monotype.Real ] :: [Monotype.Scalar]) = do
    newLeft <- check left _B

    _Γ <- get

    newRight <- check right (Context.solveType _Γ _B)

    return Syntax.Operator{ operator = Syntax.Minus, left = newLeft, right = newRight, .. }

check Syntax.List{..} Type.List{ location = _, .. } = do
    let process element = do
            _Γ <- get

            check element (Context.solveType _Γ type_)

    newElements <- traverse process elements

    return Syntax.List{ elements = newElements, .. }

check e@Syntax.Record{ fieldValues } _B@Type.Record{ fields = Type.Fields fieldTypes fields }
    | let mapValues = Map.fromList fieldValues
    , let mapTypes  = Map.fromList fieldTypes

    -- This is to prevent an infinite loop because we're going to recursively
    -- call `check` again below with two non-intersecting records to generate a
    -- type error if they're not both empty.
    , let both = Map.intersectionWith (,) mapValues mapTypes
    , not (Map.null both) = do
        let extraValues = Map.difference mapValues mapTypes
        let extraTypes  = Map.difference mapTypes  mapValues

        isRequiredTypes <- traverse isFieldRequired extraTypes

        let extraRequiredTypes =
                Map.difference extraTypes (Map.filter not isRequiredTypes)

        let extraOptionalTypes =
                Map.difference extraTypes (Map.filter id isRequiredTypes)

        let process (field, (value, type_)) = do
                _Γ <- get

                newValue <- check value (Context.solveType _Γ type_)

                return (field, newValue)

        overlappingValues <- traverse process (Map.toList both)

        let e' = Syntax.Record
                { fieldValues = Map.toList extraValues
                , location = Syntax.location e
                }

        let _B' = Type.Record
                { location = Type.location _B
                , fields = Type.Fields (Map.toList extraRequiredTypes) fields
                }

        _Γ <- get

        -- Generate a type error if either `e'` or `_B'` is not empty
        --
        -- `result` should always be an empty record, but we pass it through the
        -- returned fields out of an abundance of caution.
        result <- check e' (Context.solveType _Γ _B')

        let optionalValues = do
                (field, type_) <- Map.toList extraOptionalTypes

                return (field, Syntax.Scalar{ location = Type.location type_, scalar = Syntax.Null })

        case result of
            Syntax.Record{ fieldValues = nonOverlappingValues, .. } ->
                return Syntax.Record{ fieldValues = overlappingValues <> optionalValues <> nonOverlappingValues, .. }
            other ->
                return other

check Syntax.Project{ location, larger, smaller } annotation
    | Syntax.Single{ single = Syntax.Field{ fieldLocation, field } } <- smaller = do
        p <- fresh

        push (Context.UnsolvedFields p)

        newLarger <- check larger Type.Record
            { fields =
                Type.Fields [(field, annotation)] (Monotype.UnsolvedFields p)
            , location = fieldLocation
            }

        _Γ <- get

        return Syntax.Project{ larger = solveSyntax _Γ newLarger, .. }

check Syntax.Text{ chunks = Syntax.Chunks text₀ rest, .. } Type.Scalar{ scalar = Monotype.Text } = do
    let process (interpolation, text) = do
            newInterpolation <- check interpolation Type.Scalar{ scalar = Monotype.Text, .. }

            return (newInterpolation, text)

    newRest <- traverse process rest

    return Syntax.Text{ chunks = Syntax.Chunks text₀ newRest, .. }

check Syntax.Prompt{ schema = Nothing, .. } annotation = do
    newArguments <- check arguments (fmap (\_ -> location) (expected @Prompt))

    return Syntax.Prompt{ arguments = newArguments, schema = Just annotation, .. }

check Syntax.HTTP{ schema = Nothing, .. } annotation = do
    let input = fmap (\_ -> location) (expected @HTTP)

    newArguments <- check arguments input

    context₀ <- get

    subtype (Context.solveType context₀ annotation) Type.Scalar{ location, scalar = Monotype.JSON }

    context₁ <- get

    return Syntax.HTTP{ arguments = newArguments, schema = Just (Context.solveType context₁ annotation), .. }

check Syntax.Read{ schema = Nothing, .. } annotation = do
    newArguments <- check arguments (fmap (\_ -> location) (expected @Text))

    context₀ <- get

    subtype (Context.solveType context₀ annotation) Type.Scalar{ location, scalar = Monotype.JSON }

    context₁ <- get

    return Syntax.Read{ arguments = newArguments, schema = Just (Context.solveType context₁ annotation), .. }

check Syntax.List{..} annotation@Type.Scalar{ scalar = Monotype.JSON } = do
    newElements <- traverse (`check` annotation) elements

    let annotated = Syntax.List{ elements = newElements, .. }

    return Syntax.Annotation
        { annotated, annotation, location = Syntax.location annotated }

check Syntax.Record{..} annotation@Type.Scalar{ scalar = Monotype.JSON } = do
    let process (field, value) = do
            newValue <- check value annotation

            return (field, newValue)

    newFieldValues <- traverse process fieldValues

    return Syntax.Record{ fieldValues = newFieldValues, .. }

check e@Syntax.Text{ } Type.Scalar{ scalar = Monotype.JSON, .. } = do
    check e Type.Scalar{ scalar = Monotype.Text, .. }

check Syntax.Scalar{ scalar = Syntax.Natural natural, .. } Type.Scalar{ scalar = Monotype.JSON } = do
    return Syntax.Scalar{ scalar = Syntax.Natural natural, .. }

check Syntax.Scalar{ scalar = Syntax.Integer integer, .. } Type.Scalar{ scalar = Monotype.JSON } = do
    return Syntax.Scalar{ scalar = Syntax.Integer integer, .. }

check Syntax.Scalar{ scalar = Syntax.Real real, .. } Type.Scalar{ scalar = Monotype.JSON } = do
    return Syntax.Scalar{ scalar = Syntax.Real real, .. }

check Syntax.Scalar{ scalar = Syntax.Bool bool, .. } Type.Scalar{ scalar = Monotype.JSON } = do
    return Syntax.Scalar{ scalar = Syntax.Bool bool, .. }

check Syntax.Scalar{ scalar = Syntax.Null, .. } Type.Scalar{ scalar = Monotype.JSON } = do
    return Syntax.Scalar{ scalar = Syntax.Null, .. }

check Syntax.Scalar{ scalar = Syntax.Natural n, .. } Type.Scalar{ scalar = Monotype.Real } = do
    return Syntax.Scalar{ scalar = Syntax.Real (fromIntegral n), .. }

check Syntax.Scalar{ scalar = Syntax.Integer n, .. } Type.Scalar{ scalar = Monotype.Real } = do
    return Syntax.Scalar{ scalar = Syntax.Real (fromInteger n), .. }

check Syntax.Scalar{ scalar = Syntax.Real n, .. } Type.Scalar{ scalar = Monotype.Real } = do
    return Syntax.Scalar{ scalar = Syntax.Real n, .. }

check Syntax.Scalar{ scalar = Syntax.Natural n, .. } Type.Scalar{ scalar = Monotype.Integer } = do
    return Syntax.Scalar{ scalar = Syntax.Integer (fromIntegral n), .. }

check Syntax.Scalar{ scalar = Syntax.Integer n, .. } Type.Scalar{ scalar = Monotype.Integer } = do
    return Syntax.Scalar{ scalar = Syntax.Integer n, .. }

check Syntax.Scalar{ scalar = Syntax.Natural n, .. } Type.Scalar{ scalar = Monotype.Natural } = do
    return Syntax.Scalar{ scalar = Syntax.Natural n, .. }

check annotated annotation@Type.Scalar{ scalar = Monotype.Real } = do
    (_A₀, newAnnotated) <- infer annotated

    _Γ <- get

    let _A₁ = Context.solveType _Γ _A₀

    let real = do
            subtype _A₁ annotation

            return newAnnotated

    let integer = do
            subtype _A₁ Type.Scalar
                { scalar = Monotype.Integer
                , location = Syntax.location newAnnotated
                }

            return Syntax.Annotation
                { annotated = newAnnotated
                , annotation
                , location = Syntax.location newAnnotated
                }

    let natural = do
            subtype _A₁ Type.Scalar
                { scalar = Monotype.Natural
                , location = Syntax.location newAnnotated
                }

            return Syntax.Annotation
                { annotated = newAnnotated
                , annotation
                , location = Syntax.location newAnnotated
                }

    real `Exception.catch` \(_ :: TypeInferenceError) -> do
        integer `Exception.catch` \(_ :: TypeInferenceError) -> do
            natural `Exception.catch` \(_ :: TypeInferenceError) -> do
                subtype _A₁ annotation

                return newAnnotated

check annotated annotation@Type.Scalar{ scalar = Monotype.Integer } = do
    (_A₀, newAnnotated) <- infer annotated

    _Γ <- get

    let _A₁ = Context.solveType _Γ _A₀

    let integer = do
            subtype _A₁ annotation

            return newAnnotated

    let natural = do
            subtype _A₁ Type.Scalar
                { scalar = Monotype.Natural
                , location = Syntax.location newAnnotated
                }

            return Syntax.Annotation
                { annotated = newAnnotated
                , annotation
                , location = Syntax.location newAnnotated
                }

    integer `Exception.catch` \(_ :: TypeInferenceError) -> do
        natural `Exception.catch` \(_ :: TypeInferenceError) -> do
            subtype _A₁ annotation

            return newAnnotated

check Syntax.Embed{ embedded } _B = do
    _Γ <- get

    Status{ input, .. } <- State.get

    let absolute = input <> embedded

    Import.referentiallySane input absolute

    State.put Status{ input = absolute, .. }

    syntax <- liftIO (Import.resolve absolute)

    result <- check syntax _B

    State.modify (\s -> s{ Grace.Infer.input = input })

    return result

check Syntax.Text{ chunks = Syntax.Chunks text₀ [], .. } Type.Scalar{ scalar = Monotype.Key } = do
    return Syntax.Scalar{ scalar = Syntax.Key text₀, .. }

check annotated annotation@Type.Scalar{ scalar = Monotype.Key } = do
    (_A₀, newAnnotated) <- infer annotated

    _Γ <- get

    let _A₁ = Context.solveType _Γ _A₀

    let key = do
            subtype _A₁ annotation

            return newAnnotated

    let text = do
            subtype _A₁ Type.Scalar
                { scalar = Monotype.Text
                , location = Syntax.location newAnnotated
                }

            return Syntax.Annotation
                { annotated = newAnnotated
                , annotation
                , location = Syntax.location newAnnotated
                }

    key `Exception.catch` \(_ :: TypeInferenceError) -> do
        text `Exception.catch` \(_ :: TypeInferenceError) -> do
            subtype _A₁ annotation

            return newAnnotated

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
    -> Syntax Location Input
    -> m (Type Location, Syntax Location Void)
typeOf input = typeWith input []

-- | Like `typeOf`, but accepts a custom type-checking `Context`
typeWith
    :: (MonadCatch m, MonadIO m)
    => Input
    -> Context Location
    -> Syntax Location Input
    -> m (Type Location, Syntax Location Void)
typeWith input context syntax = do
    let initialStatus = Status{ count = 0, context, input }

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
    | InvalidOperands Text Location Location
    | ZeroDivisor Location
    | NeedConcreteDivisor Location
    --
    | FoldConcreteRecord Location (Type Location)
    | FoldInvalidHandler Location (Type Location)
    | FoldRecord Location (Type Location)
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
    | UnboundVariable Location Text
    --
    | RecordTypeMismatch (Type Location) (Type Location) [Text]
    | UnionTypeMismatch (Type Location) (Type Location) [Text]
    deriving stock (Eq, Show)

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

    displayException (InvalidOperands action left right) =
        "Invalid operands\n\
        \\n\
        \You cannot " <> Text.unpack action <> " the following operands:\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" left) <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" right)

    displayException (ZeroDivisor location) =
        "Zero divisor\n\
        \\n\
        \You cannot divide a number by zero:\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location)

    displayException (NeedConcreteDivisor location) =
        "Divisor must be concrete\n\
        \\n\
        \You must divide by a concrete (non-abstract) numeric literal:\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location)

    displayException (FoldConcreteRecord location _R) =
        "Must fold a concrete record\n\
        \\n\
        \The first argument to a fold must be a record where all fields are statically\n\
        \known.  However, you provided an argument of type:\n\
        \\n\
        \" <> insert _R <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location) <> "\n\
        \\n\
        \… where not all fields could be inferred."

    displayException (FoldInvalidHandler location _A) =
        "Invalid handler\n\
        \\n\
        \The fold keyword expects a record of handlers where all handlers are functions,\n\
        \but you provided a handler of the following type:\n\
        \\n\
        \" <> insert _A <> "\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location) <> "\n\
        \\n\
        \… which is not a function type."

    displayException (FoldRecord location _R) =
        "Must fold a record\n\
        \\n\
        \The first argument to a fold must be a record, but you provided an expression of\n\
        \the following type:\n\
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

    displayException (UnboundVariable location name) =
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
