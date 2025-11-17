{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE OverloadedLists  #-}

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
    , infer
    , inferJSON
    , checkJSON

      -- * Types
    , HTTP(..)

      -- * Errors related to type inference
    , TypeInferenceError(..)
    ) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (Exception(..), MonadThrow)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (MonadState)
import Data.Foldable (for_, toList, traverse_)
import Data.Sequence (ViewL(..), (<|))
import Data.Typeable (Typeable)
import Data.Text (Text)
import Data.Void (Void)
import Grace.Context (Context, Entry)
import Grace.Decode (FromGrace(..))
import Grace.Existential (Existential)
import Grace.GitHub (GitHub(..))
import Grace.HTTP.Type (HTTP(..))
import Grace.Input (Input(..), Mode(..))
import Grace.Location (Location(..))
import Grace.Monad (Grace, Status(..))
import Grace.Monotype (Monotype)
import Grace.Pretty (Pretty(..))
import Grace.Prompt.Types (Prompt(..))
import Grace.Type (Type(..))
import Grace.Value (Value)

import Grace.Syntax
    (Binding, BindMonad(..), Definition(..), NameBinding, Syntax)

import qualified Control.Exception.Safe as Exception
import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson.Pretty
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.HashMap.Strict.InsOrd as Map
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Scientific as Scientific
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Grace.Compat as Compat
import qualified Grace.Context as Context
import qualified Grace.Domain as Domain
import qualified Grace.Import as Import
import qualified Grace.Location as Location
import qualified Grace.Monad as Grace
import qualified Grace.Monotype as Monotype
import qualified Grace.Pretty
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type
import qualified Grace.Value as Value
import qualified Prettyprinter as Pretty

orDie :: (Exception e, MonadThrow m) => Maybe a -> e -> m a
Just x  `orDie` _ = return x
Nothing `orDie` e = Exception.throwIO e

-- | Generate a fresh existential variable (of any type)
fresh :: MonadState Status m => m (Existential a)
fresh = do
    let update Status{ count = count₀, .. } =
            (fromIntegral count₀, Status{ count = count₁, .. })
          where
            count₁ = count₀ + 1

    State.state update

-- Unlike the original paper, we don't explicitly thread the `Context` around.
-- Instead, we modify the ambient state using the following utility functions:

-- | Push a new `Context` `Entry` onto the stack
push :: MonadState Status m => Entry Location -> m ()
push entry = State.modify (\s -> s { context = entry : context s })

-- | Push an unsolved variable to the very beginning of the `Context`.  This
-- ensures that the unsolved variable is never lost.
--
-- This comes in handy for existential variables created as part of `import`
-- keywords, where we want to make sure that these type variables are preserved
-- in the final `Context` so that they can be reused by evaluation.
preserve :: MonadState Status m => Entry Location -> m ()
preserve entry = State.modify (\s -> s { context = context s <> [ entry ] })

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
isFieldRequired :: Type Location -> Grace Bool
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
subtype :: Type Location -> Type Location -> Grace ()
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
instantiateTypeL :: Existential Monotype -> Type Location -> Grace ()
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
instantiateTypeR :: Type Location -> Existential Monotype -> Grace ()
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
    :: Existential Monotype.Record -> Existential Monotype.Record -> Grace ()
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
    :: Existential Monotype.Record
    -> Location
    -> Type.Record Location
    -> Grace ()
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
    :: Location
    -> Type.Record Location
    -> Existential Monotype.Record
    -> Grace ()
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
    :: Existential Monotype.Union-> Existential Monotype.Union -> Grace ()
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
    :: Existential Monotype.Union
    -> Location
    -> Type.Union Location
    -> Grace ()
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
    :: Location
    -> Type.Union Location
    -> Existential Monotype.Union
    -> Grace ()
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

onNameBinding
    :: NameBinding Location Input
    -> Grace ((Text, Type Location), Entry Location, NameBinding Location Void)
onNameBinding Syntax.NameBinding{ nameLocation, name, annotation = Nothing, assignment = Nothing } = do
    existential <- fresh

    push (Context.UnsolvedType existential)

    let annotation = Type.UnsolvedType
            { location = nameLocation
            , existential
            }

    let fieldType = (name, annotation)

    let entry = Context.Annotation name annotation

    let newNameBinding = Syntax.NameBinding
            { nameLocation
            , name
            , annotation = Nothing
            , assignment = Nothing
            }

    return (fieldType, entry, newNameBinding)
onNameBinding Syntax.NameBinding{ nameLocation, name, annotation = Just annotation, assignment = Nothing } = do
    let fieldType = (name, annotation)

    let entry = Context.Annotation name annotation

    let newNameBinding = Syntax.NameBinding
            { nameLocation
            , name
            , annotation = Just annotation
            , assignment = Nothing
            }

    return (fieldType, entry, newNameBinding)
onNameBinding Syntax.NameBinding{ nameLocation, name, annotation = Nothing, assignment = Just assignment } = do
    (annotation₀, newAssignment) <- infer assignment

    let annotation₁ = Type.Optional
            { location = Syntax.location assignment
            , type_ = annotation₀
            }

    let fieldType = (name, annotation₁)

    let entry = Context.Annotation name annotation₀

    let newNameBinding = Syntax.NameBinding
            { nameLocation
            , name
            , annotation = Nothing
            , assignment = Just newAssignment
            }

    return (fieldType, entry, newNameBinding)
onNameBinding Syntax.NameBinding{ nameLocation, name, annotation = Just annotation₀, assignment = Just assignment } = do
    let annotation₁ = Type.Optional
            { location = Syntax.location assignment
            , type_ = annotation₀
            }

    context <- get

    newAssignment <- check assignment (Context.solveType context annotation₀)

    let fieldType = (name, annotation₁)

    let entry = Context.Annotation name annotation₀

    let newNameBinding = Syntax.NameBinding
            { nameLocation
            , name
            , annotation = Just annotation₀
            , assignment = Just newAssignment
            }

    return (fieldType, entry, newNameBinding)

onBinding
    :: Binding Location Input
    -> Grace (Type Location, Context Location, Binding Location Void)
onBinding Syntax.PlainBinding{ plain } = do
    ((_, annotation), entry, newPlain) <- onNameBinding plain

    return (annotation, [ entry ], Syntax.PlainBinding{ plain = newPlain })
onBinding Syntax.RecordBinding{ fieldNamesLocation, fieldNames } = do
    tuples <- traverse onNameBinding fieldNames

    let (fieldTypes, entries, newFieldNames) = unzip3 tuples

    existential <- fresh

    push (Context.UnsolvedFields existential)

    let annotation = Type.Record
            { location = fieldNamesLocation
            , fields =
                Type.Fields fieldTypes (Monotype.UnsolvedFields existential)
            }

    let newBinding = Syntax.RecordBinding
            { fieldNamesLocation
            , fieldNames = newFieldNames
            }

    return (annotation, entries, newBinding)

onDefinition
    :: Definition Location Input
    -> Grace ((Text, Type Location), Definition Location Void)
onDefinition Syntax.Definition
    { nameLocation
    , name
    , bindings
    , annotation = annotation₀
    , assignment = assignment₀
    } = do
        results <- traverse onBinding bindings

        let (inputs, entriess, newBindings) = unzip3 results

        let nil = do
                (annotation₁, assignment₂) <- case (bindings, annotation₀) of
                    ([], Just annotation₁) -> do
                        assignment₂ <- check assignment₀ annotation₁

                        return (annotation₁, assignment₂)
                    (_, Just annotation₁) -> do
                        let assignment₁ = Syntax.Annotation
                                { location = Syntax.location assignment₀
                                , annotated = assignment₀
                                , annotation = annotation₁
                                }

                        infer assignment₁
                    (_, Nothing) -> do
                        infer assignment₀

                let newDefinition = Syntax.Definition
                        { nameLocation
                        , name
                        , bindings = newBindings
                        , annotation = annotation₀
                        , assignment = assignment₂
                        }

                let cons input output = Type.Function
                        { location = nameLocation
                        , input
                        , output
                        }

                let annotation₂ = foldr cons annotation₁ inputs

                let fieldType = (name, annotation₂)

                return (fieldType, newDefinition)

        foldr scoped nil (concat entriess)

{-| This corresponds to the judgment:

    > Γ ⊢ e ⇒ A ⊣ Δ

    … which infers the type of e under input context Γ, producing an inferred
    type of A and an updated context Δ.
-}
infer :: Syntax Location Input -> Grace (Type Location, Syntax Location Void)
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
        Syntax.Lambda{ location, binding, body } -> do
            (input, entries, newBinding) <- onBinding binding

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

                    let inferred = Type.Function{ location, input, output }

                    let newLambda = Syntax.Lambda
                            { location
                            , binding = newBinding
                            , body = newBody
                            }

                    -- TODO: Only `solveSyntax` `newBinding`
                    return (Context.solveType context inferred, solveSyntax context newLambda)

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

        Syntax.Let{ location, assignments, body } -> do
            let assign monad₀ = do
                    let cons Syntax.Define{ assignmentLocation, definition } action = do
                            ((name, annotation), newDefinition) <- onDefinition definition

                            let entry = Context.Annotation name annotation

                            let newAssignment = Syntax.Define
                                    { assignmentLocation
                                    , definition = newDefinition
                                    }

                            scoped entry do
                                (newAssignments, newBody) <- action

                                return (newAssignment : newAssignments, newBody)

                        cons Syntax.Bind{ assignmentLocation, monad = monad₁, binding, assignment = value } action = do
                            case (monad₀, monad₁) of
                                (Nothing, Just _) -> do
                                    Exception.throwIO (MonadMismatch assignmentLocation)

                                _ -> do
                                    (annotation₀, newEntries, newBinding) <- onBinding binding

                                    (newAssignments, newBody) <- foldr scoped action newEntries

                                    context <- get

                                    let annotation₁ = Context.solveType context annotation₀

                                    let annotation₂ = case monad₀ of
                                            Just ListMonad -> Type.List
                                                { location = assignmentLocation
                                                , type_ = annotation₁
                                                }

                                            Just OptionalMonad -> Type.Optional
                                                { location = assignmentLocation
                                                , type_ = annotation₁
                                                }

                                            Just UnknownMonad -> annotation₁

                                            Nothing -> annotation₁

                                    newValue <- check value annotation₂

                                    let newAssignment = Syntax.Bind
                                            { assignmentLocation
                                            , monad = monad₀
                                            , binding = newBinding
                                            , assignment = newValue
                                            }

                                    return (newAssignment : newAssignments, newBody)

                    b <- fresh

                    push (Context.UnsolvedType b)

                    let unsolved = Type.UnsolvedType
                            { location = Syntax.location body
                            , existential = b
                            }

                    let nil = do
                            newBody <- check body unsolved

                            return ([], newBody)

                    (newAssignments, newBody) <- foldr cons nil assignments

                    let output = case monad₀ of
                            Just OptionalMonad -> Type.Optional
                                { location = location
                                , type_ = unsolved
                                }
                            Just ListMonad -> Type.List
                                { location = location
                                , type_ = unsolved
                                }
                            Just UnknownMonad -> unsolved
                            Nothing -> unsolved

                    context <- get

                    let newLet = Syntax.Let
                            { location
                            , assignments = NonEmpty.fromList newAssignments
                            , body = newBody
                            }

                    return (Context.solveType context output, solveSyntax context newLet)

            context <- get

            assign Nothing `Exception.catch` \(_ :: MonadMismatch) -> do
                set context

                assign (Just ListMonad) `Exception.catch` \(e :: TypeInferenceError) -> do
                    set context

                    assign (Just OptionalMonad) `Exception.catch` \(_ :: TypeInferenceError) -> do
                        Exception.throwIO e

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
            result <- traverse onDefinition fieldValues

            let (fieldTypes, newFieldValues) = unzip result

            let inferred = Type.Record
                    { location
                    , fields = Type.Fields fieldTypes Monotype.EmptyFields
                    }

            let newRecord = Syntax.Record
                    { location
                    , fieldValues = newFieldValues
                    }

            return (inferred, newRecord)

        Syntax.Alternative{ location, name, argument } -> do
            (argumentType, newArgument) <- infer argument

            alternatives <- fresh

            push (Context.UnsolvedAlternatives alternatives)

            let inferred = Type.Union
                    { location
                    , alternatives = Type.Alternatives
                        [(name, argumentType)]
                        (Monotype.UnsolvedAlternatives alternatives)
                    }

            let newAlternative = Syntax.Alternative
                    { location
                    , name
                    , argument = newArgument
                    }

            return (inferred, newAlternative)

        Syntax.Fold{ location, handlers } -> do
            let boolFold = do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    let bool = Type.UnsolvedType{ location, existential }

                    newHandlers <- check handlers Type.Record
                        { location
                        , fields = Type.Fields
                            [ ("false", bool)
                            , ("true", bool)
                            ]
                            Monotype.EmptyFields
                        }

                    let type_ = Type.Function
                            { location
                            , input = Type.Scalar
                                { location
                                , scalar = Monotype.Bool
                                }
                            , output = bool
                            }

                    let newFold = Syntax.Fold
                            { location
                            , handlers = newHandlers
                            }

                    return (type_, newFold)

            let naturalFold = do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    let natural = Type.UnsolvedType{ location, existential }

                    newHandlers <- check handlers Type.Record
                        { location
                        , fields = Type.Fields
                            [ ( "zero", natural )
                            , ( "succ"
                              , Type.Function
                                  { location
                                  , input = natural
                                  , output = natural
                                  }
                              )
                            ]
                            Monotype.EmptyFields
                        }

                    let type_ = Type.Function
                            { location
                            , input = Type.Scalar
                                { location
                                , scalar = Monotype.Natural
                                }
                            , output = natural
                            }

                    let newFold = Syntax.Fold
                            { location
                            , handlers = newHandlers
                            }

                    return (type_, newFold)

            let optionalFold = do
                    existential₀ <- fresh

                    push (Context.UnsolvedType existential₀)

                    let element = Type.UnsolvedType
                            { location
                            , existential = existential₀
                            }

                    existential₁ <- fresh

                    push (Context.UnsolvedType existential₁)

                    let optional = Type.UnsolvedType
                            { location
                            , existential = existential₁
                            }

                    newHandlers <- check handlers Type.Record
                        { location
                        , fields = Type.Fields
                            [ ( "null", optional )
                            , ( "some"
                              , Type.Function
                                  { location
                                  , input = element
                                  , output = optional
                                  }
                              )
                            ]
                            Monotype.EmptyFields
                        }

                    let type_ = Type.Function
                            { location
                            , input = Type.Optional
                                { location
                                , type_ = element
                                }
                            , output = optional
                            }

                    let newFold = Syntax.Fold
                            { location
                            , handlers = newHandlers
                            }

                    return (type_, newFold)

            let listFold = do
                    existential₀ <- fresh

                    push (Context.UnsolvedType existential₀)

                    let element = Type.UnsolvedType
                            { location
                            , existential = existential₀
                            }

                    existential₁ <- fresh

                    push (Context.UnsolvedType existential₁)

                    let list = Type.UnsolvedType
                            { location
                            , existential = existential₁
                            }

                    newHandlers <- check handlers Type.Record
                        { location
                        , fields = Type.Fields
                            [ ( "nil", list )
                            , ( "cons"
                              , Type.Function
                                  { location
                                  , input = element
                                  , output = Type.Function
                                      { location
                                      , input = list
                                      , output = list
                                      }
                                  }
                              )
                            ]
                            Monotype.EmptyFields
                        }

                    let type_ = Type.Function
                            { location
                            , input = Type.List
                                { location
                                , type_ = element
                                }
                            , output = list
                            }

                    let newFold = Syntax.Fold
                            { location
                            , handlers = newHandlers
                            }

                    return (type_, newFold)

            let jsonFold = do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    let json = Type.UnsolvedType{ location, existential }

                    newHandlers <- check handlers Type.Record
                        { location
                        , fields = Type.Fields
                            [ ( "array"
                              , Type.Function
                                  { location
                                  , input = Type.List
                                      { location
                                      , type_ = json
                                      }
                                  , output = json
                                  }
                              )
                            , ( "bool"
                              , Type.Function
                                  { location
                                  , input = Type.Scalar
                                      { location
                                      , scalar = Monotype.Bool
                                      }
                                  , output = json
                                  }
                              )
                            , ( "integer"
                              , Type.Function
                                  { location
                                  , input = Type.Scalar
                                      { location
                                      , scalar = Monotype.Integer
                                      }
                                  , output = json
                                  }
                              )
                            , ( "natural"
                              , Type.Function
                                  { location
                                  , input = Type.Scalar
                                      { location
                                      , scalar = Monotype.Natural
                                      }
                                  , output = json
                                  }
                              )
                            , ( "null", json )
                            , ( "object"
                              , Type.Function
                                  { location
                                  , input = Type.List
                                      { location
                                      , type_ = Type.Record
                                          { location
                                          , fields = Type.Fields
                                              [ ( "key"
                                                , Type.Scalar
                                                    { location
                                                    , scalar = Monotype.Text
                                                    }
                                                )
                                              , ( "value", json)
                                              ]
                                              Monotype.EmptyFields
                                          }
                                      }
                                  , output = json
                                  }
                              )
                            , ( "real"
                              , Type.Function
                                  { location
                                  , input = Type.Scalar
                                      { location
                                      , scalar = Monotype.Real
                                      }
                                  , output = json
                                  }
                              )
                            , ( "string"
                              , Type.Function
                                  { location
                                  , input = Type.Scalar
                                      { location
                                      , scalar = Monotype.Text
                                      }
                                  , output = json
                                  }
                              )
                            ]
                            Monotype.EmptyFields
                        }

                    let type_ = Type.Function
                            { location
                            , input = Type.Scalar
                                { location
                                , scalar = Monotype.JSON
                                }
                            , output = json
                            }

                    let newFold = Syntax.Fold
                            { location
                            , handlers = newHandlers
                            }

                    return (type_, newFold)

            let fold = do
                    context₀ <- get

                    existential₀ <- fresh

                    push (Context.UnsolvedFields existential₀)

                    let unsolvedRecord = Type.Fields
                            []
                            (Monotype.UnsolvedFields existential₀)

                    _ <- check handlers Type.Record
                        { location
                        , fields = unsolvedRecord
                        }

                    context₁ <- get

                    let Type.Fields keyTypes _ =
                            Context.solveRecord context₁ unsolvedRecord

                    set context₀

                    existential₁ <- fresh

                    push (Context.UnsolvedType existential₁)

                    let union = Type.UnsolvedType
                            { location
                            , existential = existential₁
                            }

                    let process (key, _) = do
                            existential <- fresh

                            push (Context.UnsolvedType existential)

                            let alternativeType = Type.UnsolvedType
                                    { location
                                    , existential
                                    }

                            let handlerType = Type.Function
                                    { location
                                    , input = alternativeType
                                    , output = union
                                    }

                            return ((key, handlerType), (key, alternativeType))

                    results <- traverse process keyTypes

                    let (fieldTypes, alternativeTypes) = unzip results

                    newHandlers <- check handlers Type.Record
                        { location
                        , fields = Type.Fields
                            fieldTypes
                            Monotype.EmptyFields
                        }

                    let type_ = Type.Function
                            { location
                            , input = Type.Union
                                { location
                                , alternatives = Type.Alternatives
                                    alternativeTypes
                                    Monotype.EmptyAlternatives
                                }
                            , output = union
                            }

                    let newFold = Syntax.Fold
                            { location
                            , handlers = newHandlers
                            }

                    return (type_, newFold)

            context <- get

            listFold `Exception.catch` \(_ :: TypeInferenceError) -> do
                set context

                optionalFold `Exception.catch` \(_ :: TypeInferenceError) -> do
                    set context

                    boolFold `Exception.catch` \(_ :: TypeInferenceError) -> do
                        set context

                        naturalFold `Exception.catch` \(_ :: TypeInferenceError) -> do
                            set context

                            jsonFold `Exception.catch` \(_ :: TypeInferenceError) -> do
                                set context

                                fold

        Syntax.Project{ location, larger, smaller } -> do
            let processField Syntax.Field{ fieldLocation, field } = do
                    existential <- fresh

                    push (Context.UnsolvedType existential)

                    return (field, Type.UnsolvedType{ location = fieldLocation, .. })

            case smaller of
                Syntax.Single{ single } -> do
                    let Syntax.Field{ fieldLocation, field } = single

                    context <- get

                    (largerType, newLarger₀) <- infer larger

                    case largerType of
                        Type.Record{ fields = Type.Fields fieldTypes _ }
                            | Just type_ <- lookup field fieldTypes -> do
                                return (type_, Syntax.Project{ location, larger = newLarger₀, smaller })

                        _ -> do
                            set context

                            p <- fresh

                            push (Context.UnsolvedFields p)

                            fieldType@(_, type_) <- processField single

                            newLarger₁ <- check larger Type.Record
                                { fields =
                                    Type.Fields [fieldType] (Monotype.UnsolvedFields p)
                                , location = fieldLocation
                                }

                            return (type_, Syntax.Project{ larger = newLarger₁, .. })

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

                    let list = Type.List{ location, type_ = element }

                    let optional = Type.Optional{ location, type_ = list }

                    newLarger <- check larger list

                    return (optional, Syntax.Project{ location, larger = newLarger, .. })

        Syntax.If{ location, predicate, ifTrue, ifFalse} -> do
            newPredicate <- check predicate Type.Scalar
                { location
                , scalar = Monotype.Bool
                }

            (annotation₀, newIfTrue) <- infer ifTrue

            context₀ <- get

            let annotation₁ = Context.solveType context₀ annotation₀

            newIfFalse <- check ifFalse annotation₁

            context₁ <- get

            let newIf = Syntax.If
                    { location
                    , predicate = solveSyntax context₁ newPredicate
                    , ifTrue = solveSyntax context₁ newIfTrue
                    , ifFalse = solveSyntax context₁ newIfFalse
                    }

            return (annotation₁, newIf)

        Syntax.Text{ chunks = Syntax.Chunks text₀ rest, .. } -> do
            let process (interpolation, text) = do
                    newInterpolation <- check interpolation Type.Scalar{ scalar = Monotype.Text, .. }

                    return (newInterpolation, text)

            newRest <- traverse process rest

            return (Type.Scalar{ scalar = Monotype.Text, .. }, Syntax.Text{ chunks = Syntax.Chunks text₀ newRest, .. })

        Syntax.Prompt{ location, import_, arguments, schema } -> do
            let argumentsType = fmap (\_ -> location) (expected @Prompt)

            newArguments <- check arguments argumentsType

            newSchema <- case schema of
                Just t -> do
                    return t

                Nothing -> do
                    existential <- fresh

                    preserve (Context.UnsolvedType existential)

                    return Type.UnsolvedType{ location, existential }

            context <- get

            let newPrompt = Syntax.Prompt
                    { location
                    , import_
                    , arguments = solveSyntax context newArguments
                    , schema = Just newSchema
                    }

            return (newSchema, newPrompt)

        Syntax.HTTP{ location, import_, arguments, schema } -> do
            let argumentsType = fmap (\_ -> location) (expected @HTTP)

            newArguments <- check arguments argumentsType

            newSchema <- case schema of
                Just output -> do
                    return output

                Nothing -> do
                    existential <- fresh

                    preserve (Context.UnsolvedType existential)

                    return Type.UnsolvedType{ location, existential }

            context <- get

            let newHTTP = Syntax.HTTP
                    { location
                    , import_
                    , arguments = solveSyntax context newArguments
                    , schema = Just newSchema
                    }

            return (newSchema, newHTTP)

        Syntax.Read{ location, import_, arguments, schema } -> do
            let argumentsType = fmap (\_ -> location) (expected @Text)

            newArguments <- check arguments argumentsType

            newSchema <- case schema of
                Just output -> do
                    return output

                Nothing -> do
                    existential <- fresh

                    preserve (Context.UnsolvedType existential)

                    return Type.UnsolvedType{ location, existential }

            context <- get

            let newRead = Syntax.Read
                    { location
                    , import_
                    , arguments = solveSyntax context newArguments
                    , schema = Just newSchema
                    }

            return (newSchema, newRead)

        Syntax.GitHub{ location, import_, arguments, schema } -> do
            let argumentsType = fmap (\_ -> location) (expected @GitHub)

            newArguments <- check arguments argumentsType

            newSchema <- case schema of
                Just output -> do
                    return output

                Nothing -> do
                    existential <- fresh

                    preserve(Context.UnsolvedType existential)

                    return Type.UnsolvedType{ location, existential}

            context <- get

            let newGitHub = Syntax.GitHub
                    { location
                    , import_
                    , arguments = solveSyntax context newArguments
                    , schema = Just newSchema
                    }

            return (newSchema, newGitHub)

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

            naturalArguments `Exception.catch` \(_ :: TypeInferenceError) -> do
                set context₁

                integerArguments `Exception.catch` \(_ :: TypeInferenceError) -> do
                    set context₁

                    realArguments `Exception.catch` \(_ :: TypeInferenceError) -> do
                        set context₁

                        textArguments `Exception.catch` \(_ :: TypeInferenceError) -> do
                            set context₁

                            listArguments `Exception.catch` \(_ :: TypeInferenceError) -> do
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
            let real = Type.Scalar
                    { location = operatorLocation
                    , scalar = Monotype.Real
                    }

            newLeft  <- check left  real
            newRight <- check right real

            context <- get

            let newOperator = Syntax.Operator
                    { location
                    , left = solveSyntax context newLeft
                    , operatorLocation
                    , operator = Syntax.Divide
                    , right = solveSyntax context newRight
                    }

            return (real, newOperator)

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

        Syntax.Embed{ embedded } -> do
            _Γ <- get

            input <- Reader.ask

            Reader.local (\i -> i <> embedded) do
                absolute <- Reader.ask

                Import.referentiallySane input absolute

                syntax <- liftIO (Import.resolve AsCode absolute)

                infer syntax

{-| This corresponds to the judgment:

    > Γ ⊢ e ⇐ A ⊣ Δ

    … which checks that e has type A under input context Γ, producing an updated
    context Δ.
-}
check :: Syntax Location Input -> Type Location -> Grace (Syntax Location Void)
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

check Syntax.Lambda{ binding = Syntax.PlainBinding{ plain = Syntax.NameBinding{ name, nameLocation, annotation = Nothing, assignment = Nothing } }, ..} Type.Function{ location = _, .. } = do
    scoped (Context.Annotation name input) do
        newBody <- check body output

        let newBinding = Syntax.PlainBinding
                { plain = Syntax.NameBinding
                    { nameLocation
                    , name
                    , annotation = Nothing
                    , assignment = Nothing
                    }
                }

        return Syntax.Lambda{ body = newBody, binding = newBinding, .. }

check Syntax.Lambda{ binding = Syntax.PlainBinding{ plain = Syntax.NameBinding{ name, nameLocation, annotation = Just annotation, assignment = Nothing } }, .. } Type.Function{ location = _, ..} = do
    subtype annotation input

    scoped (Context.Annotation name input) do
        newBody <- check body output

        context <- get

        let newBinding = Syntax.PlainBinding
                { plain = Syntax.NameBinding
                    { nameLocation
                    , name
                    , annotation = Just (Context.solveType context annotation)
                    , assignment = Nothing
                    }
                }

        return Syntax.Lambda{ body = newBody, binding = newBinding, .. }

check Syntax.Lambda{ binding = Syntax.PlainBinding{ plain = Syntax.NameBinding{ nameLocation, name, annotation = Nothing, assignment = Just assignment } }, .. } Type.Function{ location = _, input = input₀, output } = do
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

        let newBinding = Syntax.PlainBinding
                { plain = Syntax.NameBinding
                    { nameLocation
                    , name
                    , annotation = Nothing
                    , assignment = Just newAssignment
                    }
                }

        return Syntax.Lambda{ body = newBody, binding = newBinding, .. }

check Syntax.Lambda{ binding = Syntax.PlainBinding{ plain = Syntax.NameBinding{ name, nameLocation, annotation = Just annotation, assignment = Just assignment } }, .. } Type.Function{ location = _, ..} = do
    newAssignment <- check assignment annotation

    context₀ <- get

    subtype (Context.solveType context₀ annotation) (Context.solveType context₀ input)

    scoped (Context.Annotation name input) do
        context₁ <- get

        newBody <- check body (Context.solveType context₁ output)

        context₂ <- get

        let newBinding = Syntax.PlainBinding
                { plain = Syntax.NameBinding
                    { nameLocation
                    , name
                    , annotation = Just (Context.solveType context₂ annotation)
                    , assignment = Just newAssignment
                    }
                }

        return Syntax.Lambda{ body = newBody, binding = newBinding, .. }

check e Type.Forall{..} = do
    scoped (Context.Variable domain name) do
        check e type_

check Syntax.Let{ location, assignments, body } annotation₀ = do
    let assign monad₀ = do
            let cons Syntax.Define{ assignmentLocation, definition } action = do
                    ((name, annotation₁), newDefinition) <- onDefinition definition

                    let entry = Context.Annotation name annotation₁

                    let newAssignment = Syntax.Define
                            { assignmentLocation
                            , definition = newDefinition
                            }

                    scoped entry do
                        (newAssignments, newBody) <- action

                        return (newAssignment : newAssignments, newBody)

                cons Syntax.Bind{ assignmentLocation, monad = monad₁, binding, assignment = value } action =
                    case (monad₀, monad₁) of
                        (Nothing, Just _) -> do
                            Exception.throwIO (MonadMismatch assignmentLocation)

                        _ -> do
                            (annotation₁, newEntries, newBinding) <- onBinding binding

                            (newAssignments, newBody) <- foldr scoped action newEntries

                            annotation₂ <- case monad₀ of
                                Just ListMonad -> do
                                    context <- get

                                    existential <- fresh

                                    push (Context.UnsolvedType existential)

                                    let element = Type.UnsolvedType
                                            { location = assignmentLocation
                                            , existential
                                            }

                                    let list = Type.List
                                            { location = assignmentLocation
                                            , type_ = element
                                            }

                                    _ <- check value list `Exception.catch` \(_ :: TypeInferenceError) -> do
                                        set context

                                        Exception.throwIO (MonadMismatch (Syntax.location value))

                                    set context

                                    let annotation₂ = Type.List
                                            { location = assignmentLocation
                                            , type_ = annotation₁
                                            }

                                    return annotation₂

                                Just OptionalMonad -> do
                                    context <- get

                                    existential <- fresh

                                    push (Context.UnsolvedType existential)

                                    let element = Type.UnsolvedType
                                            { location = assignmentLocation
                                            , existential
                                            }

                                    let optional = Type.Optional
                                            { location = assignmentLocation
                                            , type_ = element
                                            }

                                    _ <- check value optional `Exception.catch` \(_ :: TypeInferenceError) -> do
                                        set context

                                        Exception.throwIO (MonadMismatch (Syntax.location value))

                                    set context

                                    let annotation₂ = Type.Optional
                                            { location = assignmentLocation
                                            , type_ = annotation₁
                                            }

                                    return annotation₂

                                Just UnknownMonad -> do
                                    return annotation₁

                                Nothing -> do
                                    return annotation₁

                            newValue <- check value annotation₂

                            let newAssignment = Syntax.Bind
                                    { assignmentLocation
                                    , monad = monad₀
                                    , binding = newBinding
                                    , assignment = newValue
                                    }

                            return (newAssignment : newAssignments, newBody)

            let nil = do
                    element <- case monad₀ of
                        Just ListMonad -> do
                            context <- get

                            existential <- fresh

                            push (Context.UnsolvedType existential)

                            let element = Type.UnsolvedType
                                    { location
                                    , existential
                                    }

                            let list = Type.List
                                    { location
                                    , type_ = element
                                    }

                            subtype list (Context.solveType context annotation₀) `Exception.catch` \(_ :: TypeInferenceError) -> do
                                set context

                                Exception.throwIO (MonadMismatch (Type.location annotation₀))


                            return element

                        Just OptionalMonad -> do
                            context <- get

                            existential <- fresh

                            push (Context.UnsolvedType existential)

                            let element = Type.UnsolvedType
                                    { location
                                    , existential
                                    }

                            let optional = Type.Optional
                                    { location
                                    , type_ = element
                                    }

                            subtype optional (Context.solveType context annotation₀) `Exception.catch` \(_ :: TypeInferenceError) -> do
                                set context

                                Exception.throwIO (MonadMismatch (Type.location annotation₀))


                            return element

                        Just UnknownMonad -> do
                            return annotation₀

                        Nothing -> do
                            return annotation₀

                    context <- get

                    newBody <- check body (Context.solveType context element)

                    return ([], newBody)

            (newAssignments, newBody) <- foldr cons nil assignments

            let newLet = Syntax.Let
                    { location
                    , assignments = NonEmpty.fromList newAssignments
                    , body = newBody
                    }

            context <- get

            return (solveSyntax context newLet)

    context <- get

    assign Nothing `Exception.catch` \(_ :: MonadMismatch) -> do
        set context

        assign (Just ListMonad) `Exception.catch` \(_ :: MonadMismatch) -> do
            set context

            assign (Just OptionalMonad)

check Syntax.Alternative{ location, name, argument } annotation@Type.Union{ alternatives = Type.Alternatives alternativeTypes remainingAlternatives } = do
    existential <- fresh

    push (Context.UnsolvedAlternatives existential)

    case lookup name alternativeTypes of
        Just innerType₁ -> do
            newArgument <- check argument innerType₁

            return Syntax.Alternative{ location, name, argument = newArgument }

        Nothing -> do
            (innerType₀, newArgument) <- infer argument

            let alternatives = Type.Alternatives
                    [ (name, innerType₀) ]
                    (Monotype.UnsolvedAlternatives existential)

            case remainingAlternatives of
                Monotype.UnsolvedAlternatives p -> do
                    instantiateAlternativesR location alternatives p

                    return Syntax.Alternative
                        { location
                        , name
                        , argument = newArgument
                        }

                _ -> do
                    let actual = Type.Union{ location, alternatives }

                    Exception.throwIO (UnionTypeMismatch actual annotation [ name ])

check Syntax.Prompt{ schema = Nothing, .. } annotation = do
    newArguments <- check arguments (fmap (\_ -> location) (expected @Prompt))

    return Syntax.Prompt{ arguments = newArguments, schema = Just annotation, .. }

check Syntax.HTTP{ import_, schema = Nothing, .. } annotation = do
    let input = fmap (\_ -> location) (expected @HTTP)

    newArguments <- check arguments input

    context₀ <- get

    Monad.unless import_ do
        subtype (Context.solveType context₀ annotation) Type.Scalar{ location, scalar = Monotype.JSON }

    context₁ <- get

    return Syntax.HTTP{ arguments = newArguments, schema = Just (Context.solveType context₁ annotation), .. }

check Syntax.Read{ import_, schema = Nothing, .. } annotation = do
    newArguments <- check arguments (fmap (\_ -> location) (expected @Text))

    context₀ <- get

    Monad.unless import_ do
        subtype (Context.solveType context₀ annotation) Type.Scalar{ location, scalar = Monotype.JSON }

    context₁ <- get

    return Syntax.Read{ arguments = newArguments, schema = Just (Context.solveType context₁ annotation), .. }

check Syntax.GitHub{ import_, schema = Nothing, .. } annotation = do
    let input = fmap (\_ -> location) (expected @GitHub)

    newArguments <- check arguments input

    context₀ <- get

    Monad.unless import_ do
        subtype (Context.solveType context₀ annotation) Type.Scalar{ location, scalar = Monotype.JSON }

    context₁ <- get

    return Syntax.GitHub{ arguments = newArguments, schema = Just (Context.solveType context₁ annotation), .. }

check Syntax.Project{ location, larger, smaller = smaller@Syntax.Single{ single = Syntax.Field{ fieldLocation, field } } } annotation = do
    context <- get

    (recordType, newLarger₀) <- infer larger

    case recordType of
        Type.Record{ fields = Type.Fields fieldTypes _ }
            | Just fieldType <- lookup field fieldTypes -> do
                subtype fieldType annotation

                return Syntax.Project{ location, larger = newLarger₀, smaller }

        _ -> do
            set context

            fields <- fresh

            push (Context.UnsolvedFields fields)

            newLarger₁ <- check larger Type.Record
                { fields = Type.Fields
                    [(field, annotation)]
                    (Monotype.UnsolvedFields fields)
                , location = fieldLocation
                }

            return Syntax.Project{ location, larger = newLarger₁, smaller }

check Syntax.Project{ location, larger, smaller = smaller@Syntax.Multiple{ multiple } } Type.Record{ location = recordLocation, fields = Type.Fields fieldTypes rest }
    | let m₀ = Map.fromList do
              Syntax.Field{ field } <- multiple

              return (field, ())

    , let m₁ = Map.fromList fieldTypes

    , Map.null (Map.difference m₀ m₁) = do
        let m = Map.intersectionWith (\_ type_ -> type_) m₀ m₁

        let newAnnotation = Type.Record
                { location = recordLocation
                , fields = Type.Fields (Map.toList m) rest
                }

        newLarger <- check larger newAnnotation

        return Syntax.Project{ location, larger = newLarger, smaller }

check Syntax.Project{ location, larger, smaller = smaller@Syntax.Slice{ } } Type.Optional{ type_ } = do
    newLarger <- check larger type_

    return Syntax.Project{ location, larger = newLarger, smaller }

check Syntax.Project{ location, larger, smaller = smaller@Syntax.Index{ } } Type.Optional{ type_ } = do
    newLarger <- check larger Type.List{ location, type_ }

    return Syntax.Project{ location, larger = newLarger, smaller }

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

check Syntax.Operator{ location, left, operatorLocation, operator = Syntax.Times, right } annotation@Type.Scalar{ scalar }
    | scalar `elem` ([ Monotype.Natural, Monotype.Integer, Monotype.Real ] :: [Monotype.Scalar]) = do
    newLeft <- check left annotation

    context <- get

    newRight <- check right (Context.solveType context  annotation)

    return Syntax.Operator
        { location
        , left = newLeft
        , operatorLocation
        , operator = Syntax.Times
        , right = newRight
        }

check Syntax.Operator{ location, left, operatorLocation, operator = Syntax.Plus, right } annotation@Type.Scalar{ scalar }
    | scalar `elem` ([ Monotype.Natural, Monotype.Integer, Monotype.Real, Monotype.Text ] :: [Monotype.Scalar]) = do
    newLeft <- check left annotation

    context <- get

    newRight <- check right (Context.solveType context annotation)

    return Syntax.Operator
        { location
        , left = newLeft
        , operatorLocation
        , operator = Syntax.Plus
        , right = newRight
        }

check Syntax.Operator{ location, left, operatorLocation, operator = Syntax.Plus, right } annotation@Type.List{ } = do
    newLeft <- check left annotation

    context <- get

    newRight <- check right (Context.solveType context  annotation)

    return Syntax.Operator
        { location
        , left = newLeft
        , operatorLocation
        , operator = Syntax.Plus
        , right = newRight
        }


check Syntax.Operator{ location, left, operatorLocation, operator = Syntax.Minus, right } annotation@Type.Scalar{ scalar }
    | scalar `elem` ([ Monotype.Integer, Monotype.Real ] :: [Monotype.Scalar]) = do
    newLeft <- check left annotation

    context <- get

    newRight <- check right (Context.solveType context annotation)

    return Syntax.Operator
        { location
        , left = newLeft
        , operatorLocation
        , operator = Syntax.Minus
        , right = newRight
        }

check Syntax.If{ location, predicate, ifTrue, ifFalse } annotation = do
    newPredicate <- check predicate Type.Scalar
        { location
        , scalar = Monotype.Bool
        }

    newIfTrue <- check ifTrue annotation

    newIfFalse <- check ifFalse annotation

    return Syntax.If
        { location
        , predicate = newPredicate
        , ifTrue = newIfTrue
        , ifFalse = newIfFalse
        }

check Syntax.List{..} Type.List{ location = _, .. } = do
    let process element = do
            _Γ <- get

            check element (Context.solveType _Γ type_)

    newElements <- traverse process elements

    return Syntax.List{ elements = newElements, .. }

check e@Syntax.Record{ fieldValues = fieldValues₀ } _B@Type.Record{ fields = Type.Fields fieldTypes fields }
    | let mapValues = Map.fromList fieldValues₁
    , let mapTypes  = Map.fromList fieldTypes
    , let both = Map.intersectionWith (,) mapValues mapTypes

    -- This is to prevent an infinite loop because we're going to recursively
    -- call `check` again below with two non-intersecting records to generate a
    -- type error if the check fails
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

        let convert (name, assignment) = Syntax.Definition
                { nameLocation = Syntax.location assignment
                , name
                , bindings = []
                , annotation = Nothing
                , assignment
                }

        let e' = Syntax.Record
                { fieldValues = map convert (Map.toList extraValues)
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
            Syntax.Record{ location, fieldValues = nonOverlappingValues } ->
                return Syntax.Record
                    { location
                    , fieldValues = map convert (overlappingValues <> optionalValues) <> nonOverlappingValues
                    }
            other ->
                return other
  where
    fieldValues₁ = do
        Syntax.Definition{ nameLocation, name, bindings, annotation = annotation₀, assignment } <- fieldValues₀

        let newAssignment = case annotation₀ of
                Nothing -> assignment
                Just annotation₁ -> Syntax.Annotation
                    { location = Syntax.location assignment
                    , annotated = assignment
                    , annotation = annotation₁
                    }

        let cons binding body = Syntax.Lambda
                { location = nameLocation
                , binding
                , body
                }

        let value = foldr cons newAssignment bindings

        return (name, value)

check Syntax.Text{ chunks = Syntax.Chunks text₀ rest, .. } Type.Scalar{ scalar = Monotype.Text } = do
    let process (interpolation, text) = do
            newInterpolation <- check interpolation Type.Scalar{ scalar = Monotype.Text, .. }

            return (newInterpolation, text)

    newRest <- traverse process rest

    return Syntax.Text{ chunks = Syntax.Chunks text₀ newRest, .. }

check Syntax.List{..} annotation@Type.Scalar{ scalar = Monotype.JSON } = do
    newElements <- traverse (`check` annotation) elements

    let annotated = Syntax.List{ elements = newElements, .. }

    return Syntax.Annotation
        { annotated, annotation, location = Syntax.location annotated }

check Syntax.Record{ location, fieldValues } annotation₀@Type.Scalar{ scalar = Monotype.JSON } = do
    let process definition₀@Syntax.Definition{ bindings, annotation = annotation₁ } = do
            definition₁ <- case bindings of
                    [] -> do
                        case annotation₁ of
                            Just annotation₂ -> do
                                subtype annotation₂ annotation₀
                            Nothing -> do
                                return ()

                        return (definition₀ :: Definition Location Input){ annotation = Just annotation₀ }
                    _ -> do
                        return definition₀

            onDefinition definition₁

    result <- traverse process fieldValues

    let (_, newFieldValues) = unzip result

    return Syntax.Record{ location, fieldValues = newFieldValues }

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

check Syntax.Embed{ embedded } annotation = do
    context <- get

    input <- Reader.ask

    Reader.local (\i -> i <> embedded) do
        absolute <- Reader.ask

        Import.referentiallySane input absolute

        let mode = case Context.solveType context annotation of
                Type.Scalar{ scalar = Monotype.Text } -> AsText
                Type.Scalar{ scalar = Monotype.Key  } -> AsKey
                _                                     -> AsCode

        syntax <- liftIO (Import.resolve mode absolute)

        check syntax annotation

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
    :: Type Location
    -> Syntax Location Input
    -> Grace (Type Location, Syntax Location Void)
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
    :: MonadIO m
    => Input
    -> Syntax Location Input
    -> m (Type Location, Syntax Location Void)
typeOf input = typeWith input []

-- | Like `typeOf`, but accepts a custom type-checking `Context`
typeWith
    :: MonadIO m
    => Input
    -> Context Location
    -> Syntax Location Input
    -> m (Type Location, Syntax Location Void)
typeWith input context syntax = do
    let initialStatus = Status{ count = 0, context }

    ((_A, elaborated), Status{ context = _Δ }) <- Grace.runGrace input initialStatus (infer syntax)

    return (Context.complete _Δ _A, solveSyntax _Δ elaborated)

solveSyntax :: Context s -> Syntax s a -> Syntax s a
solveSyntax _Γ = Lens.transform (Lens.over Syntax.types (Context.solveType _Γ))

-- | Convert from JSON, inferring the value purely from the JSON data
inferJSON :: Aeson.Value -> Value
inferJSON (Aeson.Object [("contents", contents), ("tag", Aeson.String tag)]) =
    Value.Alternative tag value
  where
    value = inferJSON contents
inferJSON (Aeson.Object object) = Value.Record (Map.fromList textValues)
  where
    properties = Map.toList (Compat.fromAesonMap object)

    textValues = fmap (fmap inferJSON) properties
inferJSON (Aeson.Array vector) = Value.List (Seq.fromList (toList elements))
  where
    elements = fmap inferJSON vector
inferJSON (Aeson.String text) = Value.Text text
inferJSON (Aeson.Number scientific) =
    case Scientific.floatingOrInteger scientific of
        Left (_ :: Double) ->
            Value.Scalar (Syntax.Real scientific)
        Right (integer :: Integer)
            | 0 <= integer -> do
                Value.Scalar (Syntax.Natural (fromInteger integer))
            | otherwise -> do
                Value.Scalar (Syntax.Integer integer)
inferJSON (Aeson.Bool bool) =
    Value.Scalar (Syntax.Bool bool)
inferJSON Aeson.Null =
    Value.Scalar Syntax.Null

-- | Check an `Aeson.Value` against an expected `Type`
checkJSON :: Type Location -> Aeson.Value -> Grace Value
checkJSON = loop []
  where
    loop path Type.Union{ Type.alternatives = Type.Alternatives alternativeTypes _ } (Aeson.Object [("contents", contents), ("tag", Aeson.String tag)])
        | Just alternativeType <- Prelude.lookup tag alternativeTypes = do
            value <- loop ("contents" : path) alternativeType contents

            pure (Value.Alternative tag value)
    loop path Type.Record{ Type.fields = Type.Fields fieldTypes _ } (Aeson.Object object) = do
        let properties = Compat.fromAesonMap object

        let process (field, type_) = do
                let property = case Map.lookup field properties of
                        Just p -> p
                        Nothing -> Aeson.Null

                expression <- loop (field : path) type_ property

                return (field, expression)

        fieldValues <- traverse process fieldTypes

        pure (Value.Record (Map.fromList fieldValues))
    loop path type_@Type.Scalar{ scalar = Monotype.JSON } (Aeson.Object object) = do
        let properties = Map.toList (Compat.fromAesonMap object)

        let process (key, property) = do
                expression <- loop (key : path) type_ property

                return (key, expression)

        textValues <- traverse process properties

        pure (Value.Record (Map.fromList textValues))
    loop path Type.List{ Type.type_ } (Aeson.Array vector) = do
        elements <- traverse (loop ("*" : path) type_) vector

        pure (Value.List (Seq.fromList (toList elements)))
    loop path type_@Type.Scalar{ scalar = Monotype.JSON } (Aeson.Array vector) = do
        elements <- traverse (loop ("*" : path) type_) vector

        pure (Value.List (Seq.fromList (toList elements)))
    loop _ Type.Scalar{ scalar = Monotype.Text } (Aeson.String text) = do
        pure (Value.Text text)
    loop _ Type.Scalar{ scalar = Monotype.JSON } (Aeson.String text) = do
        pure (Value.Text text)
    loop _ Type.Scalar{ scalar = Monotype.Real } (Aeson.Number scientific) = do
        pure (Value.Scalar (Syntax.Real scientific))
    loop path type_@Type.Scalar{ scalar = Monotype.Integer } value@(Aeson.Number scientific) = do
        case Scientific.floatingOrInteger @Double @Integer scientific of
            Right integer -> do
                pure (Value.Scalar (Syntax.Integer integer))
            _ -> do
                Exception.throwIO InvalidJSON{ path, value, type_ }
    loop path type_@Type.Scalar{ scalar = Monotype.Natural } value@(Aeson.Number scientific) =
        case Scientific.floatingOrInteger @Double @Integer scientific of
            Right integer
                | 0 <= integer -> do
                    pure (Value.Scalar (Syntax.Natural (fromInteger integer)))
            _ -> do
                Exception.throwIO InvalidJSON{ path, value, type_ }
    loop _ Type.Scalar{ scalar = Monotype.JSON } (Aeson.Number scientific) =
        case Scientific.floatingOrInteger scientific of
            Left (_ :: Double) -> do
                pure (Value.Scalar (Syntax.Real scientific))
            Right (integer :: Integer)
                | 0 <= integer -> do
                    pure (Value.Scalar (Syntax.Natural (fromInteger integer)))
                | otherwise -> do
                    pure (Value.Scalar (Syntax.Integer integer))
    loop _ Type.Scalar{ Type.scalar = Monotype.Bool } (Aeson.Bool bool) =
        pure (Value.Scalar (Syntax.Bool bool))
    loop _ Type.Scalar{ Type.scalar = Monotype.JSON } (Aeson.Bool bool) =
        pure (Value.Scalar (Syntax.Bool bool))
    loop _ Type.Optional{ } Aeson.Null =
        pure (Value.Scalar Syntax.Null)
    loop path Type.Optional{ type_ } value = do
        result <- loop path type_ value

        pure (Value.Application (Value.Builtin Syntax.Some) result)
    loop _ Type.Scalar{ scalar = Monotype.JSON } Aeson.Null =
        pure (Value.Scalar Syntax.Null)
    loop _ type₀ value = do
        let bytes = Aeson.Pretty.encodePretty value

        text <- case Encoding.decodeUtf8' (ByteString.Lazy.toStrict bytes) of
            Left exception -> Exception.throwIO exception
            Right text     -> return text

        let input = Code "(json)" text

        let mode = case type₀ of
                Type.Scalar{ scalar = Monotype.Text } -> AsText
                Type.Scalar{ scalar = Monotype.Key  } -> AsKey
                _                                     -> AsCode

        expression <- liftIO (Import.resolve mode input)

        (type₁, _) <- infer expression

        context₀ <- get

        subtype (Context.solveType context₀ type₁) (Context.solveType context₀ type₀)
        let json = Type.Scalar
                { location = Type.location type₀
                , scalar = Monotype.JSON
                }

        context₁ <- get

        subtype (Context.solveType context₁ type₁) json

        return (inferJSON value)

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
    | FoldInvalidHandler Location (Type Location)
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
    --
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

data MonadMismatch = MonadMismatch Location
    deriving stock (Eq, Show)

instance Exception MonadMismatch where
    displayException (MonadMismatch location) =
        "For comprehensions mismatch\n\
        \\n\
        \" <> Text.unpack (Location.renderError "" location)

-- | Invalid JSON output which didn't match the expected type
data InvalidJSON a = InvalidJSON
    { path :: [Text]
    , value :: Aeson.Value
    , type_ :: Type a
    } deriving stock (Show)

instance (Show a, Typeable a) => Exception (InvalidJSON a) where
    displayException InvalidJSON{ path, value, type_} =
        "Invalid JSON\n\
        \\n\
        \The following JSON value:\n\
        \\n\
        \" <> string <> "\n\
        \\n\
        \… does not match the following expected type:\n\
        \\n\
        \" <> Text.unpack (Grace.Pretty.toSmart type_) <> "\n\
        \\n\
        \… at the following location:\n\
        \\n\
        \" <> Text.unpack (Text.intercalate "." (reverse path))
      where
        bytes = Aeson.Pretty.encodePretty value

        string = case Encoding.decodeUtf8' (ByteString.Lazy.toStrict bytes) of
            Left  _    -> show bytes
            Right text -> Text.unpack text

-- Helper functions for displaying errors

insert :: Pretty a => a -> String
insert a = Text.unpack (Grace.Pretty.toSmart ("  " <> Pretty.align (pretty a)))

listToText :: Pretty a => [a] -> String
listToText elements =
    Text.unpack (Text.intercalate "\n" (map prettyEntry elements))
  where
    prettyEntry entry =
        Grace.Pretty.toSmart ("• " <> Pretty.align (pretty entry))
