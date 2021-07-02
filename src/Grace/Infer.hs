{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

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
-}
module Grace.Infer
    ( -- * Type inference
      typeOf

    , -- * Internal implementation
      Status(..)
    , wellFormedType
    , subtype
    , instantiateTypeL
    , instantiateTypeR
    , infer
    , check
    , inferApplication
    ) where

import Data.Text (Text)

import Control.Applicative ((<|>))
import Control.Monad.Except (MonadError(..))
import Control.Monad.State.Strict (MonadState)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.String.Interpolate (__i)
import Grace.Context (Context, Entry)
import Grace.Existential (Existential)
import Grace.Location (Location(..))
import Grace.Monotype (Monotype)
import Grace.Syntax (Syntax(Syntax))
import Grace.Type (Type(..))
import Grace.Value (Value)
import Prettyprinter (Pretty)

import qualified Control.Monad             as Monad
import qualified Control.Monad.Except      as Except
import qualified Control.Monad.State       as State
import qualified Data.Map                  as Map
import qualified Data.Text                 as Text
import qualified Grace.Context             as Context
import qualified Grace.Domain              as Domain
import qualified Grace.Location            as Location
import qualified Grace.Monotype            as Monotype
import qualified Grace.Syntax              as Syntax
import qualified Grace.Type                as Type
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty.Text

-- | Type-checking state
data Status = Status
    { count :: !Int
      -- ^ Used to generate fresh unsolved variables (e.g. α̂, β̂)
    , context :: Context Location
      -- ^ The type-checking context (e.g. Γ, Δ, Θ)
    }

orDie :: MonadError Text m => Maybe a -> Text -> m a
Just x  `orDie` _       = return x
Nothing `orDie` message = Except.throwError message

fresh :: MonadState Status m => m (Existential a)
fresh = do
    Status{ count = n, .. } <- State.get

    State.put $! Status{ count = n + 1, .. }

    return (fromIntegral n)

push :: MonadState Status m => Entry Location -> m ()
push entry = State.modify (\s -> s { context = entry : context s })

get :: MonadState Status m => m (Context Location)
get = fmap context State.get

set :: MonadState Status m => Context Location -> m ()
set context = State.modify (\s -> s{ context })

discardUpTo :: MonadState Status m => Entry Location -> m ()
discardUpTo entry =
    State.modify (\s -> s { context = Context.discardUpTo entry (context s) })

prettyToText :: Pretty a => a -> Text
prettyToText =
      Pretty.Text.renderStrict
    . Pretty.layoutPretty Pretty.defaultLayoutOptions 
    . Pretty.pretty

listToText :: Pretty a => [a] -> Text
listToText elements =
    Text.intercalate "\n" (map (\entry -> "• " <> prettyToText entry) elements)

{-| This corresponds to the judgment:

    > Γ ⊢ A

    … which checks that under context Γ, type A is well-formed
-}
wellFormedType :: MonadError Text m => Context Location -> Type Location -> m ()
wellFormedType _Γ Type{..} =
    case node of
        -- UvarWF
        Type.VariableType α
            | Context.Variable Domain.Type α `elem` _Γ -> do
                return ()
            | otherwise -> do
                Except.throwError [__i|
                Unbound type variable: #{α}

                #{Location.renderError "" location}
                |]

        -- ArrowWF
        Type.Function _A _B -> do
            wellFormedType _Γ _A
            wellFormedType _Γ _B

        -- ForallWF
        Type.Forall _ α domain _A -> do
            wellFormedType (Context.Variable domain α : _Γ) _A

        -- ForallWF
        Type.Exists _ α domain _A -> do
            wellFormedType (Context.Variable domain α : _Γ) _A

        -- EvarWF / SolvedEvarWF
        _A@(Type.UnsolvedType α₀)
            | any predicate _Γ -> do
                return ()
            | otherwise -> do
                Except.throwError [__i|
                Internal error: Invalid context

                The following type:

                ↳ #{prettyToText _A}

                … is not well-formed within the following context:

                #{listToText _Γ}

                #{Location.renderError "" location}
                |]
          where
            predicate (Context.UnsolvedType α₁  ) = α₀ == α₁
            predicate (Context.SolvedType   α₁ _) = α₀ == α₁
            predicate  _                          = False

        Type.Optional _A -> do
            wellFormedType _Γ _A

        Type.List _A -> do
            wellFormedType _Γ _A

        Type.Record (Type.Fields kAs Monotype.EmptyFields) -> do
            traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs

        Type.Record (Type.Fields kAs (Monotype.UnsolvedFields α₀))
            | any predicate _Γ -> do
                traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
            | otherwise -> do
                Except.throwError [__i|
                Internal error: Invalid context

                The following unsolved fields variable:

                ↳ #{prettyToText (Context.UnsolvedFields α₀)}

                … is not well-formed within the following context:

                #{listToText _Γ}

                #{Location.renderError "" location}
                |]
          where
            predicate (Context.UnsolvedFields α₁  ) = α₀ == α₁
            predicate (Context.SolvedFields   α₁ _) = α₀ == α₁
            predicate  _                            = False

        Type.Record (Type.Fields kAs (Monotype.VariableFields α₀))
            | any predicate _Γ -> do
                traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
            | otherwise -> do
                Except.throwError [__i|
                Internal error: Invalid context

                The following fields variable:

                ↳ #{prettyToText (Context.Variable Domain.Fields α₀)}

                … is not well-formed within the following context:

                #{listToText _Γ}

                #{Location.renderError "" location}
                |]
          where
            predicate (Context.Variable Domain.Fields α₁) = α₀ == α₁
            predicate  _                                  = False

        Type.Union (Type.Alternatives kAs Monotype.EmptyAlternatives) -> do
            traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs

        Type.Union (Type.Alternatives kAs (Monotype.UnsolvedAlternatives α₀))
            | any predicate _Γ -> do
                traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
            | otherwise -> do
                Except.throwError [__i|
                Internal error: Invalid context

                The following unsolved alternatives variable:

                ↳ #{prettyToText (Context.UnsolvedAlternatives α₀)}

                … is not well-formed within the following context:

                #{listToText _Γ}

                #{Location.renderError "" location}
                |]
          where
            predicate (Context.UnsolvedAlternatives α₁  ) = α₀ == α₁
            predicate (Context.SolvedAlternatives   α₁ _) = α₀ == α₁
            predicate  _                                  = False

        Type.Union (Type.Alternatives kAs (Monotype.VariableAlternatives α₀))
            | any predicate _Γ -> do
                traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
            | otherwise -> do
                Except.throwError [__i|
                Internal error: Invalid context

                The following alternatives variable:

                ↳ #{prettyToText (Context.Variable Domain.Alternatives α₀)}

                … is not well-formed within the following context:

                #{listToText _Γ}

                #{Location.renderError "" location}
                |]
          where
            predicate (Context.Variable Domain.Alternatives α₁) = α₀ == α₁
            predicate  _                             = False

        Type.Scalar _ -> do
            return ()

{-| This corresponds to the judgment:

    > Γ ⊢ A <: B ⊣ Δ

    … which updates the context Γ to produce the new context Δ, given that the
    type A is a subtype of type B.
-}
subtype
    :: (MonadState Status m, MonadError Text m)
    => Type Location -> Type Location -> m ()
subtype _A₀ _B₀ = do
    _Γ <- get

    let locA₀ = Location.renderError "" (Type.location _A₀)
    let locB₀ = Location.renderError "" (Type.location _B₀)

    case (Type.node _A₀, Type.node _B₀) of
        -- <:Var
        (Type.VariableType α₀, Type.VariableType α₁)
            | α₀ == α₁ -> do
                wellFormedType _Γ _A₀

        -- <:Exvar
        (Type.UnsolvedType α₀, Type.UnsolvedType α₁)
            | α₀ == α₁ && Context.UnsolvedType α₀ `elem` _Γ -> do
                return ()

        -- InstantiateL
        (Type.UnsolvedType α, _)
            | not (α `Type.typeFreeIn` _B₀) && elem (Context.UnsolvedType α) _Γ -> do
                instantiateTypeL α _B₀

        -- InstantiateR
        (_, Type.UnsolvedType α)
            | not (α `Type.typeFreeIn` _A₀) && elem (Context.UnsolvedType α) _Γ -> do
                instantiateTypeR _A₀ α

        -- <:→
        (Type.Function _A₁ _A₂, Type.Function _B₁ _B₂) -> do
            subtype _B₁ _A₁
            _Θ <- get
            subtype (Context.solveType _Θ _A₂) (Context.solveType _Θ _B₂)

        -- <:∃R
        (_, Type.Exists nameLocation α₀ Domain.Type _B) -> do
            α₁ <- fresh

            push (Context.MarkerType α₁)
            push (Context.UnsolvedType α₁)

            let α₁' = Type{ location = nameLocation, node = Type.UnsolvedType α₁ }
            subtype _A₀ (Type.substituteType α₀ 0 α₁' _B)

            discardUpTo (Context.MarkerType α₁)
        (_, Type.Exists _ α₀ Domain.Fields _B) -> do
            α₁ <- fresh

            push (Context.MarkerFields   α₁)
            push (Context.UnsolvedFields α₁)

            let α₁' = Type.Fields [] (Monotype.UnsolvedFields α₁)
            subtype _A₀ (Type.substituteFields α₀ 0 α₁' _B)

            discardUpTo (Context.MarkerFields α₁)
        (_, Type.Exists _ α₀ Domain.Alternatives _B) -> do
            α₁ <- fresh

            push (Context.MarkerAlternatives α₁)
            push (Context.UnsolvedAlternatives α₁)

            let α₁' = Type.Alternatives [] (Monotype.UnsolvedAlternatives α₁)
            subtype _A₀ (Type.substituteAlternatives α₀ 0 α₁' _B)

            discardUpTo (Context.MarkerAlternatives α₁)

        -- <:∀L
        (Type.Forall nameLocation α₀ Domain.Type _A, _) -> do
            α₁ <- fresh

            push (Context.MarkerType α₁)
            push (Context.UnsolvedType α₁)

            let α₁' = Type{ location = nameLocation, node = Type.UnsolvedType α₁ }
            subtype (Type.substituteType α₀ 0 α₁' _A) _B₀

            discardUpTo (Context.MarkerType α₁)
        (Type.Forall _ α₀ Domain.Fields _A, _) -> do
            α₁ <- fresh

            push (Context.MarkerFields   α₁)
            push (Context.UnsolvedFields α₁)

            let α₁' = Type.Fields [] (Monotype.UnsolvedFields α₁)
            subtype (Type.substituteFields α₀ 0 α₁' _A) _B₀

            discardUpTo (Context.MarkerFields α₁)
        (Type.Forall _ α₀ Domain.Alternatives _A, _) -> do
            α₁ <- fresh

            push (Context.MarkerAlternatives α₁)
            push (Context.UnsolvedAlternatives α₁)

            let α₁' = Type.Alternatives [] (Monotype.UnsolvedAlternatives α₁)
            subtype (Type.substituteAlternatives α₀ 0 α₁' _A) _B₀

            discardUpTo (Context.MarkerAlternatives α₁)

        -- <:∃L
        (Type.Exists _ α domain _A, _) -> do
            push (Context.Variable domain α)

            subtype _A _B₀

            discardUpTo (Context.Variable domain α)

        -- <:∀R
        (_, Type.Forall _ α domain _B) -> do
            push (Context.Variable domain α)

            subtype _A₀ _B

            discardUpTo (Context.Variable domain α)

        (Type.Scalar s₀, Type.Scalar s₁)
            | s₀ == s₁ -> do
                return ()
            
        (Type.Scalar Monotype.Natural, Type.Scalar Monotype.Integer) -> do
            return ()

        (Type.Scalar Monotype.Natural, Type.Scalar Monotype.Double) -> do
            return ()

        (Type.Scalar Monotype.Integer, Type.Scalar Monotype.Double) -> do
            return ()

        (Type.Optional _A, Type.Optional _B) -> do
            subtype _A _B

        (_, Type.Optional _B) -> do
            subtype _A₀ _B

        (Type.List _A, Type.List _B) -> do
            subtype _A _B

        (_A@(Type.Record (Type.Fields kAs₀ fields₀)), _B@(Type.Record (Type.Fields kBs₀ fields₁))) -> do
            let mapA = Map.fromList kAs₀
            let mapB = Map.fromList kBs₀

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            -- TODO: The `fields* /= Monotype.EmptyFields` might not be correct
            --
            -- See also the matching `check` code.
            let okayA = Map.null extraA || (fields₁ /= Monotype.EmptyFields && fields₀ /= fields₁)
            let okayB = Map.null extraB || (fields₀ /= Monotype.EmptyFields && fields₀ /= fields₁)

            if | not okayA && not okayB -> do
                Except.throwError [__i|
                Record type mismatch

                The following record type:

                ↳ #{prettyToText _A₀}

                #{locA₀}

                … is not a subtype of the following record type:

                ↳ #{prettyToText _B₀}

                #{locB₀}

                The former record has the following extra fields:

                #{listToText (Map.keys extraA)}

                … while the latter record has the following extra fields:

                #{listToText (Map.keys extraB)}
                |]

               | not okayA -> do
                Except.throwError [__i|
                Record type mismatch

                The following record type:

                ↳ #{prettyToText _A₀}

                #{locA₀}

                … is not a subtype of the following record type:

                ↳ #{prettyToText _B₀}

                #{locB₀}

                The former record has the following extra fields:

                #{listToText (Map.keys extraA)}
                |]

               | not okayB -> do
                Except.throwError [__i|
                Record type mismatch

                The following record type:

                ↳ #{prettyToText _A₀}

                #{locA₀}

                … is not a subtype of the following record type:

                ↳ #{prettyToText _B₀}

                #{locB₀}

                The latter record has the following extra fields:

                #{listToText (Map.keys extraB)}
                |]

               | otherwise -> do
                   return ()

            let process (_A₁, _B₁) = do
                    _Θ <- get
                    subtype (Context.solveType _Θ _A₁) (Context.solveType _Θ _B₁)

            _ <- traverse process both

            case (fields₀, fields₁) of
                _ | fields₀ == fields₁ -> do
                        return ()

                (Monotype.UnsolvedFields ρ₀, Monotype.UnsolvedFields ρ₁) -> do
                    ρ₂ <- fresh

                    _Γ₀ <- get

                    let ρ₀First = do
                            (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields ρ₀ _Γ₀

                            Monad.guard (Context.UnsolvedFields ρ₁ `elem` _ΓR)

                            return (set (_ΓR <> (Context.UnsolvedFields ρ₀ : Context.UnsolvedFields ρ₂ : _ΓL)))

                    let ρ₁First = do
                            (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields ρ₁ _Γ₀

                            Monad.guard (Context.UnsolvedFields ρ₀ `elem` _ΓR)

                            return (set (_ΓR <> (Context.UnsolvedFields ρ₁ : Context.UnsolvedFields ρ₂ : _ΓL)))

                    case ρ₀First <|> ρ₁First of
                        Nothing -> do
                            Except.throwError [__i|
                            Internal error: Invalid context

                            One of the following fields variables:

                            #{listToText [Context.UnsolvedFields ρ₀, Context.UnsolvedFields ρ₁ ]}

                            … is missing from the following context:

                            #{listToText _Γ}

                            #{locA₀}

                            #{locB₀}
                            |]

                        Just setContext -> do
                            setContext

                    _Θ <- get

                    instantiateFieldsL ρ₀ (Type.location _B₀) (Context.solveRecord _Θ (Type.Fields (Map.toList extraB) (Monotype.UnsolvedFields ρ₂)))

                    _Δ <- get

                    instantiateFieldsR (Type.location _A₀) (Context.solveRecord _Δ (Type.Fields (Map.toList extraA) (Monotype.UnsolvedFields ρ₂))) ρ₁

                (Monotype.UnsolvedFields ρ₀, _) -> do
                    _Θ <- get

                    instantiateFieldsL ρ₀ (Type.location _B₀) (Context.solveRecord _Θ (Type.Fields (Map.toList extraB) fields₁))

                (_, Monotype.UnsolvedFields ρ₁) -> do
                    _Θ <- get

                    instantiateFieldsR (Type.location _A₀) (Context.solveRecord _Θ (Type.Fields (Map.toList extraA) fields₀)) ρ₁

                (_, _) -> do
                    Except.throwError [__i|
                    Not a record subtype

                    The following type:

                    ↳ #{prettyToText _A}

                    #{locA₀}

                    … cannot be a subtype of:

                    ↳ #{prettyToText _B}

                    #{locB₀}
                    |]

        (_A@(Type.Union (Type.Alternatives kAs₀ alternatives₀)), _B@(Type.Union (Type.Alternatives kBs₀ alternatives₁))) -> do
            let mapA = Map.fromList kAs₀
            let mapB = Map.fromList kBs₀

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            let okayA = Map.null extraA || alternatives₁ /= Monotype.EmptyAlternatives
            let okayB = Map.null extraB || alternatives₀ /= Monotype.EmptyAlternatives

            if | not okayA && not okayB -> do
                Except.throwError [__i|
                Union type mismatch

                The following union type:

                ↳ #{prettyToText _A₀}

                #{locA₀}

                … is not a subtype of the following union type:

                ↳ #{prettyToText _B₀}

                #{locB₀}

                The former union has the following extra alternatives:

                #{listToText (Map.keys extraA)}

                … while the latter union has the following extra alternatives:

                #{listToText (Map.keys extraB)}
                |]

               | not okayA && okayB -> do
                Except.throwError [__i|
                Union type mismatch

                The following union type:

                ↳ #{prettyToText _A₀}

                #{locA₀}

                … is not a subtype of the following union type:

                ↳ #{prettyToText _B₀}

                #{locB₀}

                The former union has the following extra alternatives:

                #{listToText (Map.keys extraA)}
                |]

               | okayA && not okayB -> do
                Except.throwError [__i|
                Union type mismatch

                The following union type:

                ↳ #{prettyToText _A₀}

                #{locA₀}

                … is not a subtype of the following union type:

                ↳ #{prettyToText _B₀}

                #{locB₀}

                The latter union has the following extra alternatives:

                #{listToText (Map.keys extraB)}
                |]

               | otherwise -> do
                return ()

            let process (_A₁, _B₁) = do
                    _Θ <- get
                    subtype (Context.solveType _Θ _A₁) (Context.solveType _Θ _B₁)

            _ <- traverse process both

            case (alternatives₀, alternatives₁) of
                (Monotype.UnsolvedAlternatives ρ₀, Monotype.UnsolvedAlternatives ρ₁) -> do
                    ρ₂ <- fresh

                    _Γ₀ <- get

                    let ρ₀First = do
                            (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives ρ₀ _Γ₀

                            Monad.guard (Context.UnsolvedAlternatives ρ₁ `elem` _ΓR)

                            return (set (_ΓR <> (Context.UnsolvedAlternatives ρ₀ : Context.UnsolvedAlternatives ρ₂ : _ΓL)))

                    let ρ₁First = do
                            (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives ρ₁ _Γ₀

                            Monad.guard (Context.UnsolvedAlternatives ρ₀ `elem` _ΓR)

                            return (set (_ΓR <> (Context.UnsolvedAlternatives ρ₁ : Context.UnsolvedAlternatives ρ₂ : _ΓL)))

                    case ρ₀First <|> ρ₁First of
                        Nothing -> do
                            Except.throwError [__i|
                            Internal error: Invalid context

                            One of the following alternatives variables:

                            #{listToText [Context.UnsolvedAlternatives ρ₀, Context.UnsolvedAlternatives ρ₁ ]}

                            … is missing from the following context:

                            #{listToText _Γ}

                            #{locA₀}

                            #{locB₀}
                            |]

                        Just setContext -> do
                            setContext

                    _Θ <- get

                    instantiateAlternativesL ρ₀ (Type.location _B₀) (Context.solveUnion _Θ (Type.Alternatives (Map.toList extraB) (Monotype.UnsolvedAlternatives ρ₂)))

                    _Δ <- get

                    instantiateAlternativesR (Type.location _A₀) (Context.solveUnion _Δ (Type.Alternatives (Map.toList extraA) (Monotype.UnsolvedAlternatives ρ₂))) ρ₁

                (Monotype.EmptyAlternatives, Monotype.EmptyAlternatives) -> do
                    return ()

                (Monotype.UnsolvedAlternatives ρ₀, _) -> do
                    _Θ <- get

                    instantiateAlternativesL ρ₀ (Type.location _B₀) (Context.solveUnion _Θ (Type.Alternatives (Map.toList extraB) alternatives₁))

                (Monotype.VariableAlternatives ρ₀, Monotype.VariableAlternatives ρ₁)
                    | ρ₀ == ρ₁ -> do
                        return ()

                (_, Monotype.UnsolvedAlternatives ρ₁) -> do
                    _Θ <- get

                    instantiateAlternativesR (Type.location _A₀) (Context.solveUnion _Θ (Type.Alternatives (Map.toList extraA) alternatives₀)) ρ₁

                (_, _) -> do
                    Except.throwError [__i|
                    Not a union subtype

                    The following type:

                    ↳ #{prettyToText _A}

                    #{locA₀}

                    … cannot be a subtype of:

                    ↳ #{prettyToText _B}

                    #{locB₀}
                    |]

        (_A, _B) -> do
            Except.throwError [__i|
            Not a subtype

            The following type:

            ↳ #{prettyToText _A}

            #{locA₀}

            … cannot be a subtype of:

            ↳ #{prettyToText _B}

            #{locB₀}
            |]

{-| This corresponds to the judgment:

    > Γ ⊢ α̂ :≦ A ⊣ Δ

    … which updates the context Γ to produce the new context Δ, by instantiating
    α̂ such that α̂ <: A.
-}
instantiateTypeL
    :: (MonadState Status m, MonadError Text m)
    => Existential Monotype -> Type Location -> m ()
instantiateTypeL α _A₀ = do
    _Γ₀ <- get

    (_Γ', _Γ) <- Context.splitOnUnsolvedType α _Γ₀ `orDie`
        [__i|
        Internal error: Invalid context

        The following unsolved variable:

        ↳ #{prettyToText (Context.UnsolvedType α)}}

        … cannot be instantiated because the variable is missing from the context:

        #{listToText _Γ₀}
        |]

    let instLSolve τ = do
            wellFormedType _Γ _A₀

            set (_Γ' <> (Context.SolvedType α τ : _Γ))

    case Type.node _A₀ of
        -- InstLReach
        Type.UnsolvedType β
            | let _ΓL = _Γ
            , Just (_ΓR, _ΓM) <- Context.splitOnUnsolvedType β _Γ' -> do
                set (_ΓR <> (Context.SolvedType β (Monotype.UnsolvedType α) : _ΓM) <> (Context.UnsolvedType α : _ΓL))

        -- InstLSolve
        Type.UnsolvedType β -> do
            instLSolve (Monotype.UnsolvedType β)
        Type.VariableType β -> do
            instLSolve (Monotype.VariableType β)
        Type.Scalar scalar -> do
            instLSolve (Monotype.Scalar scalar)

        -- InstLExt
        Type.Exists nameLocation β₀ Domain.Type _B -> do
            β₁ <- fresh

            push (Context.MarkerType β₁)
            push (Context.UnsolvedType β₁)

            let β₁' = Type{ location = nameLocation, node = Type.UnsolvedType β₁ }
            instantiateTypeR (Type.substituteType β₀ 0 β₁' _B) α

            discardUpTo (Context.MarkerType β₁)
        Type.Exists _ β₀ Domain.Fields _B -> do
            β₁ <- fresh

            push (Context.MarkerFields β₁)
            push (Context.UnsolvedFields β₁)

            let β₁' = Type.Fields [] (Monotype.UnsolvedFields β₁)
            instantiateTypeR (Type.substituteFields β₀ 0 β₁' _B) α

            discardUpTo (Context.MarkerFields β₁)
        Type.Exists _ β₀ Domain.Alternatives _B -> do
            β₁ <- fresh

            push (Context.MarkerAlternatives β₁)
            push (Context.UnsolvedAlternatives β₁)

            let β₁' = Type.Alternatives [] (Monotype.UnsolvedAlternatives β₁)
            instantiateTypeR (Type.substituteAlternatives β₀ 0 β₁' _B) α

            discardUpTo (Context.MarkerAlternatives β₁)

        -- InstLArr
        Type.Function _A₁ _A₂ -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            α₁ <- fresh
            α₂ <- fresh

            set (_ΓR <> (Context.SolvedType α (Monotype.Function (Monotype.UnsolvedType α₁) (Monotype.UnsolvedType α₂)) : Context.UnsolvedType α₁ : Context.UnsolvedType α₂ : _ΓL))

            instantiateTypeR _A₁ α₁

            _Θ <- get

            instantiateTypeL α₂ (Context.solveType _Θ _A₂)

        -- InstLAllR
        Type.Forall _ β domain _B -> do
            push (Context.Variable domain β)

            instantiateTypeL α _B

            discardUpTo (Context.Variable domain β)

        Type.Optional _A -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            α₁ <- fresh

            set (_ΓR <> (Context.SolvedType α (Monotype.Optional (Monotype.UnsolvedType α₁)) : Context.UnsolvedType α₁ : _ΓL))

            instantiateTypeL α₁ _A

        Type.List _A -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            α₁ <- fresh

            set (_ΓR <> (Context.SolvedType α (Monotype.List (Monotype.UnsolvedType α₁)) : Context.UnsolvedType α₁ : _ΓL))

            instantiateTypeL α₁ _A

        Type.Record r -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            ρ <- fresh

            set (_ΓR <> (Context.SolvedType α (Monotype.Record (Monotype.Fields [] (Monotype.UnsolvedFields ρ))) : Context.UnsolvedFields ρ : _ΓL))

            instantiateFieldsL ρ (Type.location _A₀) r

        Type.Union u -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            ρ <- fresh

            set (_ΓR <> (Context.SolvedType α (Monotype.Union (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives ρ))) : Context.UnsolvedAlternatives ρ : _ΓL))

            instantiateAlternativesL ρ (Type.location _A₀) u

{-| This corresponds to the judgment:

    > Γ ⊢ A ≦: α̂ ⊣ Δ

    … which updates the context Γ to produce the new context Δ, by instantiating
    α̂ such that A :< α̂.
-}
instantiateTypeR
    :: (MonadState Status m, MonadError Text m)
    => Type Location -> Existential Monotype -> m ()
instantiateTypeR _A₀ α = do
    _Γ₀ <- get

    (_Γ', _Γ) <- Context.splitOnUnsolvedType α _Γ₀ `orDie`
        [__i|
        Internal error: Invalid context

        The following unsolved variable:

        ↳ #{prettyToText (Context.UnsolvedType α)}}

        … cannot be instantiated because the variable is missing from the context:

        #{listToText _Γ₀}
        |]

    let instRSolve τ = do
            wellFormedType _Γ _A₀

            set (_Γ' <> (Context.SolvedType α τ : _Γ))

    case Type.node _A₀ of
        -- InstRReach
        Type.UnsolvedType β
            | let _ΓL = _Γ
            , Just (_ΓR, _ΓM) <- Context.splitOnUnsolvedType β _Γ' -> do
                set (_ΓR <> (Context.SolvedType β (Monotype.UnsolvedType α) : _ΓM) <> (Context.UnsolvedType α : _ΓL))

        -- InstRSolve
        Type.UnsolvedType β -> do
            instRSolve (Monotype.UnsolvedType β)
        Type.VariableType β -> do
            instRSolve (Monotype.VariableType β)
        Type.Scalar scalar -> do
            instRSolve (Monotype.Scalar scalar)

        -- InstRArr
        Type.Function _A₁ _A₂ -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            α₁ <- fresh
            α₂ <- fresh

            set (_ΓR <> (Context.SolvedType α (Monotype.Function (Monotype.UnsolvedType α₁) (Monotype.UnsolvedType α₂)) : Context.UnsolvedType α₁ : Context.UnsolvedType α₂ : _ΓL))

            instantiateTypeL α₁ _A₁

            _Θ <- get

            instantiateTypeR (Context.solveType _Θ _A₂) α₂

        -- InstRExtL
        Type.Exists _ β domain _B -> do
            push (Context.Variable domain β)

            instantiateTypeL α _B

            discardUpTo (Context.Variable domain β)

        -- InstRAllL
        Type.Forall nameLocation β₀ Domain.Type _B -> do
            β₁ <- fresh

            push (Context.MarkerType β₁)
            push (Context.UnsolvedType β₁)

            let β₁' = Type{ location = nameLocation, node = Type.UnsolvedType β₁ }
            instantiateTypeR (Type.substituteType β₀ 0 β₁' _B) α

            discardUpTo (Context.MarkerType β₁)
        Type.Forall _ β₀ Domain.Fields _B -> do
            β₁ <- fresh

            push (Context.MarkerFields β₁)
            push (Context.UnsolvedFields β₁)

            let β₁' = Type.Fields [] (Monotype.UnsolvedFields β₁)
            instantiateTypeR (Type.substituteFields β₀ 0 β₁' _B) α

            discardUpTo (Context.MarkerFields β₁)
        Type.Forall _ β₀ Domain.Alternatives _B -> do
            β₁ <- fresh

            push (Context.MarkerAlternatives β₁)
            push (Context.UnsolvedAlternatives β₁)

            let β₁' = Type.Alternatives [] (Monotype.UnsolvedAlternatives β₁)
            instantiateTypeR (Type.substituteAlternatives β₀ 0 β₁' _B) α

            discardUpTo (Context.MarkerAlternatives β₁)

        Type.Optional _A -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            α₁ <- fresh

            set (_ΓR <> (Context.SolvedType α (Monotype.Optional (Monotype.UnsolvedType α₁)) : Context.UnsolvedType α₁ : _ΓL))

            instantiateTypeR _A α₁

        Type.List _A -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            α₁ <- fresh

            set (_ΓR <> (Context.SolvedType α (Monotype.List (Monotype.UnsolvedType α₁)) : Context.UnsolvedType α₁ : _ΓL))

            instantiateTypeR _A α₁

        Type.Record r -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            ρ <- fresh

            set (_ΓR <> (Context.SolvedType α (Monotype.Record (Monotype.Fields [] (Monotype.UnsolvedFields ρ))) : Context.UnsolvedFields ρ : _ΓL))

            instantiateFieldsR (Type.location _A₀) r ρ

        Type.Union u -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            ρ <- fresh

            set (_ΓR <> (Context.SolvedType α (Monotype.Union (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives ρ))) : Context.UnsolvedAlternatives ρ : _ΓL))

            instantiateAlternativesR (Type.location _A₀) u ρ

{- The following `equateFields` / `instantiateFieldsL` / `instantiateFieldsR`,
   `equateAlternatives` / `instantiateAlternativesL` / `instantiateAlternativesR` judgments
   are not present in the bidirectional type-checking paper.  These were added
   in order to support row polymorphism and variant polymorphism, by following
   the same general type-checking principles as the original paper.

   Note that the implementation and the user-facing terminology use the term
   fields/alternatives instead of rows/variants, respectively.
-}
equateFields
    :: (MonadState Status m, MonadError Text m)
    => Existential Monotype.Record -> Existential Monotype.Record -> m ()
equateFields ρ₀ ρ₁ = do
    _Γ₀ <- get

    let ρ₀First = do
            (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields ρ₁ _Γ₀

            Monad.guard (Context.UnsolvedFields ρ₀ `elem` _ΓL)

            return (set (_ΓR <> (Context.SolvedFields ρ₁ (Monotype.Fields [] (Monotype.UnsolvedFields ρ₀)) : _ΓL)))

    let ρ₁First = do
            (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields ρ₀ _Γ₀

            Monad.guard (Context.UnsolvedFields ρ₁ `elem` _ΓL)

            return (set (_ΓR <> (Context.SolvedFields ρ₀ (Monotype.Fields [] (Monotype.UnsolvedFields ρ₁)) : _ΓL)))

    case ρ₀First <|> ρ₁First of
        Nothing -> do
            Except.throwError [__i|
            Internal error: Invalid context

            One of the following fields variables:

            #{listToText [Context.UnsolvedFields ρ₀, Context.UnsolvedFields ρ₁ ]}

            … is missing from the following context:

            #{listToText _Γ₀}
            |]

        Just setContext -> do
            setContext

instantiateFieldsL
    :: (MonadState Status m, MonadError Text m)
    => Existential Monotype.Record -> Location -> Type.Record Location -> m ()
instantiateFieldsL ρ₀ location r@(Type.Fields kAs rest) = do
    if ρ₀ `Type.fieldsFreeIn` Type{ node = Type.Record r, .. }
        then do
            Except.throwError [__i|
            Not a fields subtype

            The following fields variable:

            ↳ #{Pretty.pretty ρ₀}

            … cannot be instantiated to the following record type:

            ↳ #{Pretty.pretty (Type.Record r)}

            #{Location.renderError "" location}

            … because the same fields variable appears within that record type.
            |]
        else return ()

    let process (k, _A) = do
            β <- fresh

            return (k, _A, β)

    kAβs <- traverse process kAs

    let βs  = map (\(_, _, β) -> Context.UnsolvedType β      ) kAβs
    let kβs = map (\(k, _, β) -> (k, Monotype.UnsolvedType β)) kAβs

    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields ρ₀ _Γ `orDie`
        [__i|
        Internal error: Invalid context

        The following unsolved fields variable:

        ↳ #{prettyToText (Context.UnsolvedFields ρ₀)}}

        … cannot be instantiated because the fields variable is missing from the
        context:

        #{listToText _Γ}
        |]

    case rest of
        Monotype.UnsolvedFields ρ₁ -> do
            ρ₂ <- fresh

            set (_ΓR <> (Context.SolvedFields ρ₀ (Monotype.Fields kβs (Monotype.UnsolvedFields ρ₂)) : Context.UnsolvedFields ρ₂ : βs <> _ΓL))

            equateFields ρ₁ ρ₂

        _ -> do
            set (_ΓR <> (Context.SolvedFields ρ₀ (Monotype.Fields kβs rest) : βs <> _ΓL))

    let instantiate (_, _A, β) = do
            _Θ <- get

            instantiateTypeL β (Context.solveType _Θ _A)

    traverse_ instantiate kAβs

instantiateFieldsR
    :: (MonadState Status m, MonadError Text m)
    => Location -> Type.Record Location -> Existential Monotype.Record -> m ()
instantiateFieldsR location r@(Type.Fields kAs rest) ρ₀ = do
    if ρ₀ `Type.fieldsFreeIn` Type{ node = Type.Record r, .. }
        then do
            Except.throwError [__i|
            Not a fields subtype

            The following fields variable:

            ↳ #{Pretty.pretty ρ₀}

            … cannot be instantiated to the following record type:

            ↳ #{Pretty.pretty (Type.Record r)}

            #{Location.renderError "" location}

            … because the same fields variable appears within that record type.
            |]
        else return ()

    let process (k, _A) = do
            β <- fresh

            return (k, _A, β)

    kAβs <- traverse process kAs

    let βs  = map (\(_, _, β) -> Context.UnsolvedType β      ) kAβs
    let kβs = map (\(k, _, β) -> (k, Monotype.UnsolvedType β)) kAβs

    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields ρ₀ _Γ `orDie`
        [__i|
        Internal error: Invalid context

        The following unsolved fields variable:

        ↳ #{prettyToText (Context.UnsolvedFields ρ₀)}}

        … cannot be instantiated because the fields variable is missing from the
        context:

        #{listToText _Γ}
        |]

    case rest of
        Monotype.UnsolvedFields ρ₁ -> do
            ρ₂ <- fresh

            set (_ΓR <> (Context.SolvedFields ρ₀ (Monotype.Fields kβs (Monotype.UnsolvedFields ρ₂)) : Context.UnsolvedFields ρ₂ : βs <> _ΓL))

            equateFields ρ₁ ρ₂

        _ -> do
            set (_ΓR <> (Context.SolvedFields ρ₀ (Monotype.Fields kβs rest) : βs <> _ΓL))

    let instantiate (_, _A, β) = do
            _Θ <- get

            instantiateTypeR (Context.solveType _Θ _A) β

    traverse_ instantiate kAβs

equateAlternatives
    :: (MonadState Status m, MonadError Text m)
    => Existential Monotype.Union-> Existential Monotype.Union -> m ()
equateAlternatives ρ₀ ρ₁ = do
    _Γ₀ <- get

    let ρ₀First = do
            (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives ρ₁ _Γ₀

            Monad.guard (Context.UnsolvedAlternatives ρ₀ `elem` _ΓL)

            return (set (_ΓR <> (Context.SolvedAlternatives ρ₁ (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives ρ₀)) : _ΓL)))

    let ρ₁First = do
            (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives ρ₀ _Γ₀

            Monad.guard (Context.UnsolvedAlternatives ρ₁ `elem` _ΓL)

            return (set (_ΓR <> (Context.SolvedAlternatives ρ₀ (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives ρ₁)) : _ΓL)))

    case ρ₀First <|> ρ₁First of
        Nothing -> do
            Except.throwError [__i|
            Internal error: Invalid context

            One of the following alternatives variables:

            #{listToText [Context.UnsolvedAlternatives ρ₀, Context.UnsolvedAlternatives ρ₁ ]}

            … is missing from the following context:

            #{listToText _Γ₀}
            |]

        Just setContext -> do
            setContext

instantiateAlternativesL
    :: (MonadState Status m, MonadError Text m)
    => Existential Monotype.Union -> Location -> Type.Union Location -> m ()
instantiateAlternativesL ρ₀ location u@(Type.Alternatives kAs rest) = do
    if ρ₀ `Type.alternativesFreeIn` Type{ node = Type.Union u, .. }
        then do
            Except.throwError [__i|
            Not an alternatives subtype

            The following alternatives variable:

            ↳ #{Pretty.pretty ρ₀}

            … cannot be instantiated to the following union type:

            ↳ #{Pretty.pretty (Type.Union u)}

            #{Location.renderError "" location}

            … because the same alternatives variable appears within that record type.
            |]
        else return ()

    let process (k, _A) = do
            β <- fresh

            return (k, _A, β)

    kAβs <- traverse process kAs

    let βs  = map (\(_, _, β) -> Context.UnsolvedType β      ) kAβs
    let kβs = map (\(k, _, β) -> (k, Monotype.UnsolvedType β)) kAβs

    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives ρ₀ _Γ `orDie`
        [__i|
        Internal error: Invalid context

        The following unsolved alternatives variable:

        ↳ #{prettyToText (Context.UnsolvedAlternatives ρ₀)}}

        … cannot be instantiated because the alternatives variable is missing from the
        context:

        #{listToText _Γ}
        |]

    case rest of
        Monotype.UnsolvedAlternatives ρ₁ -> do
            ρ₂ <- fresh

            set (_ΓR <> (Context.SolvedAlternatives ρ₀ (Monotype.Alternatives kβs (Monotype.UnsolvedAlternatives ρ₂)) : Context.UnsolvedAlternatives ρ₂ : βs <> _ΓL))

            equateAlternatives ρ₁ ρ₂

        _ -> do
            set (_ΓR <> (Context.SolvedAlternatives ρ₀ (Monotype.Alternatives kβs rest) : βs <> _ΓL))

    let instantiate (_, _A, β) = do
            _Θ <- get

            instantiateTypeL β (Context.solveType _Θ _A)

    traverse_ instantiate kAβs

instantiateAlternativesR
    :: (MonadState Status m, MonadError Text m)
    => Location -> Type.Union Location -> Existential Monotype.Union -> m ()
instantiateAlternativesR location u@(Type.Alternatives kAs rest) ρ₀ = do
    if ρ₀ `Type.alternativesFreeIn` Type{ node = Type.Union u, .. }
        then do
            Except.throwError [__i|
            Not an alternatives subtype

            The following alternatives variable:

            ↳ #{Pretty.pretty ρ₀}

            … cannot be instantiated to the following union type:

            ↳ #{Pretty.pretty (Type.Union u)}

            #{Location.renderError "" location}

            … because the same alternatives variable appears within that union type.
            |]
        else return ()

    let process (k, _A) = do
            β <- fresh

            return (k, _A, β)

    kAβs <- traverse process kAs

    let βs  = map (\(_, _, β) -> Context.UnsolvedType β      ) kAβs
    let kβs = map (\(k, _, β) -> (k, Monotype.UnsolvedType β)) kAβs

    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives ρ₀ _Γ `orDie`
        [__i|
        Internal error: Invalid context

        The following unsolved alternatives variable:

        ↳ #{prettyToText (Context.UnsolvedAlternatives ρ₀)}}

        … cannot be instantiated because the alternatives variable is missing from the
        context:

        #{listToText _Γ}
        |]

    case rest of
        Monotype.UnsolvedAlternatives ρ₁ -> do
            ρ₂ <- fresh

            set (_ΓR <> (Context.SolvedAlternatives ρ₀ (Monotype.Alternatives kβs (Monotype.UnsolvedAlternatives ρ₂)) : Context.UnsolvedAlternatives ρ₂ : βs <> _ΓL))

            equateAlternatives ρ₁ ρ₂

        _ -> do
            set (_ΓR <> (Context.SolvedAlternatives ρ₀ (Monotype.Alternatives kβs rest) : βs <> _ΓL))

    let instantiate (_, _A, β) = do
            _Θ <- get

            instantiateTypeR (Context.solveType _Θ _A) β

    traverse_ instantiate kAβs

{-| This corresponds to the judgment:

    > Γ ⊢ e ⇒ A ⊣ Δ

    … which infers the type of e under input context Γ, producing an inferred
    type of A and an updated context Δ.
-}
infer
    :: (MonadState Status m, MonadError Text m)
    => Syntax Location (Type Location, Value)
    -> m (Type Location)
infer e₀ = do
    let _Type :: Type Location
        _Type = Type
            { location = Syntax.location e₀
            , node = error "_Type: Uninitialized node field"
            }

    let a ~> b = _Type{ node = Type.Function a b }

    case Syntax.node e₀ of
        -- Var
        _A@(Syntax.Variable x₀ n) -> do
            _Γ <- get

            Context.lookup x₀ n _Γ `orDie`
                [__i|
                Unbound variable: #{prettyToText (void _A)}

                #{Location.renderError "" (Syntax.location e₀)}
                |]

        -- →I⇒ 
        Syntax.Lambda nameLocation x e -> do
            α <- fresh
            β <- fresh

            let _A = Type{ location = nameLocation, node = Type.UnsolvedType α }

            let _B =
                    Type{ location = Syntax.location e, node = Type.UnsolvedType β }

            push (Context.UnsolvedType α)
            push (Context.UnsolvedType β)
            push (Context.Annotation x _A)

            check e _B

            discardUpTo (Context.Annotation x _A)

            return _Type{ node = Type.Function _A _B }

        -- →E
        Syntax.Application e₁ e₂ -> do
            _A <- infer e₁

            _Θ <- get

            inferApplication (Context.solveType _Θ _A) e₂

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
            case xs of
                [] -> do
                    α <- fresh

                    push (Context.UnsolvedType α)

                    return _Type
                        { node = Type.List _Type{ node = Type.UnsolvedType α }
                        }
                y : ys -> do
                    _A <- infer y

                    traverse_ (`check` _A) ys

                    return _Type{ node = Type.List _A }

        Syntax.Record kvs -> do
            let process (k, v) = do
                    _A <- infer v

                    return (k, _A)

            kAs <- traverse process kvs

            return _Type{ node = Type.Record (Type.Fields kAs Monotype.EmptyFields) }

        Syntax.Alternative k -> do
            α <- fresh
            ρ <- fresh

            push (Context.UnsolvedType α)
            push (Context.UnsolvedAlternatives ρ)

            return _Type
                { node =
                    Type.Function
                        _Type{ node = Type.UnsolvedType α }
                        _Type
                            { node =
                                Type.Union
                                    (Type.Alternatives
                                        [ ( k
                                          , _Type{ node = Type.UnsolvedType α }
                                          )
                                        ]
                                        (Monotype.UnsolvedAlternatives ρ)
                                    )
                            }
                }

        Syntax.Merge record -> do
            ρ <- fresh

            push (Context.UnsolvedFields ρ)

            let _R = Type{ location = Syntax.location record, node = Type.Record (Type.Fields [] (Monotype.UnsolvedFields ρ)) }

            check record _R

            _Γ <- get

            let _R' = Context.solveType _Γ _R

            case Type.node _R' of
                Type.Record (Type.Fields keyTypes Monotype.EmptyFields) -> do
                    β <- fresh

                    push (Context.UnsolvedType β)

                    let process (key, Type{ node = Type.Function _A _B }) = do
                            _ϴ <- get

                            let β' = Type
                                    { location = Type.location _B
                                    , node = Type.UnsolvedType β
                                    }
                            subtype (Context.solveType _ϴ _B) (Context.solveType _ϴ β')

                            return (key, _A)
                        process (_, _A) = do
                            Except.throwError [__i|
                                Invalid handler

                                The merge keyword expects a record of handlers where all handlers are functions,
                                but you provided a handler of the following type:

                                ↳ #{prettyToText _A}

                                #{Location.renderError "" (Type.location _A)}

                                … which is not a function type.
                            |]

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
                                _Type{ node = Type.UnsolvedType β }
                        }

                Type.Record _ -> do
                    Except.throwError [__i|
                        Must merge a concrete record

                        The first argument to a merge expression must be a record where all fields are
                        statically known.  However, you provided an argument of type:

                        ↳ #{prettyToText _R}

                        #{Location.renderError "" (Type.location _R)}

                        … where not all fields could be inferred.
                    |]

                _ -> do
                    Except.throwError [__i|
                        Must merge a record

                        The first argument to a merge expression must be a record, but you provided an
                        expression of the following type:

                        ↳ #{prettyToText _R}

                        #{Location.renderError "" (Type.location _R)}

                        … which is not a record type.
                    |]

        Syntax.Field record location key -> do
            α <- fresh
            ρ <- fresh

            push (Context.UnsolvedType α)
            push (Context.UnsolvedFields ρ)

            check record Type
                { location
                , node =
                    Type.Record
                        (Type.Fields
                            [ ( key
                              , Type{ location , node = Type.UnsolvedType α }
                              )
                            ]
                            (Monotype.UnsolvedFields ρ)
                        )
                }

            return Type{ location, node = Type.UnsolvedType α }

        Syntax.Scalar (Syntax.Bool _) -> do
            return _Type{ node = Type.Scalar Monotype.Bool }

        Syntax.Operator l location Syntax.And r -> do
            check l Type{ location, node = Type.Scalar Monotype.Bool }
            check r Type{ location, node = Type.Scalar Monotype.Bool }

            return Type{ location, node = Type.Scalar Monotype.Bool }

        Syntax.Operator l location Syntax.Or r -> do
            check l Type{ location, node = Type.Scalar Monotype.Bool }
            check r Type{ location, node = Type.Scalar Monotype.Bool }

            return Type{ location, node = Type.Scalar Monotype.Bool }

        Syntax.If predicate l r -> do
            check predicate _Type{ node = Type.Scalar Monotype.Bool }

            _L₀ <- infer l

            _Γ  <- get

            let _L₁ = Context.solveType _Γ _L₀

            check r _L₁

            return _L₁

        Syntax.Scalar (Syntax.Double _) -> do
            return _Type{ node = Type.Scalar Monotype.Double }

        Syntax.Scalar (Syntax.Integer _) -> do
            return _Type{ node = Type.Scalar Monotype.Integer }

        Syntax.Scalar (Syntax.Natural _) -> do
            return _Type{ node = Type.Scalar Monotype.Natural }

        Syntax.Scalar Syntax.Null -> do
            α <- fresh

            push (Context.UnsolvedType α)

            return _Type
                { node = Type.Optional _Type{ node = Type.UnsolvedType α } }

        Syntax.Operator l location Syntax.Times r -> do
            check l Type{ location, node = Type.Scalar Monotype.Natural }
            check r Type{ location, node = Type.Scalar Monotype.Natural }

            return Type{ location, node = Type.Scalar Monotype.Natural }

        Syntax.Operator l location Syntax.Plus r -> do
            check l Type{ location, node = Type.Scalar Monotype.Natural }
            check r Type{ location, node = Type.Scalar Monotype.Natural }

            return Type{ location, node = Type.Scalar Monotype.Natural }

        Syntax.Builtin Syntax.DoubleShow -> do
            return
                (   _Type{ node = Type.Scalar Monotype.Double }
                ~>  _Type{ node = Type.Scalar Monotype.Text }
                )

        Syntax.Builtin Syntax.ListFold -> do
            return _Type
                { node =
                    Type.Forall (Syntax.location e₀) "a" Domain.Type _Type
                        { node =
                            Type.Forall (Syntax.location e₀) "b" Domain.Type
                                (   _Type{ node = Type.List _Type{ node = "a" } }
                                ~>  (   (   _Type{ node = "a" }
                                        ~>  ( _Type{ node = "b" }
                                            ~>  _Type{ node = "b" }
                                            )
                                        )
                                    ~>  (   _Type{ node = "b" }
                                        ~>  _Type{ node = "b" }
                                        )
                                    )
                                )
                        }
                }

        Syntax.Builtin Syntax.ListLength -> do
            return _Type
                { node =
                    Type.Forall (Syntax.location e₀) "a" Domain.Type
                        (   _Type{ node = Type.List _Type{ node = "a" } }
                        ~>  _Type{ node = Type.Scalar Monotype.Natural }
                        )
                }

        Syntax.Builtin Syntax.ListMap -> do
            return _Type
                { node =
                    Type.Forall (Syntax.location e₀) "a" Domain.Type _Type
                        { node =
                            Type.Forall (Syntax.location e₀) "b" Domain.Type
                                (   (   _Type{ node = "a" }
                                    ~>  _Type{ node = "b" }
                                    )
                                ~>  (   _Type{ node = Type.List _Type{ node = "a" } }
                                    ~>  _Type{ node = Type.List _Type{ node = "b" } }
                                    )
                                )
                        }
                }

        Syntax.Builtin Syntax.IntegerEven -> do
            return
                (   _Type{ node = Type.Scalar Monotype.Integer }
                ~>  _Type{ node = Type.Scalar Monotype.Bool }
                )

        Syntax.Builtin Syntax.IntegerOdd -> do
            return
                (   _Type{ node = Type.Scalar Monotype.Integer }
                ~>  _Type{ node = Type.Scalar Monotype.Bool }
                )

        Syntax.Builtin Syntax.NaturalFold -> do
            return _Type
                { node =
                    Type.Forall (Syntax.location e₀) "a" Domain.Type
                        (   _Type{ node = Type.Scalar Monotype.Natural }
                        ~>  (  (_Type{ node = "a" } ~> _Type{ node = "a" })
                            ~> (_Type{ node = "a" } ~> _Type{ node = "a" })
                            )
                        )
                }

        Syntax.Scalar (Syntax.Text _) -> do
            return _Type{ node = Type.Scalar Monotype.Text }

        Syntax.Operator l location Syntax.Append r -> do
            check l Type{ location, node = Type.Scalar Monotype.Text }
            check r Type{ location, node = Type.Scalar Monotype.Text }

            return Type{ location, node = Type.Scalar Monotype.Text }

        Syntax.Embed (type_, _) -> do
            return type_

{-| This corresponds to the judgment:

    > Γ ⊢ e ⇐ A ⊣ Δ

    … which checks that e has type A under input context Γ, producing an updated
    context Δ.
-}
check
    :: (MonadState Status m, MonadError Text m)
    => Syntax Location (Type Location, Value) -> Type Location -> m ()
-- →I
check Syntax{ node = Syntax.Lambda _ x e } Type{ node = Type.Function _A _B } = do
    push (Context.Annotation x _A)

    check e _B

    discardUpTo (Context.Annotation x _A)

-- ∃I
check e Type{ node = Type.Exists nameLocation α₀ Domain.Type _A } = do
    α₁ <- fresh

    push (Context.MarkerType α₁)
    push (Context.UnsolvedType α₁)

    let α₁' = Type{ location = nameLocation, node = Type.UnsolvedType α₁ }

    check e (Type.substituteType α₀ 0 α₁' _A)

    discardUpTo (Context.MarkerType α₁)
check e Type{ node = Type.Exists _ α₀ Domain.Fields _A } = do
    α₁ <- fresh

    push (Context.MarkerFields α₁)
    push (Context.UnsolvedFields α₁)

    let α₁' = Type.Fields [] (Monotype.UnsolvedFields α₁)

    check e (Type.substituteFields α₀ 0 α₁' _A)

    discardUpTo (Context.MarkerFields α₁)
check e Type{ node = Type.Exists _ α₀ Domain.Alternatives _A } = do
    α₁ <- fresh

    push (Context.MarkerAlternatives α₁)
    push (Context.UnsolvedAlternatives α₁)

    let α₁' = Type.Alternatives [] (Monotype.UnsolvedAlternatives α₁)

    check e (Type.substituteAlternatives α₀ 0 α₁' _A)

    discardUpTo (Context.MarkerAlternatives α₁)

-- ∀I
check e Type{ node = Type.Forall _ α domain _A } = do
    push (Context.Variable domain α)

    check e _A

    discardUpTo (Context.Variable domain α)

check Syntax{ node = Syntax.List elements } Type{ node = Type.List α } = do
    traverse_ (`check` α)  elements

check e@Syntax{ node = Syntax.Record keyValues } _B@Type{ node = Type.Record (Type.Fields keyTypes fields) }
    | let mapValues = Map.fromList keyValues
    , let mapTypes  = Map.fromList keyTypes

    , let extraValues = Map.difference mapValues mapTypes
    , let extraTypes  = Map.difference mapTypes  mapValues

    , let both = Map.intersectionWith (,) mapValues mapTypes
    , not (Map.null both) = do
        let process (value, type_) = check value type_

        _ <- traverse process both

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

        check e' _B'

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
    :: (MonadState Status m, MonadError Text m)
    => Type Location
    -> Syntax Location (Type Location, Value)
    -> m (Type Location)
-- ∀App
inferApplication _A₀@Type{ node = Type.Forall nameLocation α₀ Domain.Type _A } e = do
    α₁ <- fresh

    push (Context.UnsolvedType α₁)

    let α₁' = Type{ location = nameLocation, node = Type.UnsolvedType α₁ }

    inferApplication (Type.substituteType α₀ 0 α₁' _A) e
inferApplication _A₀@Type{ node = Type.Forall _ α₀ Domain.Fields _A } e = do
    α₁ <- fresh

    push (Context.UnsolvedFields α₁)

    let α₁' = Type.Fields [] (Monotype.UnsolvedFields α₁)

    inferApplication (Type.substituteFields α₀ 0 α₁' _A) e
inferApplication _A₀@Type{ node = Type.Forall _ α₀ Domain.Alternatives _A } e = do
    α₁ <- fresh

    push (Context.UnsolvedAlternatives α₁)

    let α₁' = Type.Alternatives [] (Monotype.UnsolvedAlternatives α₁)

    inferApplication (Type.substituteAlternatives α₀ 0 α₁' _A) e

-- ∃App
inferApplication _A₀@Type{ node = Type.Exists _ α domain _A } e = do
    push (Context.Variable domain α)

    _B <- inferApplication _A e

    discardUpTo (Context.Variable domain α)

    return _B

-- αApp
inferApplication Type{ node = Type.UnsolvedType α, .. } e = do
    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedType α _Γ `orDie`
        [__i|
        Internal error: Invalid context

        The following unsolved variable:

        ↳ #{prettyToText (Context.UnsolvedType α)}}

        … cannot be solved because the variable is missing from the context:

        #{listToText _Γ}
        |]

    α₁ <- fresh
    α₂ <- fresh

    set (_ΓR <> (Context.SolvedType α (Monotype.Function (Monotype.UnsolvedType α₁) (Monotype.UnsolvedType α₂)) : Context.UnsolvedType α₁ : Context.UnsolvedType α₂ : _ΓL))

    check e Type{ node = Type.UnsolvedType α₁, .. }

    return Type{ node = Type.UnsolvedType α₂, .. }
inferApplication Type{ node = Type.Function _A _C } e = do
    check e _A

    return _C
inferApplication Type{ node = Type.VariableType α, ..} _ = do
    Except.throwError [__i|
    Internal error: Unexpected type variable in function type

    The following type variable:

    ↳ #{α}

    … should have been replaced with an unsolved variable.

    #{Location.renderError "" location}
    |]
inferApplication _A@Type{..} _ = do
    Except.throwError [__i|
    Not a function type

    An expression of the following type:

    ↳ #{prettyToText _A}

    #{Location.renderError "" location}

    … was invoked as if it were a function, but the above type is not a function
    type.
    |]

-- | Infer the `Type` of the given `Syntax` tree
typeOf
    :: Syntax Location (Type Location, Value)
    -> Either Text (Type Location)
typeOf syntax = do
    let initialStatus = Status{ count = 0, context = [] }

    (_A, Status{ context = _Δ }) <- State.runStateT (infer syntax) initialStatus

    return (Context.complete _Δ _A)
