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
    , instantiateL
    , instantiateR
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
    deriving stock (Show)

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
        Type.Variable α
            | Context.Variable α `elem` _Γ -> do
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
        Type.Forall _ α _A -> do
            wellFormedType (Context.Variable α : _Γ) _A

        -- EvarWF / SolvedEvarWF
        _A@(Type.Unsolved α₀)
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
            predicate (Context.Unsolved α₁  ) = α₀ == α₁
            predicate (Context.Solved   α₁ _) = α₀ == α₁
            predicate  _                      = False

        Type.List _A -> do
            wellFormedType _Γ _A

        Type.Record (Type.Fields kAs Nothing) -> do
            traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs

        Type.Record (Type.Fields kAs (Just α₀))
            | any predicate _Γ -> do
                traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
            | otherwise -> do
                Except.throwError [__i|
                Internal error: Invalid context

                The following row type variable:

                ↳ #{prettyToText (Context.UnsolvedRow α₀)}

                … is not well-formed within the following context:

                #{listToText _Γ}

                #{Location.renderError "" location}
                |]
          where
            predicate (Context.UnsolvedRow α₁  ) = α₀ == α₁
            predicate (Context.SolvedRow   α₁ _) = α₀ == α₁
            predicate  _                         = False

        Type.Union (Type.Alternatives kAs Nothing) -> do
            traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs

        Type.Union (Type.Alternatives kAs (Just α₀))
            | any predicate _Γ -> do
                traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
            | otherwise -> do
                Except.throwError [__i|
                Internal error: Invalid context

                The following variant type variable:

                ↳ #{prettyToText (Context.UnsolvedVariant α₀)}

                … is not well-formed within the following context:

                #{listToText _Γ}

                #{Location.renderError "" location}
                |]
          where
            predicate (Context.UnsolvedVariant α₁  ) = α₀ == α₁
            predicate (Context.SolvedVariant   α₁ _) = α₀ == α₁
            predicate  _                             = False

        Type.Bool -> do
            return ()

        Type.Natural -> do
            return ()

        Type.Text -> do
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
        (Type.Variable α₀, Type.Variable α₁)
            | α₀ == α₁ && Context.Variable α₀ `elem` _Γ -> do
                return ()

        -- <:Exvar
        (Type.Unsolved α₀, Type.Unsolved α₁)
            | α₀ == α₁ && Context.Unsolved α₀ `elem` _Γ -> do
                return ()

        -- InstantiateL
        (Type.Unsolved α, _)
            | not (α `Type.typeFreeIn` _B₀) && elem (Context.Unsolved α) _Γ -> do
                instantiateL α _B₀

        -- InstantiateR
        (_, Type.Unsolved α)
            | not (α `Type.typeFreeIn` _A₀) && elem (Context.Unsolved α) _Γ -> do
                instantiateR _A₀ α

        -- <:→
        (Type.Function _A₁ _A₂, Type.Function _B₁ _B₂) -> do
            subtype _B₁ _A₁
            _Θ <- get
            subtype (Context.solve _Θ _A₂) (Context.solve _Θ _B₂)

        -- <:∀L
        (Type.Forall nameLocation α₀ _A, _) -> do
            α₁ <- fresh

            push (Context.Marker   α₁)
            push (Context.Unsolved α₁)

            let α₁' = Type{ location = nameLocation, node = Type.Unsolved α₁ }
            subtype (Type.substitute α₀ 0 α₁' _A) _B₀

            discardUpTo (Context.Marker α₁)

        -- <:∀R
        (_, Type.Forall _ α _B) -> do
            push (Context.Variable α)

            subtype _A₀ _B

            discardUpTo (Context.Variable α)

        (Type.Bool, Type.Bool) -> do
            return ()

        (Type.Natural, Type.Natural) -> do
            return ()

        (Type.Text, Type.Text) -> do
            return ()

        (Type.List _A, Type.List _B) -> do
            subtype _A _B

        (Type.Record (Type.Fields kAs₀ Nothing), Type.Record (Type.Fields kBs₀ Nothing)) -> do
            let mapA = Map.fromList kAs₀
            let mapB = Map.fromList kBs₀

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            let nullA = Map.null extraA
            let nullB = Map.null extraB

            if | not nullA && not nullB -> do
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

               | not nullA && nullB -> do
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

               | nullA && not nullB -> do
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
                let process (_A₁, _B₁) = do
                        _Θ <- get
                        subtype (Context.solve _Θ _A₁) (Context.solve _Θ _B₁)

                _ <- traverse process both

                return ()

        (Type.Record (Type.Fields kAs₀ (Just ρ)), Type.Record (Type.Fields kBs₀ Nothing)) -> do
            let mapA = Map.fromList kAs₀
            let mapB = Map.fromList kBs₀

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            if | not (Map.null extraA) -> do
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

               | otherwise -> do
                let process (_A₁, _B₁) = do
                        _Θ <- get
                        subtype (Context.solve _Θ _A₁) (Context.solve _Θ _B₁)

                _ <- traverse process both

                _Θ <- get
                instantiateRowL ρ (Type.location _B₀) (Context.solveRecord _Θ (Type.Fields (Map.toList extraB) Nothing))

        (Type.Record (Type.Fields kAs₀ Nothing), Type.Record (Type.Fields kBs₀ (Just ρ))) -> do
            let mapA = Map.fromList kAs₀
            let mapB = Map.fromList kBs₀

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            if | not (Map.null extraB) -> do
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
                let process (_A₁, _B₁) = do
                        _Θ <- get
                        subtype (Context.solve _Θ _A₁) (Context.solve _Θ _B₁)

                _ <- traverse process both

                _Θ <- get
                instantiateRowR (Type.location _A₀) (Context.solveRecord _Θ (Type.Fields (Map.toList extraA) Nothing)) ρ

        (Type.Record (Type.Fields kAs₀ (Just ρ₀)), Type.Record (Type.Fields kBs₀ (Just ρ₁))) -> do
            let mapA = Map.fromList kAs₀
            let mapB = Map.fromList kBs₀

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            let process (_A₁, _B₁) = do
                    _Θ <- get
                    subtype (Context.solve _Θ _A₁) (Context.solve _Θ _B₁)

            _ <- traverse process both

            ρ₂ <- fresh

            _Γ₀ <- get

            let ρ₀First = do
                    (_ΓR, _ΓL) <- Context.splitOnUnsolvedRow ρ₀ _Γ₀

                    Monad.guard (Context.UnsolvedRow ρ₁ `elem` _ΓR)

                    return (set (_ΓR <> (Context.UnsolvedRow ρ₀ : Context.UnsolvedRow ρ₂ : _ΓL)))

            let ρ₁First = do
                    (_ΓR, _ΓL) <- Context.splitOnUnsolvedRow ρ₁ _Γ₀

                    Monad.guard (Context.UnsolvedRow ρ₀ `elem` _ΓR)

                    return (set (_ΓR <> (Context.UnsolvedRow ρ₁ : Context.UnsolvedRow ρ₂ : _ΓL)))

            case ρ₀First <|> ρ₁First of
                Nothing -> do
                    Except.throwError [__i|
                    Internal error: Invalid context

                    One of the following row type variables:

                    #{listToText [Context.UnsolvedRow ρ₀, Context.UnsolvedRow ρ₁ ]}

                    … is missing from the following context:

                    #{listToText _Γ}

                    #{locA₀}

                    #{locB₀}
                    |]

                Just setContext -> do
                    setContext

            _Θ <- get

            instantiateRowL ρ₀ (Type.location _B₀) (Context.solveRecord _Θ (Type.Fields (Map.toList extraB) (Just ρ₂)))

            _Δ <- get

            instantiateRowR (Type.location _A₀) (Context.solveRecord _Δ (Type.Fields (Map.toList extraA) (Just ρ₂))) ρ₁

        (Type.Union (Type.Alternatives kAs₀ Nothing), Type.Union (Type.Alternatives kBs₀ Nothing)) -> do
            let mapA = Map.fromList kAs₀
            let mapB = Map.fromList kBs₀

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            let nullA = Map.null extraA
            let nullB = Map.null extraB

            if | not nullA && not nullB -> do
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

               | not nullA && nullB -> do
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

               | nullA && not nullB -> do
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
                let process (_A₁, _B₁) = do
                        _Θ <- get
                        subtype (Context.solve _Θ _A₁) (Context.solve _Θ _B₁)

                _ <- traverse process both

                return ()

        (Type.Union (Type.Alternatives kAs₀ (Just ρ)), Type.Union (Type.Alternatives kBs₀ Nothing)) -> do
            let mapA = Map.fromList kAs₀
            let mapB = Map.fromList kBs₀

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            if | not (Map.null extraA) -> do
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

               | otherwise -> do
                let process (_A₁, _B₁) = do
                        _Θ <- get
                        subtype (Context.solve _Θ _A₁) (Context.solve _Θ _B₁)

                _ <- traverse process both

                _Θ <- get
                instantiateVariantL ρ (Type.location _B₀) (Context.solveUnion _Θ (Type.Alternatives (Map.toList extraB) Nothing))

        (Type.Union (Type.Alternatives kAs₀ Nothing), Type.Union (Type.Alternatives kBs₀ (Just ρ))) -> do
            let mapA = Map.fromList kAs₀
            let mapB = Map.fromList kBs₀

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            if | not (Map.null extraB) -> do
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
                let process (_A₁, _B₁) = do
                        _Θ <- get
                        subtype (Context.solve _Θ _A₁) (Context.solve _Θ _B₁)

                _ <- traverse process both

                _Θ <- get
                instantiateVariantR (Type.location _A₀) (Context.solveUnion _Θ (Type.Alternatives (Map.toList extraA) Nothing)) ρ

        (Type.Union (Type.Alternatives kAs₀ (Just ρ₀)), Type.Union (Type.Alternatives kBs₀ (Just ρ₁))) -> do
            let mapA = Map.fromList kAs₀
            let mapB = Map.fromList kBs₀

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            let process (_A₁, _B₁) = do
                    _Θ <- get
                    subtype (Context.solve _Θ _A₁) (Context.solve _Θ _B₁)

            _ <- traverse process both

            ρ₂ <- fresh

            _Γ₀ <- get

            let ρ₀First = do
                    (_ΓR, _ΓL) <- Context.splitOnUnsolvedVariant ρ₀ _Γ₀

                    Monad.guard (Context.UnsolvedVariant ρ₁ `elem` _ΓR)

                    return (set (_ΓR <> (Context.UnsolvedVariant ρ₀ : Context.UnsolvedVariant ρ₂ : _ΓL)))

            let ρ₁First = do
                    (_ΓR, _ΓL) <- Context.splitOnUnsolvedVariant ρ₁ _Γ₀

                    Monad.guard (Context.UnsolvedVariant ρ₀ `elem` _ΓR)

                    return (set (_ΓR <> (Context.UnsolvedVariant ρ₁ : Context.UnsolvedVariant ρ₂ : _ΓL)))

            case ρ₀First <|> ρ₁First of
                Nothing -> do
                    Except.throwError [__i|
                    Internal error: Invalid context

                    One of the following variant type variables:

                    #{listToText [Context.UnsolvedVariant ρ₀, Context.UnsolvedVariant ρ₁ ]}

                    … is missing from the following context:

                    #{listToText _Γ}

                    #{locA₀}

                    #{locB₀}
                    |]

                Just setContext -> do
                    setContext

            _Θ <- get

            instantiateVariantL ρ₀ (Type.location _B₀) (Context.solveUnion _Θ (Type.Alternatives (Map.toList extraB) (Just ρ₂)))

            _Δ <- get

            instantiateVariantR (Type.location _A₀) (Context.solveUnion _Δ (Type.Alternatives (Map.toList extraA) (Just ρ₂))) ρ₁

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
instantiateL
    :: (MonadState Status m, MonadError Text m)
    => Existential Monotype -> Type Location -> m ()
instantiateL α _A₀ = do
    _Γ₀ <- get

    (_Γ', _Γ) <- Context.splitOnUnsolved α _Γ₀ `orDie`
        [__i|
        Internal error: Invalid context

        The following unsolved variable:

        ↳ #{prettyToText (Context.Unsolved α)}}

        … cannot be instantiated because the variable is missing from the context:

        #{listToText _Γ₀}
        |]

    let instLSolve τ = do
            wellFormedType _Γ _A₀

            set (_Γ' <> (Context.Solved α τ : _Γ))

    case Type.node _A₀ of
        -- InstLReach
        Type.Unsolved β
            | let _ΓL = _Γ
            , Just (_ΓR, _ΓM) <- Context.splitOnUnsolved β _Γ' -> do
                set (_ΓR <> (Context.Solved β (Monotype.Unsolved α) : _ΓM) <> (Context.Unsolved α : _ΓL))

        -- InstLSolve
        Type.Unsolved β -> do
            instLSolve (Monotype.Unsolved β)
        Type.Variable β -> do
            instLSolve (Monotype.Variable β)
        Type.Bool -> do
            instLSolve Monotype.Bool
        Type.Natural -> do
            instLSolve Monotype.Natural
        Type.Text -> do
            instLSolve Monotype.Text

        -- InstLArr
        Type.Function _A₁ _A₂ -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            α₁ <- fresh
            α₂ <- fresh

            set (_ΓR <> (Context.Solved α (Monotype.Function (Monotype.Unsolved α₁) (Monotype.Unsolved α₂)) : Context.Unsolved α₁ : Context.Unsolved α₂ : _ΓL))

            instantiateR _A₁ α₁

            _Θ <- get

            instantiateL α₂ (Context.solve _Θ _A₂)

        -- InstLAllR
        Type.Forall _ β _B -> do
            push (Context.Variable β)

            instantiateL α _B

            discardUpTo (Context.Variable β)

        Type.List _A -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            α₁ <- fresh

            set (_ΓR <> (Context.Solved α (Monotype.List (Monotype.Unsolved α₁)) : Context.Unsolved α₁ : _ΓL))

            instantiateL α₁ _A

        Type.Record r -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            ρ <- fresh

            set (_ΓR <> (Context.Solved α (Monotype.Record (Monotype.Fields [] (Just ρ))) : Context.UnsolvedRow ρ : _ΓL))

            instantiateRowL ρ (Type.location _A₀) r

        Type.Union u -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            ρ <- fresh

            set (_ΓR <> (Context.Solved α (Monotype.Union (Monotype.Alternatives [] (Just ρ))) : Context.UnsolvedVariant ρ : _ΓL))

            instantiateVariantL ρ (Type.location _A₀) u

{-| This corresponds to the judgment:

    > Γ ⊢ A ≦: α̂ ⊣ Δ

    … which updates the context Γ to produce the new context Δ, by instantiating
    α̂ such that A :< α̂.
-}
instantiateR
    :: (MonadState Status m, MonadError Text m)
    => Type Location -> Existential Monotype -> m ()
instantiateR _A₀ α = do
    _Γ₀ <- get

    (_Γ', _Γ) <- Context.splitOnUnsolved α _Γ₀ `orDie`
        [__i|
        Internal error: Invalid context

        The following unsolved variable:

        ↳ #{prettyToText (Context.Unsolved α)}}

        … cannot be instantiated because the variable is missing from the context:

        #{listToText _Γ₀}
        |]

    let instRSolve τ = do
            wellFormedType _Γ _A₀

            set (_Γ' <> (Context.Solved α τ : _Γ))

    case Type.node _A₀ of
        -- InstRReach
        Type.Unsolved β
            | let _ΓL = _Γ
            , Just (_ΓR, _ΓM) <- Context.splitOnUnsolved β _Γ' -> do
                set (_ΓR <> (Context.Solved β (Monotype.Unsolved α) : _ΓM) <> (Context.Unsolved α : _ΓL))

        -- InstRSolve
        Type.Unsolved β -> do
            instRSolve (Monotype.Unsolved β)
        Type.Variable β -> do
            instRSolve (Monotype.Variable β)
        Type.Bool -> do
            instRSolve Monotype.Bool
        Type.Natural -> do
            instRSolve Monotype.Natural
        Type.Text -> do
            instRSolve Monotype.Text

        -- InstRArr
        Type.Function _A₁ _A₂ -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            α₁ <- fresh
            α₂ <- fresh

            set (_ΓR <> (Context.Solved α (Monotype.Function (Monotype.Unsolved α₁) (Monotype.Unsolved α₂)) : Context.Unsolved α₁ : Context.Unsolved α₂ : _ΓL))

            instantiateL α₁ _A₁

            _Θ <- get

            instantiateR (Context.solve _Θ _A₂) α₂

        -- InstRAllL
        Type.Forall nameLocation β₀ _B -> do
            β₁ <- fresh

            push (Context.Marker β₁)
            push (Context.Unsolved β₁)

            let β₁' = Type{ location = nameLocation, node = Type.Unsolved β₁ }
            instantiateR (Type.substitute β₀ 0 β₁' _B) α

            discardUpTo (Context.Marker β₁)

        Type.List _A -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            α₁ <- fresh

            set (_ΓR <> (Context.Solved α (Monotype.List (Monotype.Unsolved α₁)) : Context.Unsolved α₁ : _ΓL))

            instantiateR _A α₁

        Type.Record r -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            ρ <- fresh

            set (_ΓR <> (Context.Solved α (Monotype.Record (Monotype.Fields [] (Just ρ))) : Context.UnsolvedRow ρ : _ΓL))

            instantiateRowR (Type.location _A₀) r ρ

        Type.Union u -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            ρ <- fresh

            set (_ΓR <> (Context.Solved α (Monotype.Union (Monotype.Alternatives [] (Just ρ))) : Context.UnsolvedVariant ρ : _ΓL))

            instantiateVariantR (Type.location _A₀) u ρ

{- The following `equateRows` / `instantiateRowL` / `instantiateRowR`,
   `equateVariants` / `instantiateVariantL` / `instantiateVariantR` judgments
   are not present in the bidirectional type-checking paper.  These were added
   in order to support row polymorphism and variant polymorphism, by following
   the same general type-checking principles as the original paper.
-}
equateRows
    :: (MonadState Status m, MonadError Text m)
    => Existential Monotype.Record -> Existential Monotype.Record -> m ()
equateRows ρ₀ ρ₁ = do
    _Γ₀ <- get

    let ρ₀First = do
            (_ΓR, _ΓL) <- Context.splitOnUnsolvedRow ρ₁ _Γ₀

            Monad.guard (Context.UnsolvedRow ρ₀ `elem` _ΓL)

            return (set (_ΓR <> (Context.SolvedRow ρ₁ (Monotype.Fields [] (Just ρ₀)) : _ΓL)))

    let ρ₁First = do
            (_ΓR, _ΓL) <- Context.splitOnUnsolvedRow ρ₀ _Γ₀

            Monad.guard (Context.UnsolvedRow ρ₁ `elem` _ΓL)

            return (set (_ΓR <> (Context.SolvedRow ρ₀ (Monotype.Fields [] (Just ρ₁)) : _ΓL)))

    case ρ₀First <|> ρ₁First of
        Nothing -> do
            Except.throwError [__i|
            Internal error: Invalid context

            One of the following row type variables:

            #{listToText [Context.UnsolvedRow ρ₀, Context.UnsolvedRow ρ₁ ]}

            … is missing from the following context:

            #{listToText _Γ₀}
            |]

        Just setContext -> do
            setContext

instantiateRowL
    :: (MonadState Status m, MonadError Text m)
    => Existential Monotype.Record -> Location -> Type.Record Location -> m ()
instantiateRowL ρ₀ location r@(Type.Fields kAs rest) = do
    if ρ₀ `Type.rowFreeIn` Type{ node = Type.Record r, .. }
        then do
            Except.throwError [__i|
            Not a subtype

            The following row variable:

            ↳ #{Pretty.pretty ρ₀}

            … cannot be instantiated to the following record type:

            ↳ #{Pretty.pretty (Type.Record r)}

            #{Location.renderError "" location}

            … because the same row variable appears within that record type.
            |]
        else return ()

    let process (k, _A) = do
            β <- fresh

            return (k, _A, β)

    kAβs <- traverse process kAs

    let βs  = map (\(_, _, β) -> Context.Unsolved β      ) kAβs
    let kβs = map (\(k, _, β) -> (k, Monotype.Unsolved β)) kAβs

    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedRow ρ₀ _Γ `orDie`
        [__i|
        Internal error: Invalid context

        The following unsolved row variable:

        ↳ #{prettyToText (Context.UnsolvedRow ρ₀)}}

        … cannot be instantiated because the row variable is missing from the context:

        #{listToText _Γ}
        |]

    case rest of
        Just ρ₁ -> do
            ρ₂ <- fresh

            set (_ΓR <> (Context.SolvedRow ρ₀ (Monotype.Fields kβs (Just ρ₂)) : Context.UnsolvedRow ρ₂ : βs <> _ΓL))

            equateRows ρ₁ ρ₂

        Nothing -> do
            set (_ΓR <> (Context.SolvedRow ρ₀ (Monotype.Fields kβs Nothing) : βs <> _ΓL))

    let instantiate (_, _A, β) = do
            _Θ <- get

            instantiateL β (Context.solve _Θ _A)

    traverse_ instantiate kAβs

instantiateRowR
    :: (MonadState Status m, MonadError Text m)
    => Location -> Type.Record Location -> Existential Monotype.Record -> m ()
instantiateRowR location r@(Type.Fields kAs rest) ρ₀ = do
    if ρ₀ `Type.rowFreeIn` Type{ node = Type.Record r, .. }
        then do
            Except.throwError [__i|
            Not a subtype

            The following row variable:

            ↳ #{Pretty.pretty ρ₀}

            … cannot be instantiated to the following record type:

            ↳ #{Pretty.pretty (Type.Record r)}

            #{Location.renderError "" location}

            … because the same row variable appears within that record type.
            |]
        else return ()

    let process (k, _A) = do
            β <- fresh

            return (k, _A, β)

    kAβs <- traverse process kAs

    let βs  = map (\(_, _, β) -> Context.Unsolved β      ) kAβs
    let kβs = map (\(k, _, β) -> (k, Monotype.Unsolved β)) kAβs

    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedRow ρ₀ _Γ `orDie`
        [__i|
        Internal error: Invalid context

        The following unsolved row variable:

        ↳ #{prettyToText (Context.UnsolvedRow ρ₀)}}

        … cannot be instantiated because the row variable is missing from the context:

        #{listToText _Γ}
        |]

    case rest of
        Just ρ₁ -> do
            ρ₂ <- fresh

            set (_ΓR <> (Context.SolvedRow ρ₀ (Monotype.Fields kβs (Just ρ₂)) : Context.UnsolvedRow ρ₂ : βs <> _ΓL))

            equateRows ρ₁ ρ₂

        Nothing -> do
            set (_ΓR <> (Context.SolvedRow ρ₀ (Monotype.Fields kβs Nothing) : βs <> _ΓL))

    let instantiate (_, _A, β) = do
            _Θ <- get

            instantiateR (Context.solve _Θ _A) β

    traverse_ instantiate kAβs

equateVariants
    :: (MonadState Status m, MonadError Text m)
    => Existential Monotype.Union-> Existential Monotype.Union -> m ()
equateVariants ρ₀ ρ₁ = do
    _Γ₀ <- get

    let ρ₀First = do
            (_ΓR, _ΓL) <- Context.splitOnUnsolvedVariant ρ₁ _Γ₀

            Monad.guard (Context.UnsolvedVariant ρ₀ `elem` _ΓL)

            return (set (_ΓR <> (Context.SolvedVariant ρ₁ (Monotype.Alternatives [] (Just ρ₀)) : _ΓL)))

    let ρ₁First = do
            (_ΓR, _ΓL) <- Context.splitOnUnsolvedVariant ρ₀ _Γ₀

            Monad.guard (Context.UnsolvedVariant ρ₁ `elem` _ΓL)

            return (set (_ΓR <> (Context.SolvedVariant ρ₀ (Monotype.Alternatives [] (Just ρ₁)) : _ΓL)))

    case ρ₀First <|> ρ₁First of
        Nothing -> do
            Except.throwError [__i|
            Internal error: Invalid context

            One of the following variant type variables:

            #{listToText [Context.UnsolvedVariant ρ₀, Context.UnsolvedVariant ρ₁ ]}

            … is missing from the following context:

            #{listToText _Γ₀}
            |]

        Just setContext -> do
            setContext

instantiateVariantL
    :: (MonadState Status m, MonadError Text m)
    => Existential Monotype.Union -> Location -> Type.Union Location -> m ()
instantiateVariantL ρ₀ location u@(Type.Alternatives kAs rest) = do
    if ρ₀ `Type.variantFreeIn` Type{ node = Type.Union u, .. }
        then do
            Except.throwError [__i|
            Not a subtype

            The following variant variable:

            ↳ #{Pretty.pretty ρ₀}

            … cannot be instantiated to the following union type:

            ↳ #{Pretty.pretty (Type.Union u)}

            #{Location.renderError "" location}

            … because the same variant variable appears within that record type.
            |]
        else return ()

    let process (k, _A) = do
            β <- fresh

            return (k, _A, β)

    kAβs <- traverse process kAs

    let βs  = map (\(_, _, β) -> Context.Unsolved β      ) kAβs
    let kβs = map (\(k, _, β) -> (k, Monotype.Unsolved β)) kAβs

    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedVariant ρ₀ _Γ `orDie`
        [__i|
        Internal error: Invalid context

        The following unsolved variant variable:

        ↳ #{prettyToText (Context.UnsolvedVariant ρ₀)}}

        … cannot be instantiated because the variant variable is missing from the
        context:

        #{listToText _Γ}
        |]

    case rest of
        Just ρ₁ -> do
            ρ₂ <- fresh

            set (_ΓR <> (Context.SolvedVariant ρ₀ (Monotype.Alternatives kβs (Just ρ₂)) : Context.UnsolvedVariant ρ₂ : βs <> _ΓL))

            equateVariants ρ₁ ρ₂

        Nothing -> do
            set (_ΓR <> (Context.SolvedVariant ρ₀ (Monotype.Alternatives kβs Nothing) : βs <> _ΓL))

    let instantiate (_, _A, β) = do
            _Θ <- get

            instantiateL β (Context.solve _Θ _A)

    traverse_ instantiate kAβs

instantiateVariantR
    :: (MonadState Status m, MonadError Text m)
    => Location -> Type.Union Location -> Existential Monotype.Union -> m ()
instantiateVariantR location u@(Type.Alternatives kAs rest) ρ₀ = do
    if ρ₀ `Type.variantFreeIn` Type{ node = Type.Union u, .. }
        then do
            Except.throwError [__i|
            Not a subtype

            The following variant variable:

            ↳ #{Pretty.pretty ρ₀}

            … cannot be instantiated to the following union type:

            ↳ #{Pretty.pretty (Type.Union u)}

            #{Location.renderError "" location}

            … because the same variant variable appears within that union type.
            |]
        else return ()

    let process (k, _A) = do
            β <- fresh

            return (k, _A, β)

    kAβs <- traverse process kAs

    let βs  = map (\(_, _, β) -> Context.Unsolved β      ) kAβs
    let kβs = map (\(k, _, β) -> (k, Monotype.Unsolved β)) kAβs

    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedVariant ρ₀ _Γ `orDie`
        [__i|
        Internal error: Invalid context

        The following unsolved variant variable:

        ↳ #{prettyToText (Context.UnsolvedVariant ρ₀)}}

        … cannot be instantiated because the variant variable is missing from the
        context:

        #{listToText _Γ}
        |]

    case rest of
        Just ρ₁ -> do
            ρ₂ <- fresh

            set (_ΓR <> (Context.SolvedVariant ρ₀ (Monotype.Alternatives kβs (Just ρ₂)) : Context.UnsolvedVariant ρ₂ : βs <> _ΓL))

            equateVariants ρ₁ ρ₂

        Nothing -> do
            set (_ΓR <> (Context.SolvedVariant ρ₀ (Monotype.Alternatives kβs Nothing) : βs <> _ΓL))

    let instantiate (_, _A, β) = do
            _Θ <- get

            instantiateR (Context.solve _Θ _A) β

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

            let _A = Type{ location = nameLocation, node = Type.Unsolved α }

            let _B =
                    Type{ location = Syntax.location e, node = Type.Unsolved β }

            push (Context.Unsolved α)
            push (Context.Unsolved β)
            push (Context.Annotation x _A)

            check e _B

            discardUpTo (Context.Annotation x _A)

            return _Type{ node = Type.Function _A _B }

        -- →E
        Syntax.Application e₁ e₂ -> do
            _A <- infer e₁

            _Θ <- get

            inferApplication (Context.solve _Θ _A) e₂

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

                    push (Context.Unsolved α)

                    return _Type
                        { node = Type.List _Type{ node = Type.Unsolved α }
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

            return _Type{ node = Type.Record (Type.Fields kAs Nothing) }

        Syntax.Alternative k -> do
            α <- fresh
            ρ <- fresh

            push (Context.Unsolved α)
            push (Context.UnsolvedVariant ρ)

            return _Type
                { node =
                    Type.Function
                        _Type{ node = Type.Unsolved α }
                        _Type
                            { node =
                                Type.Union
                                    (Type.Alternatives
                                        [(k, _Type{ node = Type.Unsolved α })]
                                        (Just ρ)
                                    )
                            }
                }

        Syntax.Merge record -> do
            _R <- infer record

            case Type.node _R of
                Type.Record (Type.Fields keyTypes Nothing) -> do
                    β <- fresh

                    push (Context.Unsolved β)

                    let process (key, Type{ node = Type.Function _A _B }) = do
                            _ϴ <- get

                            let β' = Type
                                    { location = Type.location _B
                                    , node = Type.Unsolved β
                                    }
                            subtype (Context.solve _ϴ _B) (Context.solve _ϴ β')

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
                                                Nothing
                                            )
                                    }
                                _Type{ node = Type.Unsolved β }
                        }

                Type.Record (Type.Fields _ (Just _)) -> do
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

            push (Context.Unsolved α)
            push (Context.UnsolvedRow ρ)

            check record Type
                { location
                , node =
                    Type.Record
                        (Type.Fields
                            [(key, Type{ location , node = Type.Unsolved α })]
                            (Just ρ)
                        )
                }

            return Type{ location, node = Type.Unsolved α }

        Syntax.True -> do
            return _Type{ node = Type.Bool }

        Syntax.False -> do
            return _Type{ node = Type.Bool }

        Syntax.And l location r -> do
            check l Type{ location, node = Type.Bool }
            check r Type{ location, node = Type.Bool }

            return Type{ location, node = Type.Bool }

        Syntax.Or l location r -> do
            check l Type{ location, node = Type.Bool }
            check r Type{ location, node = Type.Bool }

            return Type{ location, node = Type.Bool }

        Syntax.If predicate l r -> do
            check predicate _Type{ node = Type.Bool }

            _L₀ <- infer l

            _Γ  <- get

            let _L₁ = Context.solve _Γ _L₀

            check r _L₁

            return _L₁

        Syntax.Natural _ -> do
            return _Type{ node = Type.Natural }

        Syntax.Times l location r -> do
            check l Type{ location, node = Type.Natural }
            check r Type{ location, node = Type.Natural }

            return Type{ location = Syntax.location e₀, node = Type.Natural }

        Syntax.Plus l location r -> do
            check l Type{ location, node = Type.Natural }
            check r Type{ location, node = Type.Natural }

            return Type{ location, node = Type.Natural }

        Syntax.NaturalFold -> do
            return _Type
                { node =
                    Type.Forall (Syntax.location e₀) "a"
                        (   _Type{ node = Type.Natural }
                        ~>  (  (_Type{ node = "a" } ~> _Type{ node = "a" })
                            ~> (_Type{ node = "a" } ~> _Type{ node = "a" })
                            )
                        )
                }

        Syntax.Text _ -> do
            return _Type{ node = Type.Text }

        Syntax.Append l location r -> do
            check l Type{ location, node = Type.Text }
            check r Type{ location, node = Type.Text }

            return Type{ location, node = Type.Text }

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

-- ∀I
check e Type{ node = Type.Forall _ α _A } = do
    push (Context.Variable α)

    check e _A

    discardUpTo (Context.Variable α)

-- Sub
check e _B = do
    _A <- infer e

    _Θ <- get

    subtype (Context.solve _Θ _A) (Context.solve _Θ _B)

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
inferApplication _A₀@Type{ node = Type.Forall nameLocation α₀ _A } e = do
    α₁ <- fresh

    push (Context.Unsolved α₁)

    let α₁' = Type{ location = nameLocation, node = Type.Unsolved α₁ }

    inferApplication (Type.substitute α₀ 0 α₁' _A) e

-- αApp
inferApplication Type{ node = Type.Unsolved α, .. } e = do
    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolved α _Γ `orDie`
        [__i|
        Internal error: Invalid context

        The following unsolved variable:

        ↳ #{prettyToText (Context.Unsolved α)}}

        … cannot be solved because the variable is missing from the context:

        #{listToText _Γ}
        |]

    α₁ <- fresh
    α₂ <- fresh

    set (_ΓR <> (Context.Solved α (Monotype.Function (Monotype.Unsolved α₁) (Monotype.Unsolved α₂)) : Context.Unsolved α₁ : Context.Unsolved α₂ : _ΓL))

    check e Type{ node = Type.Unsolved α₁, .. }

    return Type{ node = Type.Unsolved α₂, .. }
inferApplication Type{ node = Type.Function _A _C } e = do
    check e _A

    return _C
inferApplication Type{ node = Type.Variable α, ..} _ = do
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
