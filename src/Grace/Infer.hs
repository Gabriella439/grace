{-# OPTIONS_GHC -Wno-orphans #-}

module Grace.Infer where

import Data.Text (Text)

import Control.Monad.Except (MonadError(..))
import Control.Monad.State.Strict (MonadState)
import Data.String.Interpolate (__i)
import Grace.Context (Context, Entry)
import Grace.Syntax (Syntax)
import Grace.Type (Type)
import Prettyprinter (Pretty)

import qualified Control.Monad.Except      as Except
import qualified Control.Monad.State       as State
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty.Text
import qualified Data.Text                 as Text
import qualified Grace.Context             as Context
import qualified Grace.Monotype            as Monotype
import qualified Grace.Syntax              as Syntax
import qualified Grace.Type                as Type

orDie :: MonadError Text m => Maybe a -> Text -> m a
Just x  `orDie` _       = return x
Nothing `orDie` message = Except.throwError message

fresh :: MonadState Int m => m Int
fresh = do
    n <- State.get
    State.put $! n + 1
    return n

prettyToText :: Pretty a => a -> Text
prettyToText =
      Pretty.Text.renderStrict
    . Pretty.layoutPretty Pretty.defaultLayoutOptions 
    . Pretty.pretty

contextToText :: Context -> Text
contextToText entries =
    Text.unlines (map (\entry -> "• " <> entryToText entry) entries)

entryToText :: Entry -> Text
entryToText (Context.Variable α) =
    α
entryToText (Context.Unsolved α) =
    Text.pack (show α) <> "?"
entryToText (Context.Solved α τ) =
    Text.pack (show α) <> " = " <> prettyToText τ
entryToText (Context.Annotation x α) =
    x <> " : " <> prettyToText α
entryToText (Context.Marker α) =
    "➤" <> Text.pack (show α) <> "?"

wellFormedType :: MonadError Text m => Context -> Type -> m ()
-- UvarWF
wellFormedType _Γ (Type.Variable α)
    | Context.Variable α `elem` _Γ = return ()
    | otherwise                    = Except.throwError [__i|
Unbound variable: #{α}
|]
-- ArrowWF
wellFormedType _Γ (Type.Function _A _B) = do
    wellFormedType _Γ _A
    wellFormedType _Γ _B
-- ForallWF
wellFormedType _Γ (Type.Forall α _A) = do
    wellFormedType (Context.Variable α : _Γ) _A
-- EvarWF / SolvedEvarWF
wellFormedType _Γ _A@(Type.Unsolved α₀)
    | any predicate _Γ = return ()
    | otherwise        = Except.throwError [__i|
Internal error: Invalid context

The following type:

↳ #{prettyToText _A}

… is not well-formed within the following context:

#{contextToText _Γ}
|]
  where
    predicate (Context.Unsolved α₁) = α₀ == α₁
    predicate (Context.Solved α₁ _) = α₀ == α₁
    predicate  _                    = False
wellFormedType _Γ Type.Bool = do
    return ()

subtype
    :: (MonadState Int m, MonadError Text m)
    => Context -> Type -> Type -> m Context
-- <:Var
subtype _Γ (Type.Variable α₀) (Type.Variable α₁)
    | α₀ == α₁ && Context.Variable α₀ `elem` _Γ = do
        return _Γ
-- <:Exvar
subtype _Γ (Type.Unsolved α₀) (Type.Unsolved α₁)
    | α₀ == α₁ && Context.Unsolved α₀ `elem` _Γ = do
        return _Γ
-- InstantiateL
subtype _Γ (Type.Unsolved α) _A
    | not (α `Type.freeIn` _A) && elem (Context.Unsolved α) _Γ = do
        instantiateL _Γ α _A
-- InstantiateR
subtype _Γ _A (Type.Unsolved α)
    | not (α `Type.freeIn` _A) && elem (Context.Unsolved α) _Γ = do
        instantiateR _Γ _A α
-- <:→
subtype _Γ (Type.Function _A₁ _A₂) (Type.Function _B₁ _B₂) = do
    _Θ <- subtype _Γ _B₁ _A₁
    subtype _Θ (Context.solve _Θ _A₂) (Context.solve _Θ _B₂)
-- <:∀L
subtype _Γ (Type.Forall α₀ _A) _B = do
    α₁ <- fresh
    (Context.discardUpTo (Context.Marker α₁) -> _Δ) <- subtype (Context.Unsolved α₁ : Context.Marker α₁ : _Γ) (Type.substitute α₀ 0 (Type.Unsolved α₁) _A) _B
    return _Δ
-- <:∀R
subtype _Γ _A (Type.Forall α _B) = do
    (Context.discardUpTo (Context.Variable α) -> _Δ) <- subtype (Context.Variable α : _Γ) _A _B
    return _Δ
subtype _Γ Type.Bool Type.Bool = do
    return _Γ
subtype _ _A _B = Except.throwError [__i|
Not a subtype

The following type:

↳ #{prettyToText _A}

… cannot be a subtype of:

↳ #{prettyToText _B}
|]

instantiateL
    :: (MonadState Int m, MonadError Text m)
    => Context -> Int -> Type -> m Context
-- InstLReach
instantiateL _Γ₀ α (Type.Unsolved β)
    | Just (_ΓR, _Γ₁) <- Context.split β _Γ₀
    , Just (_ΓM, _ΓL) <- Context.split α _Γ₁ = do
        return (_ΓR <> (Context.Solved β (Monotype.Unsolved α) : _ΓM) <> (Context.Unsolved α : _ΓL))
-- InstLSolve
instantiateL _Γ₀ α (Type.Unsolved τ) = do
    (_Γ', _Γ)  <- Context.split α _Γ₀ `orDie` "InstLSolve"
    wellFormedType _Γ (Type.Unsolved τ)
    return (_Γ' <> (Context.Solved α (Monotype.Unsolved τ) : _Γ))
instantiateL _Γ₀ α (Type.Variable τ) = do
    (_Γ', _Γ)  <- Context.split α _Γ₀ `orDie` "InstLSolve"
    wellFormedType _Γ (Type.Variable τ)
    return (_Γ' <> (Context.Solved α (Monotype.Variable τ) : _Γ))
instantiateL _Γ₀ α Type.Bool = do
    (_Γ', _Γ)  <- Context.split α _Γ₀ `orDie` "InstLSolve"
    wellFormedType _Γ Type.Bool
    return (_Γ' <> (Context.Solved α Monotype.Bool : _Γ))
-- InstLArr
instantiateL _Γ α (Type.Function _A₁ _A₂) = do
    (_ΓR, _ΓL) <- Context.split α _Γ `orDie` "InstLArr"
    α₁ <- fresh
    α₂ <- fresh
    _Θ <- instantiateR (_ΓR <> (Context.Solved α (Monotype.Function (Monotype.Unsolved α₁) (Monotype.Unsolved α₂)) : Context.Unsolved α₁ : Context.Unsolved α₂ : _ΓL)) _A₁ α₁
    instantiateL _Θ α₂ (Context.solve _Θ _A₂)
-- InstLAllR
instantiateL _Γ α (Type.Forall β _B)
    | Context.Unsolved α `elem` _Γ = do
        (Context.discardUpTo (Context.Variable β) -> _Δ) <- instantiateL (Context.Variable β : _Γ) α _B
        return _Δ
    | otherwise = do
        Except.throwError [__i|
Internal error: Malformed context

The following unsolved variable:

↳ #{entryToText (Context.Unsolved α)}}

… cannot be instantiated because the variable is missing from the context:

#{contextToText _Γ}
|]

instantiateR
    :: (MonadState Int m, MonadError Text m)
    => Context -> Type -> Int -> m Context
-- InstRReach
instantiateR _Γ₀ (Type.Unsolved β) α
    | Just (_ΓR, _Γ₁) <- Context.split β _Γ₀
    , Just (_ΓM, _ΓL) <- Context.split α _Γ₁ = do
        return (_ΓR <> (Context.Solved β (Monotype.Unsolved α) : _ΓM) <> (Context.Unsolved α : _ΓL))
-- InstRSolve
instantiateR _Γ₀ (Type.Unsolved τ) α = do
    (_Γ', _Γ)  <- Context.split α _Γ₀ `orDie` "InstRSolve"
    wellFormedType _Γ (Type.Unsolved τ)
    return (_Γ' <> (Context.Solved α (Monotype.Unsolved τ) : _Γ))
instantiateR _Γ₀ (Type.Variable τ) α = do
    (_Γ', _Γ)  <- Context.split α _Γ₀ `orDie` "InstRSolve"
    wellFormedType _Γ (Type.Variable τ)
    return (_Γ' <> (Context.Solved α (Monotype.Variable τ) : _Γ))
instantiateR _Γ₀ Type.Bool  α = do
    (_Γ', _Γ)  <- Context.split α _Γ₀ `orDie` "InstRSolve"
    wellFormedType _Γ Type.Bool
    return (_Γ' <> (Context.Solved α Monotype.Bool  : _Γ))
-- InstRArr
instantiateR _Γ (Type.Function _A₁ _A₂) α = do
    (_ΓR, _ΓL) <- Context.split α _Γ `orDie` "InstRArr"
    α₁ <- fresh
    α₂ <- fresh
    _Θ <- instantiateL (_ΓR <> (Context.Solved α (Monotype.Function (Monotype.Unsolved α₁) (Monotype.Unsolved α₂)) : Context.Unsolved α₁ : Context.Unsolved α₂ : _ΓL)) α₁ _A₁
    instantiateR _Θ (Context.solve _Θ _A₂) α₂
-- InstRAllL
instantiateR _Γ (Type.Forall β₀ _B) α
    | Context.Unsolved α `elem` _Γ = do
        β₁ <- fresh
        (Context.discardUpTo (Context.Marker β₁) -> _Δ) <- instantiateR (Context.Unsolved β₁ : Context.Marker β₁ : _Γ) (Type.substitute β₀ 0 (Type.Unsolved β₁) _B) α
        return _Δ
    | otherwise = do
        Except.throwError [__i|
Internal error: Malformed context

The following unsolved variable:

↳ #{entryToText (Context.Unsolved α)}}

… cannot be instantiated because the variable is missing from the context:

#{contextToText _Γ}
|]

infer
    :: (MonadState Int m, MonadError Text m)
    => Context -> Syntax -> m (Type, Context)
-- Var
infer _Γ (Syntax.Variable x₀ n) = do
    _A <- Context.lookup x₀ n _Γ `orDie` ("Unbound variable: " <> x₀ <> if n == 0 then "" else "@" <> Text.pack (show n))
    return (_A, _Γ)
infer _Γ (Syntax.Lambda x e) = do
    α <- fresh
    β <- fresh
    (Context.discardUpTo (Context.Annotation x (Type.Unsolved α)) -> _Δ) <- check (Context.Annotation x (Type.Unsolved α) : Context.Unsolved β : Context.Unsolved α : _Γ) e (Type.Unsolved β)
    return (Type.Function (Type.Unsolved α) (Type.Unsolved β), _Δ)
-- →E
infer _Γ (Syntax.Application e₁ e₂) = do
    (_A, _Θ) <- infer _Γ e₁
    inferApplication _Θ (Context.solve _Θ _A) e₂
-- Anno
infer _Γ (Syntax.Annotation e _A) = do
    wellFormedType _Γ _A
    _Δ <- check _Γ e _A
    return (_A, _Δ)
-- let
infer _Γ₀ (Syntax.Let x a b) = do
    (_A, _Γ₁) <- infer _Γ₀ a
    infer (Context.Annotation x _A : _Γ₁) b
-- if
infer _Γ₀ (Syntax.If predicate l r) = do
    _Γ₁ <- check _Γ₀ predicate Type.Bool
    (_L₀, _Γ₂) <- infer _Γ₁ l
    let _L₁ = Context.solve _Γ₂ _L₀
    _Γ₃ <- check _Γ₂ r _L₁
    return (_L₁, _Γ₃)
infer _Γ₀ (Syntax.And l r) = do
    _Γ₁ <- check _Γ₀ l Type.Bool
    _Γ₂ <- check _Γ₁ r Type.Bool
    return (Type.Bool, _Γ₂)
infer _Γ₀ (Syntax.Or l r) = do
    _Γ₁ <- check _Γ₀ l Type.Bool
    _Γ₂ <- check _Γ₁ r Type.Bool
    return (Type.Bool, _Γ₂)
infer _Γ Syntax.True = do
    return (Type.Bool, _Γ)
infer _Γ Syntax.False = do
    return (Type.Bool, _Γ)

check
    :: (MonadState Int m, MonadError Text m)
    => Context -> Syntax -> Type -> m Context
-- →I
check _Γ (Syntax.Lambda x e) (Type.Function _A _B) = do
    (Context.discardUpTo (Context.Annotation x _A) -> _Δ) <- check (Context.Annotation x _A : _Γ) e _B
    return _Δ
-- ∀I
check _Γ e (Type.Forall α _A) = do
    (Context.discardUpTo (Context.Variable α) -> _Δ) <- check (Context.Variable α : _Γ) e _A
    return _Δ
-- Sub
check _Γ e _B = do
    (_A, _Θ) <- infer _Γ e
    subtype _Θ (Context.solve _Θ _A) (Context.solve _Θ _B)

inferApplication
    :: (MonadState Int m, MonadError Text m)
    => Context -> Type -> Syntax -> m (Type, Context)
-- ∀App
inferApplication _Γ (Type.Forall α₀ _A) e = do
    α₁ <- fresh
    inferApplication (Context.Unsolved α₁ : _Γ) (Type.substitute α₀ 0 (Type.Unsolved α₁) _A) e
-- αApp
inferApplication _Γ (Type.Unsolved α) e = do
    (_ΓR, _ΓL) <- Context.split α _Γ `orDie` "αApp"
    α₁ <- fresh
    α₂ <- fresh
    _Δ <- check (_ΓR <> (Context.Solved α (Monotype.Function (Monotype.Unsolved α₁) (Monotype.Unsolved α₂)) : Context.Unsolved α₁ : Context.Unsolved α₂ : _ΓL)) e (Type.Unsolved α₁)
    return (Type.Unsolved α₂, _Δ)
inferApplication _Γ (Type.Function _A _C) e = do
    _Δ <- check _Γ e _A
    return (_C, _Δ)
inferApplication _ (Type.Variable α) _ = do
    Except.throwError [__i|
Internal error: Unexpected type variable in function type

The following type variable:

↳ #{α}

… should have been replaced with an unsolved variable when type-checking
function application.
|]
inferApplication _ _A _ = do
    Except.throwError [__i|
Not a function type

An expression of the following type:

↳ #{prettyToText _A}

… was invoked as if it were a function, but the above type is not a function
type.
|]

typeOf :: Syntax -> Either Text Type
typeOf syntax = do
    (_A, _Δ) <- State.evalStateT (infer [] syntax) 0
    return (Context.complete _Δ _A)
