module Grace.Context where

import Data.Text (Text)
import Grace.Monotype (Monotype)
import Grace.Type (Type)
import Prelude hiding (lookup)

import qualified Control.Monad              as Monad
import qualified Control.Monad.State.Strict as State
import qualified Grace.Monotype             as Monotype
import qualified Grace.Type                 as Type

data Entry
    = Variable Text
    | Annotation Text Type
    | Unsolved Int
    | Solved Int Monotype
    | Marker Int
    deriving (Eq, Show)

type Context = [Entry]

solve :: Context -> Type -> Type
solve context type_ = foldl snoc type_ context
  where
    snoc t (Solved α τ) = Type.solve α τ t
    snoc t  _           = t

complete :: Context -> Type -> Type
complete context type_ = do
    State.evalState (Monad.foldM snoc type_ context) 0
  where
    snoc t (Solved α τ) = do
        return (Type.solve α τ t)
    snoc t (Unsolved α) = do
        n <- State.get

        State.put $! n + 1

        let a = Monotype.toVariable n

        return (Type.Forall a (Type.solve α (Monotype.Variable a) t))
    snoc t  _           = do
        return t

split :: Int -> Context -> Maybe (Context, Context)
split α₀ (Unsolved α₁ : entries)
    | α₀ == α₁ = return ([], entries)
split α (entry : entries) = do
    (prefix, suffix) <- split α entries
    return (entry : prefix, suffix)
split _ [] = Nothing

lookup :: Text -> Int -> Context -> Maybe Type
lookup _ _ [] =
    Nothing
lookup x₀ n (Annotation x₁ _A : _Γ) =
    if x₀ == x₁
    then
       if n <= 0
       then Just _A
       else lookup x₀ (n - 1) _Γ
    else lookup x₀ n _Γ
lookup x n (_ : _Γ) =
    lookup x n _Γ

discardUpTo :: Entry -> Context -> Context
discardUpTo entry₀ (entry₁ : _Γ)
    | entry₀ == entry₁ = _Γ
    | otherwise = discardUpTo entry₀ _Γ
discardUpTo _ [] = []
