module Grace.Type where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..))
import Grace.Monotype (Monotype)

import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Grace.Monotype as Monotype

data Type
    = Variable Text
    | Unsolved Int
    | Forall Text Type
    | Function Type Type
    | List Type
    | Bool
    deriving (Eq, Show)

instance Pretty Type where
    pretty = prettyType

fromMonotype :: Monotype -> Type
fromMonotype (Monotype.Variable α) =
    Variable α
fromMonotype (Monotype.Unsolved α) =
    Unsolved α
fromMonotype (Monotype.Function τ σ) =
    Function (fromMonotype τ) (fromMonotype σ)
fromMonotype (Monotype.List τ) =
    List (fromMonotype τ)
fromMonotype Monotype.Bool =
    Bool

solve :: Int -> Monotype -> Type -> Type
solve _ _ (Variable α) =
    Variable α
solve α₀ τ (Unsolved α₁)
    | α₀ == α₁ = fromMonotype τ
    | otherwise = Unsolved α₁
solve α₀ τ (Forall α₁ _A) =
    Forall α₁ (solve α₀ τ _A)
solve α τ (Function _A _B) =
    Function (solve α τ _A) (solve α τ _B)
solve α τ (List _A) =
    List (solve α τ _A)
solve _ _ Bool =
    Bool

substitute :: Text -> Int -> Type -> Type -> Type
substitute α₀ n _A (Variable α₁)
    | α₀ == α₁ && n == 0 = _A
    | otherwise          = Variable α₁
substitute _ _ _ (Unsolved α) =
    Unsolved α
substitute α₀ n _A₀ (Forall α₁ _A₁) =
    if α₀ == α₁
    then
        if n <= 0
        then Forall α₁ _A₁
        else Forall α₁ (substitute α₀ (n - 1) _A₀ _A₁)
    else Forall α₁ (substitute α₀ n _A₀ _A₁)
substitute α n _A₀ (Function _A₁ _B) =
    Function (substitute α n _A₀ _A₁) (substitute α n _A₀ _B)
substitute α n _A₀ (List _A₁) =
    List (substitute α n _A₀ _A₁)
substitute _ _ _ Bool =
    Bool

freeIn :: Int -> Type -> Bool
_  `freeIn` Variable _     = False
α₀ `freeIn` Unsolved α₁    = α₀ == α₁
α  `freeIn` Forall _ _A    = α `freeIn` _A
α  `freeIn` Function _A _B = α `freeIn` _A || α `freeIn` _B
α  `freeIn` List _A        = α `freeIn` _A
_  `freeIn` Bool           = False

prettyType :: Type -> Doc a
prettyType (Forall α _A) =
    "forall " <> Pretty.pretty α <> " . " <> prettyType _A
prettyType other = prettyFunctionType other

prettyFunctionType :: Type -> Doc a
prettyFunctionType (Function _A _B) =
    prettyApplicationType _A <> " -> " <> prettyFunctionType _B
prettyFunctionType other =
    prettyApplicationType other

prettyApplicationType :: Type -> Doc a
prettyApplicationType (List _A) = "List " <> prettyPrimitiveType _A
prettyApplicationType  other    = prettyPrimitiveType other

prettyPrimitiveType :: Type -> Doc a
prettyPrimitiveType (Variable α) =
    Pretty.pretty α
prettyPrimitiveType (Unsolved α) =
    Pretty.pretty (Monotype.toVariable α) <> "?"
prettyPrimitiveType Bool =
    "Bool"
prettyPrimitiveType other =
    "(" <> prettyType other <> ")"
