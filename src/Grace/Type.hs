{-| This module stores the `Type` type representing polymorphic types and
    utilities for operating on `Type`s
-}
module Grace.Type
    ( -- * Types
      Type(..)
    , Record(..)

      -- * Utilities
    , solve
    , solveRow
    , typeFreeIn
    , rowFreeIn
    , substitute
    ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..))
import Grace.Existential (Existential)
import Grace.Monotype (Monotype)

import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Grace.Monotype as Monotype

-- | A potentially polymorphic type
data Type
    = Variable Text
    -- ^ Type variable
    --
    -- >>> pretty (Variable "a")
    -- a
    | Unsolved (Existential Monotype)
    -- ^ A placeholder variable whose type has not yet been inferred
    --
    -- >>> pretty (Unsolved 0)
    -- a?
    | Forall Text Type
    -- ^ Universally quantified type
    --
    -- >>> pretty (Forall "a" (Variable "a"))
    -- forall a . a
    | Function Type Type
    -- ^ Function type
    --
    -- >>> pretty (Function (Variable "a") (Variable "b"))
    -- a -> b
    | List Type
    -- ^ List type
    --
    -- >>> pretty (List (Variable "a"))
    -- List a
    | Record Record
    -- ^ Record type
    --
    -- >>> pretty (Record [("x", Variable "X"), ("y", Variable "Y")] Nothing)
    -- { x : X, y : Y }
    -- >>> pretty (Record [("x", Variable "X"), ("y", Variable "Y")] (Just 0))
    -- { x : X, y : Y | a }
    | Bool
    -- ^ Boolean type
    --
    -- >>> pretty Bool
    -- Bool
    deriving (Eq, Ord, Show)

-- | A potentially polymorphic record type
data Record = Fields [(Text, Type)] (Maybe (Existential Monotype.Record))
    deriving (Eq, Ord, Show)

instance Pretty Type where
    pretty = prettyType

{-| This function should not be exported or generally used.  It is only really
    safe to use within one of the @solve*@ functions
-}
fromMonotype :: Monotype -> Type
fromMonotype (Monotype.Variable α) =
    Variable α
fromMonotype (Monotype.Unsolved α) =
    Unsolved α
fromMonotype (Monotype.Function τ σ) =
    Function (fromMonotype τ) (fromMonotype σ)
fromMonotype (Monotype.List τ) =
    List (fromMonotype τ)
fromMonotype (Monotype.Record (Monotype.Fields kτs ρ)) =
    Record (Fields (map (\(k, τ) -> (k, fromMonotype τ)) kτs) ρ)
fromMonotype Monotype.Bool =
    Bool

{-| Substitute a `Type` by replacing all occurrences of the given unsolved
    variable with a `Monotype`
-}
solve :: Existential Monotype -> Monotype -> Type -> Type
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
solve α τ (Record (Fields kAs ρ)) =
    Record (Fields (map (\(k, _A) -> (k, solve α τ _A)) kAs) ρ)
solve _ _ Bool =
    Bool

{-| Substitute a `Type` by replacing all occurrences of the given unsolved
    row variable with a `Record`
-}
solveRow :: Existential Monotype.Record -> Monotype.Record -> Type -> Type
solveRow _ _ (Variable α) =
    Variable α
solveRow _ _ (Unsolved α) =
    Unsolved α
solveRow ρ₀ r (Forall α _A) =
    Forall α (solveRow ρ₀ r _A)
solveRow ρ₀ r (Function _A _B) =
    Function (solveRow ρ₀ r _A) (solveRow ρ₀ r _B)
solveRow ρ₀ r (List _A) =
    List (solveRow ρ₀ r _A)
solveRow ρ₀ r@(Monotype.Fields kτs ρ₁) (Record (Fields kAs₀ ρ))
    | Just ρ₀ == ρ =
        Record (Fields (map (\(k, _A) -> (k, solveRow ρ₀ r _A)) kAs₁) ρ₁)
    | otherwise =
        Record (Fields (map (\(k, _A) -> (k, solveRow ρ₀ r _A)) kAs₀) ρ)
  where
    kAs₁ = kAs₀ <> map (\(k, τ) -> (k, fromMonotype τ)) kτs
solveRow _ _ Bool =
    Bool

{-| Replace all occurrences of a variable within one `Type` with another `Type`,
    given the variable's label and index
-}
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
substitute α n _A₀ (Record (Fields kAs ρ)) =
    Record (Fields (map (\(k, _A₁) -> (k, substitute α n _A₀ _A₁)) kAs) ρ)
substitute _ _ _ Bool =
    Bool

{-| Count how many times the given `Existential` `Type` variable appears within
    a `Type`
-}
typeFreeIn :: Existential Monotype -> Type -> Bool
_  `typeFreeIn` Variable _            = False
α₀ `typeFreeIn` Unsolved α₁           = α₀ == α₁
α  `typeFreeIn` Forall _ _A           = α `typeFreeIn` _A
α  `typeFreeIn` Function _A _B        = α `typeFreeIn` _A || α `typeFreeIn` _B
α  `typeFreeIn` List _A               = α `typeFreeIn` _A
_  `typeFreeIn` Bool                  = False
α  `typeFreeIn` Record (Fields kAs _) = any (\(_, _A) -> α `typeFreeIn` _A) kAs

{-| Count how many times the given `Existential` `Record` variable appears
    within a `Type`
-}
rowFreeIn :: Existential Monotype.Record -> Type -> Bool
_  `rowFreeIn` Variable _             = False
_  `rowFreeIn` Unsolved _             = False
ρ  `rowFreeIn` Forall _ _A            = ρ `rowFreeIn` _A
ρ  `rowFreeIn` Function _A _B         = ρ `rowFreeIn` _A || ρ `rowFreeIn` _B
ρ  `rowFreeIn` List _A                = ρ `rowFreeIn` _A
_  `rowFreeIn` Bool                   = False
ρ₀ `rowFreeIn` Record (Fields kAs ρ₁) =
    any (ρ₀ ==) ρ₁ || any (\(_, _A) -> ρ₀ `rowFreeIn` _A) kAs

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
prettyPrimitiveType (Variable α) = Pretty.pretty α
prettyPrimitiveType (Unsolved α) = Pretty.pretty α <> "?"
prettyPrimitiveType (Record r)   = prettyRecordType r
prettyPrimitiveType  Bool        = "Bool"
prettyPrimitiveType  other       = "(" <> prettyType other <> ")"

prettyRecordType :: Record -> Doc a
prettyRecordType (Fields [] Nothing) =
    "{ }"
prettyRecordType (Fields [] (Just ρ)) =
    "{ " <> Pretty.pretty ρ <> " }"
prettyRecordType (Fields ((key₀, type₀) : keyTypes) Nothing) =
        "{ "
    <>  Pretty.pretty key₀
    <>  " : "
    <>  prettyType type₀
    <>  foldMap prettyKeyType keyTypes
    <>  " }"
prettyRecordType (Fields ((key₀, type₀) : keyTypes) (Just ρ)) =
        "{ "
    <>  Pretty.pretty key₀
    <>  " : "
    <>  prettyType type₀
    <>  foldMap prettyKeyType keyTypes
    <>  " | "
    <>  Pretty.pretty ρ
    <>  " }"

prettyKeyType :: (Text, Type) -> Doc a
prettyKeyType (key, type_) =
    ", " <> Pretty.pretty key <> " : " <> prettyType type_
