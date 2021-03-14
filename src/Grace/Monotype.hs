{-| This module stores the `Monotype` type representing monomorphic types and
    utilites for operating on `Monotype`s
-}
module Grace.Monotype
    ( -- * Types
      Monotype(..)
    , Record(..)
    ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..))
import Grace.Existential (Existential)

-- | A monomorphic type
data Monotype
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
    | Function Monotype Monotype
    -- ^ Function type
    --
    -- >>> pretty (Function (Variable "a") (Variable "b"))
    -- a -> b
    | List Monotype
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
    deriving (Eq, Show)

instance Pretty Monotype where
    pretty = prettyMonotype

-- | A monomorphic record type
data Record = Fields [(Text, Monotype)] (Maybe (Existential Record))
    deriving (Eq, Show)

prettyMonotype :: Monotype -> Doc a
prettyMonotype (Function _A _B) =
    prettyApplicationType _A <> " -> " <> prettyMonotype _B
prettyMonotype other =
    prettyApplicationType other

prettyApplicationType :: Monotype -> Doc a
prettyApplicationType (List _A) = "List " <> prettyPrimitiveType _A
prettyApplicationType  other    =  prettyPrimitiveType other

prettyPrimitiveType :: Monotype -> Doc a
prettyPrimitiveType (Variable α) =
    pretty α
prettyPrimitiveType (Unsolved α) =
    pretty α <> "?"
prettyPrimitiveType (Record r) =
    prettyRecordType r
prettyPrimitiveType Bool =
    "Bool"
prettyPrimitiveType other =
    "(" <> prettyMonotype other <> ")"

prettyRecordType :: Record -> Doc a
prettyRecordType (Fields [] Nothing) =
    "{ }"
prettyRecordType (Fields [] (Just ρ)) =
    "{ " <> pretty ρ <> " }"
prettyRecordType (Fields ((key₀, type₀) : keyTypes) Nothing) =
        "{ "
    <>  pretty key₀
    <>  " : "
    <>  prettyMonotype type₀
    <>  foldMap prettyKeyType keyTypes
    <>  " }"
prettyRecordType (Fields ((key₀, type₀) : keyTypes) (Just ρ)) =
        "{ "
    <>  pretty key₀
    <>  " : "
    <>  prettyMonotype type₀
    <>  foldMap prettyKeyType keyTypes
    <>  " | "
    <>  pretty ρ
    <>  " }"

prettyKeyType :: (Text, Monotype) -> Doc a
prettyKeyType (key, monotype) =
    ", " <> pretty key <> " : " <> prettyMonotype monotype
