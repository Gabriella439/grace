{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

{-| This module stores the `Monotype` type representing monomorphic types and
    utilites for operating on `Monotype`s
-}
module Grace.Monotype
    ( -- * Types
      Monotype(..)
    , Record(..)
    , RemainingFields(..)
    , Union(..)
    , RemainingAlternatives(..)
    ) where

import Data.String (IsString(..))
import Data.Text (Text)
import Prettyprinter (Doc, Pretty(..))
import Grace.Existential (Existential)

{- $setup

   >>> :set -XOverloadedStrings
   >>> import qualified Grace.Monotype as Monotype
-}

-- | A monomorphic type
data Monotype
    = VariableType Text
    -- ^ Type variable
    --
    -- >>> pretty (VariableType "a")
    -- a
    | UnsolvedType (Existential Monotype)
    -- ^ A placeholder variable whose type has not yet been inferred
    --
    -- >>> pretty (UnsolvedType 0)
    -- a?
    | Function Monotype Monotype
    -- ^ Function type
    --
    -- >>> pretty (Function "a" "b")
    -- a -> b
    | List Monotype
    -- ^ List type
    --
    -- >>> pretty (List "a")
    -- List a
    | Record Record
    -- ^ Record type
    --
    -- >>> pretty (Record (Fields [("x", "X"), ("y", "Y")] Monotype.EmptyFields))
    -- { x : X, y : Y }
    -- >>> pretty (Record (Fields [("x", "X"), ("y", "Y")] (Monotype.UnsolvedFields 0)))
    -- { x : X, y : Y | a? }
    | Union Union
    -- ^ Union type
    --
    -- >>> pretty (Union (Alternatives [("x", "X"), ("y", "Y")] Monotype.EmptyAlternatives))
    -- < x : X, y : Y >
    -- >>> pretty (Union (Alternatives [("x", "X"), ("y", "Y")] (Monotype.UnsolvedAlternatives 0)))
    -- < x : X, y : Y | a? >
    | Bool
    -- ^ Boolean type
    --
    -- >>> pretty Bool
    -- Bool
    | Natural
    -- ^ Natural number type
    --
    -- >>> pretty Natural
    -- Natural
    | Text
    -- ^ Text type
    --
    -- >>> pretty Text
    -- Text
    deriving stock (Eq, Show)

instance IsString Monotype where
    fromString string = VariableType (fromString string)

instance Pretty Monotype where
    pretty = prettyMonotype

-- | A monomorphic record type
data Record = Fields [(Text, Monotype)] RemainingFields
    deriving stock (Eq, Show)

-- | This represents whether or not the record type is open or closed
data RemainingFields
    = EmptyFields
    -- ^ The record type is closed, meaning that all fields are known
    | UnsolvedFields (Existential Record)
    -- ^ The record type is open, meaning that some fields are known and there
    --   is an unsolved fields variable that is a placeholder for other fields
    --   that may or may not be present
    | VariableFields Text
    -- ^ Same as `UnsolvedFields`, except that the user has given the fields
    --   variable an explicit name in the source code
    deriving stock (Eq, Ord, Show)

-- | A monomorphic union type
data Union = Alternatives [(Text, Monotype)] RemainingAlternatives
    deriving stock (Eq, Show)

-- | This represents whether or not the union type is open or closed
data RemainingAlternatives
    = EmptyAlternatives
    -- ^ The union type is closed, meaning that all alternatives are known
    | UnsolvedAlternatives (Existential Union)
    -- ^ The union type is open, meaning that some alternatives are known and
    --   there is an unsolved alternatives variable that is a placeholder for
    --   other alternatives that may or may not be present
    | VariableAlternatives Text
    -- ^ Same as `UnsolvedAlternatives`, except that the user has given the
    --   alternatives variable an explicit name in the source code
    deriving stock (Eq, Ord, Show)

prettyMonotype :: Monotype -> Doc a
prettyMonotype (Function _A _B) =
    prettyApplicationType _A <> " -> " <> prettyMonotype _B
prettyMonotype other =
    prettyApplicationType other

prettyApplicationType :: Monotype -> Doc a
prettyApplicationType (List _A) = "List " <> prettyPrimitiveType _A
prettyApplicationType  other    =  prettyPrimitiveType other

prettyPrimitiveType :: Monotype -> Doc a
prettyPrimitiveType (VariableType α) =
    pretty α
prettyPrimitiveType (UnsolvedType α) =
    pretty α <> "?"
prettyPrimitiveType (Record r) =
    prettyRecordType r
prettyPrimitiveType (Union u) =
    prettyUnionType u
prettyPrimitiveType Bool =
    "Bool"
prettyPrimitiveType Natural =
    "Natural"
prettyPrimitiveType Text =
    "Text"
prettyPrimitiveType other =
    "(" <> prettyMonotype other <> ")"

prettyRecordType :: Record -> Doc a
prettyRecordType (Fields [] EmptyFields) =
    "{ }"
prettyRecordType (Fields [] (UnsolvedFields ρ)) =
    "{ " <> pretty ρ <> "? }"
prettyRecordType (Fields [] (VariableFields ρ)) =
    "{ " <> pretty ρ <> " }"
prettyRecordType (Fields ((key₀, type₀) : keyTypes) fields) =
        "{ "
    <>  pretty key₀
    <>  " : "
    <>  prettyMonotype type₀
    <>  foldMap prettyKeyType keyTypes
    <>  case fields of
            EmptyFields      -> " }"
            UnsolvedFields ρ -> " | " <> pretty ρ <> "? }"
            VariableFields ρ -> " | " <> pretty ρ <> " }"

prettyUnionType :: Union -> Doc a
prettyUnionType (Alternatives [] EmptyAlternatives) =
    "< >"
prettyUnionType (Alternatives [] (UnsolvedAlternatives ρ)) =
    "< " <> pretty ρ <> "?  >"
prettyUnionType (Alternatives [] (VariableAlternatives ρ)) =
    "< " <> pretty ρ <> "  >"
prettyUnionType (Alternatives ((key₀, type₀) : keyTypes) fields) =
        "< "
    <>  pretty key₀
    <>  " : "
    <>  prettyMonotype type₀
    <>  foldMap prettyKeyType keyTypes
    <>  case fields of
            EmptyAlternatives      -> " >"
            UnsolvedAlternatives ρ -> " | " <> pretty ρ <> "? >"
            VariableAlternatives ρ -> " | " <> pretty ρ <> " >"

prettyKeyType :: (Text, Monotype) -> Doc a
prettyKeyType (key, monotype) =
    ", " <> pretty key <> " : " <> prettyMonotype monotype
