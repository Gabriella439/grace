{-| This module stores the `Monotype` type representing monomorphic types and
    utilites for operating on `Monotype`s
-}
module Grace.Monotype
    ( -- * Types
      Monotype(..)
    , Record(..)

    , existentialTypeToVariable
    , existentialRowToVariable
    ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..))
import Grace.Existential (ExistentialRow(..), ExistentialType(..))

import qualified Data.Char as Char
import qualified Data.Text as Text

-- | A monomorphic type
data Monotype
    = Variable Text
    -- ^ Type variable
    --
    -- >>> pretty (Variable "a")
    -- a
    | Unsolved ExistentialType
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

data Record = Fields [(Text, Monotype)] (Maybe ExistentialRow)
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
    pretty (existentialTypeToVariable α) <> "?"
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
    "{ " <> pretty (existentialRowToVariable ρ) <> " }"
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
    <>  pretty (existentialRowToVariable ρ)
    <>  " }"

prettyKeyType :: (Text, Monotype) -> Doc a
prettyKeyType (key, monotype) =
    ", " <> pretty key <> " : " <> prettyMonotype monotype

{-| Convert an existential type variable to a user-friendly `Text`
    representation

>>> toVariable 0
"a"
>>> toVariable 1
"b"
>>> toVariable 26
"a0"
-}
existentialTypeToVariable :: ExistentialType -> Text
existentialTypeToVariable (ExistentialType n) = Text.cons prefix suffix
  where
    (q, r) = n `quotRem` 26

    prefix = Char.chr (Char.ord 'a' + r)

    suffix = if q == 0 then "" else Text.pack (show (q - 1))

{-| Convert an existential row variable to a user-friendly `Text` representation

    Currently this uses the same logic as `existentialTypeTOVariable`
-}
existentialRowToVariable :: ExistentialRow -> Text
existentialRowToVariable (ExistentialRow n) =
    existentialTypeToVariable (ExistentialType n)
