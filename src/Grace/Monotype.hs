{-| This module stores the `Monotype` type representing monomorphic types and
    utilites for operating on `Monotype`s
-}
module Grace.Monotype
    ( -- * Types
      Monotype(..)

    , toVariable
    ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..))

import qualified Data.Char as Char
import qualified Data.Text as Text

-- | A monomorphic type
data Monotype
    = Variable Text
    -- ^ Type variable
    --
    -- >>> pretty (Variable "a")
    -- a
    | Unsolved Int
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
    | Bool
    -- ^ Boolean type
    --
    -- >>> pretty Bool
    -- Bool
    deriving (Eq, Show)

instance Pretty Monotype where
    pretty = prettyMonotype

prettyMonotype :: Monotype -> Doc a
prettyMonotype (Function _A _B) =
    prettyApplicationType _A <> " -> " <> prettyMonotype _B
prettyMonotype other =
    prettyApplicationType other

prettyApplicationType :: Monotype -> Doc a
prettyApplicationType (List _A) = "List " <> prettyPrimitiveType _A
prettyApplicationType  other    =  prettyPrimitiveType other

prettyPrimitiveType :: Monotype -> Doc a
prettyPrimitiveType (Variable α) = pretty α
prettyPrimitiveType (Unsolved α) = pretty (toVariable α) <> "?"
prettyPrimitiveType  Bool        = "Bool"
prettyPrimitiveType  other       = "(" <> prettyMonotype other <> ")"

{-| Convert an unbound variable (internally represented as an `Int`) to a
    user-friendly `Text` representation

>>> toVariable 0
"a"
>>> toVariable 1
"b"
>>> toVariable 26
"a0"
-}
toVariable :: Int -> Text
toVariable n = Text.cons prefix suffix
  where
    (q, r) = n `quotRem` 26

    prefix = Char.chr (Char.ord 'a' + r)

    suffix = if q == 0 then "" else Text.pack (show (q - 1))
