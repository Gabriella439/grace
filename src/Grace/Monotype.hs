module Grace.Monotype where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..))

import qualified Data.Char                 as Char
import qualified Data.Text                 as Text
import qualified Data.Text.Prettyprint.Doc as Pretty

data Monotype
    = Variable Text
    | Unsolved Int
    | Function Monotype Monotype
    | Bool
    deriving (Eq, Show)

instance Pretty Monotype where
    pretty = prettyMonotype

prettyMonotype :: Monotype -> Doc a
prettyMonotype (Function _A _B) =
    prettyPrimitiveType _A <> " -> " <> prettyMonotype _B
prettyMonotype other =
    prettyPrimitiveType other

prettyPrimitiveType :: Monotype -> Doc a
prettyPrimitiveType (Variable α) = Pretty.pretty α
prettyPrimitiveType (Unsolved α) = Pretty.pretty (toVariable α) <> "?"
prettyPrimitiveType  Bool        = "Bool"
prettyPrimitiveType  other       = "(" <> prettyMonotype other <> ")"

toVariable :: Int -> Text
toVariable n = Text.cons prefix suffix
  where
    (q, r) = n `quotRem` 26

    prefix = Char.chr (Char.ord 'a' + r)

    suffix = if q == 0 then "" else Text.pack (show (q - 1))
