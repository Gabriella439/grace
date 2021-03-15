{-| This module contains the syntax tree used for the surface syntax (i.e. the
    result of parsing), representing the code as the user wrote it.
-}

module Grace.Syntax
    ( -- * Syntax
      Syntax(..)
    ) where

import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..))
import Grace.Type (Type)
import Numeric.Natural (Natural)

import qualified Prettyprinter as Pretty

-- | The surface syntax for the language
data Syntax
    = Variable Text Int
    -- ^
    --   >>> pretty (Variable "x" 0)
    --   x
    --   >>> pretty (Variable "x" 1)
    --   x@1
    | Lambda Text Syntax
    -- ^
    --   >>> pretty (Lambda "x" "x")
    --   \x -> x
    | Application Syntax Syntax
    -- ^
    --   >>> pretty (Application "f" "x")
    --   f x
    | Annotation Syntax Type
    -- ^
    --   >>> pretty (Annotation "x" "A")
    --   x : A
    | Let Text (Maybe Type) Syntax Syntax
    -- ^
    --   >>> pretty (Let "x" Nothing "y" "z")
    --   let x = y in z
    --   >>> pretty (Let "x" (Just "t") "y" "z")
    --   let x : t = y in z
    | List [Syntax]
    -- ^
    --   >>> pretty (List [ "x", "y", "z" ])
    --   [ x, y, z ]
    | Record [(Text, Syntax)]
    -- ^
    --   >>> pretty (Record [ ("x", "a"), ("y", "b") ])
    --   { x = a, y = b }
    | Field Syntax Text
    -- ^
    --   >>> pretty (Field "x" "a")
    --   x.a
    | True
    -- ^
    --   >>> pretty Grace.Syntax.True
    --   True
    | False
    -- ^
    --   >>> pretty Grace.Syntax.False
    --   False
    | And Syntax Syntax
    -- ^
    --   >>> pretty (And "x" "y")
    --   x && y
    | Or Syntax Syntax
    -- ^
    --   >>> pretty (Or "x" "y")
    --   x || y
    | If Syntax Syntax Syntax
    -- ^
    --   >>> pretty (If "x" "y" "z")
    --   if x then y else z
    | Natural Natural
    -- ^
    --   >>> pretty (Natural 1)
    --   1
    | Times Syntax Syntax
    -- ^
    --   >>> pretty (Times "x" "y")
    --   x * y
    | Plus Syntax Syntax
    -- ^
    --   >>> pretty (Plus "x" "y")
    --   x + y
    | NaturalFold
    -- ^
    --   >>> pretty NaturalFold
    --   Natural/fold
    deriving (Show)

instance IsString Syntax where
    fromString string = Variable (fromString string) 0

instance Pretty Syntax where
    pretty = prettyExpression

-- | Pretty-print an expression
prettyExpression :: Syntax -> Doc a
prettyExpression (Lambda name body) =
        "\\"
    <>  Pretty.pretty name
    <>  " -> "
    <>  prettyExpression body
prettyExpression (Let name Nothing assignment body) =
        "let "
    <>  Pretty.pretty name
    <>  " = "
    <>  prettyExpression assignment
    <>  " in "
    <>  prettyExpression body
prettyExpression (Let name (Just type_) assignment body) =
        "let "
    <>  Pretty.pretty name
    <>  " : "
    <>  Pretty.pretty type_
    <>  " = "
    <>  prettyExpression assignment
    <>  " in "
    <>  prettyExpression body
prettyExpression (If predicate ifTrue ifFalse) =
        "if "
    <>  prettyExpression predicate
    <>  " then "
    <>  prettyExpression ifTrue
    <>  " else "
    <>  prettyExpression ifFalse
prettyExpression (Annotation annotated annotation) =
        prettyTimesExpression annotated
    <>  " : "
    <>  Pretty.pretty annotation
prettyExpression other =
    prettyTimesExpression other

prettyTimesExpression :: Syntax -> Doc a
prettyTimesExpression (Times left right) =
    prettyTimesExpression left <> " * " <> prettyPlusExpression right
prettyTimesExpression other =
    prettyPlusExpression other

prettyPlusExpression :: Syntax -> Doc a
prettyPlusExpression (Plus left right) =
    prettyPlusExpression left <> " + " <> prettyOrExpression right
prettyPlusExpression other =
    prettyOrExpression other

prettyOrExpression :: Syntax -> Doc a
prettyOrExpression (Or left right) =
    prettyOrExpression left <> " || " <> prettyAndExpression right
prettyOrExpression other =
    prettyAndExpression other

prettyAndExpression :: Syntax -> Doc a
prettyAndExpression (And left right) =
    prettyAndExpression left <> " && " <> prettyApplicationExpression right
prettyAndExpression other =
    prettyApplicationExpression other

prettyApplicationExpression :: Syntax -> Doc a
prettyApplicationExpression (Application function argument) =
        prettyApplicationExpression function
    <>  " "
    <>  prettyFieldExpression argument
prettyApplicationExpression other =
    prettyFieldExpression other

prettyFieldExpression :: Syntax -> Doc a
prettyFieldExpression (Field record key) =
    prettyFieldExpression record <> "." <> Pretty.pretty key
prettyFieldExpression other =
    prettyPrimitiveExpression other

prettyPrimitiveExpression :: Syntax -> Doc a
prettyPrimitiveExpression (Variable name index)
    | index == 0 = Pretty.pretty name
    | otherwise  = Pretty.pretty name <> "@" <> Pretty.pretty index
prettyPrimitiveExpression (List []) =
    "[ ]"
prettyPrimitiveExpression (List (element₀ : elements)) =
    "[ " <> prettyExpression element₀ <> foldMap prettyElement elements <> " ]"
  where
    prettyElement element = ", " <> prettyExpression element
prettyPrimitiveExpression (Record []) =
    "{ }"
prettyPrimitiveExpression (Record ((key₀, value₀) : keyValues)) =
        "{ "
    <>  Pretty.pretty key₀
    <>  " = "
    <>  prettyExpression value₀
    <>  foldMap prettyKeyValue keyValues
    <>  " }"
  where
    prettyKeyValue (key, value) =
        ", " <> Pretty.pretty key <> " = " <> Pretty.pretty value
prettyPrimitiveExpression Grace.Syntax.True =
    "True"
prettyPrimitiveExpression Grace.Syntax.False =
    "False"
prettyPrimitiveExpression (Natural n) =
    Pretty.pretty n
prettyPrimitiveExpression NaturalFold =
    "Natural/fold"
prettyPrimitiveExpression other =
    "(" <> prettyExpression other <> ")"
