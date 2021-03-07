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
    --   >>> pretty (Annotation "x" (Type.Variable "A"))
    --   x : A
    | Let Text Syntax Syntax
    -- ^
    --   >>> pretty (Let "x" "y" "z")
    --   let x = y in z
    | List [Syntax]
    -- ^
    --   >>> pretty (List [ "x", "y", "z" ])
    --   [ x, y, z ]
    | If Syntax Syntax Syntax
    -- ^
    --   >>> pretty (If "x" "y" "z")
    --   if x then y else z
    | And Syntax Syntax
    -- ^
    --   >>> pretty (And "x" "y")
    --   x && y
    | Or Syntax Syntax
    -- ^
    --   >>> pretty (Or "x" "y")
    --   x || y
    | True
    -- ^
    --   >>> pretty True
    --   True
    | False
    -- ^
    --   >>> pretty False
    --   False
    deriving (Show)

instance IsString Syntax where
    fromString string = Variable (fromString string) 0

instance Pretty Syntax where
    pretty = prettyExpression

-- | Pretty-print an expression
prettyExpression :: Syntax -> Doc a
prettyExpression (Lambda name body) =
    Pretty.nest 4
        (   "\\"
        <>  Pretty.pretty name
        <>  " -> "
        <>  prettyExpression body
        )
prettyExpression (Let name assignment body) =
    Pretty.align
        (   "let "
        <>  Pretty.pretty name
        <>  " = "
        <>  prettyExpression assignment
        <>  " in "
        <>  prettyExpression body
        )
prettyExpression (If predicate ifTrue ifFalse) =
    Pretty.align
        (   "if "
        <>  prettyExpression predicate
        <>  " then "
        <>  prettyExpression ifTrue
        <>  " else "
        <>  prettyExpression ifFalse
        )
prettyExpression (Annotation annotated annotation) =
        prettyOrExpression annotated
    <>  " : "
    <>  Pretty.pretty annotation
prettyExpression other =
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
    Pretty.nest 4
        (   prettyApplicationExpression function
        <>  " "
        <>  prettyPrimitiveExpression argument
        )
prettyApplicationExpression other =
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
prettyPrimitiveExpression Grace.Syntax.True =
    "True"
prettyPrimitiveExpression Grace.Syntax.False =
    "False"
prettyPrimitiveExpression other =
    "(" <> prettyExpression other <> ")"
