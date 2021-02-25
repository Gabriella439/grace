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

import qualified Data.Text.Prettyprint.Doc as Pretty

-- | The surface syntax for the language
data Syntax
    = Variable Text Int
    | Lambda Text Syntax
    | Application Syntax Syntax
    | Annotation Syntax Type
    | Let Text Syntax Syntax
    | If Syntax Syntax Syntax
    | And Syntax Syntax
    | Or Syntax Syntax
    | True
    | False
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
        <>  " ->"
        <>  Pretty.line
        <>  prettyExpression body
        )
prettyExpression (Let name assignment body) =
    Pretty.align
        (   "let "
        <>  Pretty.pretty name
        <>  " = "
        <>  prettyExpression assignment
        <>  Pretty.line
        <>  "in "
        <>  prettyExpression body
        )
prettyExpression (If predicate ifTrue ifFalse) =
    Pretty.align
        (   "if "
        <>  prettyExpression predicate
        <>  Pretty.line
        <>  "then "
        <>  prettyExpression ifTrue
        <>  Pretty.line
        <>  "else "
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
        <>  Pretty.line
        <>  prettyPrimitiveExpression argument
        )
prettyApplicationExpression other =
    prettyPrimitiveExpression other

prettyPrimitiveExpression :: Syntax -> Doc a
prettyPrimitiveExpression (Variable name index)
    | index == 0 = Pretty.pretty name
    | otherwise  = Pretty.pretty name <> "@" <> Pretty.pretty index
prettyPrimitiveExpression Grace.Syntax.True =
    "True"
prettyPrimitiveExpression Grace.Syntax.False =
    "False"
prettyPrimitiveExpression other =
    "(" <> prettyExpression other <> ")"
