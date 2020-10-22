module Grace.Pretty where

import Data.Text.Prettyprint.Doc (Doc)
import Grace.Syntax (Syntax(..))

import qualified Data.Text.Prettyprint.Doc as Pretty

expression :: Syntax -> Doc a
expression (Lambda name inputType body) =
    Pretty.nest 4
        (   "\\("
        <>  Pretty.pretty name
        <>  " : "
        <>  expression inputType
        <>  ") ->"
        <>  Pretty.line
        <>  expression body
        )
expression (Forall "_" inputType outputType) =
    Pretty.nest 4
        (    expression inputType
        <>   " ->"
        <>   Pretty.line
        <>   expression outputType
        )
expression (Forall name inputType outputType) =
    Pretty.nest 4
        (    "forall ("
        <>   Pretty.pretty name
        <>   " : "
        <>   expression inputType
        <>   ") ->"
        <>   Pretty.line
        <>   expression outputType
        )
expression (Let name annotation assignment body) =
    Pretty.align
        (    "let "
        <>   Pretty.pretty name
        <>   " : "
        <>   expression annotation
        <>   " = "
        <>   expression assignment
        <>   Pretty.line
        <>   "in "
        <>   expression body
        )
expression other =
    annotationExpression other

annotationExpression :: Syntax -> Doc a
annotationExpression (Annotation body annotation) =
    orExpression body <> " : " <> annotationExpression annotation
annotationExpression other =
    orExpression other

orExpression :: Syntax -> Doc a
orExpression (Or left right) =
    orExpression left <> " || " <> andExpression right
orExpression other =
    andExpression other

andExpression :: Syntax -> Doc a
andExpression (And left right) =
    andExpression left <> " && " <> applicationExpression right
andExpression other =
    applicationExpression other

applicationExpression :: Syntax -> Doc a
applicationExpression (Application function argument) =
    Pretty.nest 4
        (   applicationExpression function
        <>  Pretty.line
        <>  primitiveExpression argument
        )
applicationExpression other =
    primitiveExpression other

primitiveExpression :: Syntax -> Doc a
primitiveExpression (Variable name index)
    | index == 0 = Pretty.pretty name
    | otherwise  = Pretty.pretty name <> "@" <> Pretty.pretty index
primitiveExpression Bool =
    "Bool"
primitiveExpression Grace.Syntax.True =
    "True"
primitiveExpression Grace.Syntax.False =
    "False"
primitiveExpression Type =
    "Type"
primitiveExpression Kind =
    "Kind"
primitiveExpression other =
    "(" <> expression other <> ")"
