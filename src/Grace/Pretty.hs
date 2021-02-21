{-| Pretty-printing logic

    The organization of this module closely parallels the organization of the
    parsing module.  Doing so ensures that precedence is handled correctly.

    The downside of doing things this way is that if you forget to handle a
    newly added constructor you will get an infinite loop when pretty-printing.
-}
module Grace.Pretty
    ( -- * Pretty-printing
      prettyExpression
    ) where

import Data.Text.Prettyprint.Doc (Doc)
import Grace.Syntax (Syntax(..))

import qualified Data.Text.Prettyprint.Doc as Pretty

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
