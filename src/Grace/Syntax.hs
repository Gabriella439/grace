{-| This module contains the syntax tree used for the surface syntax (i.e. the
    result of parsing), representing the code as the user wrote it.
-}

module Grace.Syntax
    ( -- * Syntax
      Syntax(..)
    , Binding(..)
    ) where

import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..))
import Grace.Type (Type)
import Numeric.Natural (Natural)

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
    | Let (NonEmpty Binding) Syntax
    -- ^
    --   >>> pretty (Let (Binding "x" Nothing "y" :| []) "z")
    --   let x = y in z
    --   >>> pretty (Let (Binding "x" (Just "X") "y" :| []) "z")
    --   let x : X = y in z
    --   >>> pretty (Let (Binding "a" Nothing "b" :| [ Binding "c" Nothing "d" ]) "e")
    --   let a = b let c = d in e
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
    "\\" <> pretty name <> " -> " <> prettyExpression body
prettyExpression (Let bindings body) =
    foldMap pretty bindings <> "in " <> prettyExpression body
prettyExpression (If predicate ifTrue ifFalse) =
        "if "
    <>  prettyExpression predicate
    <>  " then "
    <>  prettyExpression ifTrue
    <>  " else "
    <>  prettyExpression ifFalse
prettyExpression (Annotation annotated annotation) =
        prettyTimesExpression annotated <> " : " <> pretty annotation
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
    prettyFieldExpression record <> "." <> pretty key
prettyFieldExpression other =
    prettyPrimitiveExpression other

prettyPrimitiveExpression :: Syntax -> Doc a
prettyPrimitiveExpression (Variable name index)
    | index == 0 = pretty name
    | otherwise  = pretty name <> "@" <> pretty index
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
    <>  pretty key₀
    <>  " = "
    <>  prettyExpression value₀
    <>  foldMap prettyKeyValue keyValues
    <>  " }"
  where
    prettyKeyValue (key, value) = ", " <> pretty key <> " = " <> pretty value
prettyPrimitiveExpression Grace.Syntax.True =
    "True"
prettyPrimitiveExpression Grace.Syntax.False =
    "False"
prettyPrimitiveExpression (Natural n) =
    pretty n
prettyPrimitiveExpression NaturalFold =
    "Natural/fold"
prettyPrimitiveExpression other =
    "(" <> prettyExpression other <> ")"

{-| The assignment part of a @let@ binding

    >>> pretty (Binding "x" Nothing "y")
    let x = y
    >>> pretty (Binding "x" (Just "X") "y")
    let x : X = y
-}
data Binding = Binding Text (Maybe Type) Syntax
    deriving (Show)

instance Pretty Binding where
    pretty (Binding name Nothing assignment) =
            "let "
        <>  pretty name
        <>  " = "
        <>  prettyExpression assignment
        <>  " "
    pretty (Binding name (Just type_) assignment) =
            "let "
        <>  pretty name
        <>  " : "
        <>  pretty type_
        <>  " = "
        <>  prettyExpression assignment
        <>  " "
