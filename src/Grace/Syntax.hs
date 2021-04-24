{-| This module contains the syntax tree used for the surface syntax (i.e. the
    result of parsing), representing the code as the user wrote it.
-}

module Grace.Syntax
    ( -- * Syntax
      Syntax(..)
    , Binding(..)
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..))
import Grace.Type (Type)
import Numeric.Natural (Natural)

{- $setup
   >>> import Data.Void (Void)
-}

-- | The surface syntax for the language
data Syntax a
    = Variable Text Int
    -- ^
    --   >>> pretty @(Syntax Void) (Variable "x" 0)
    --   x
    --   >>> pretty @(Syntax Void) (Variable "x" 1)
    --   x@1
    | Lambda Text (Syntax a)
    -- ^
    --   >>> pretty @(Syntax Void) (Lambda "x" "x")
    --   \x -> x
    | Application (Syntax a) (Syntax a)
    -- ^
    --   >>> pretty @(Syntax Void) (Application "f" "x")
    --   f x
    | Annotation (Syntax a) Type
    -- ^
    --   >>> pretty @(Syntax Void) (Annotation "x" "A")
    --   x : A
    | Let (NonEmpty (Binding a)) (Syntax a)
    -- ^
    --   >>> pretty @(Syntax Void) (Let (Binding "x" Nothing "y" :| []) "z")
    --   let x = y in z
    --   >>> pretty @(Syntax Void) (Let (Binding "x" (Just "X") "y" :| []) "z")
    --   let x : X = y in z
    --   >>> pretty @(Syntax Void) (Let (Binding "a" Nothing "b" :| [ Binding "c" Nothing "d" ]) "e")
    --   let a = b let c = d in e
    | List [Syntax a]
    -- ^
    --   >>> pretty @(Syntax Void) (List [ "x", "y", "z" ])
    --   [ x, y, z ]
    | Record [(Text, Syntax a)]
    -- ^
    --   >>> pretty @(Syntax Void) (Record [ ("x", "a"), ("y", "b") ])
    --   { x = a, y = b }
    | Field (Syntax a) Text
    -- ^
    --   >>> pretty @(Syntax Void) (Field "x" "a")
    --   x.a
    | Alternative Text
    -- ^
    --   >>> pretty @(Syntax Void) (Alternative "Nil")
    --   Nil
    | Merge (Syntax a)
    -- ^
    --   >>> pretty @(Syntax Void) (Merge "x")
    --   merge x
    | True
    -- ^
    --   >>> pretty @(Syntax Void) Grace.Syntax.True
    --   True
    | False
    -- ^
    --   >>> pretty @(Syntax Void) Grace.Syntax.False
    --   False
    | And (Syntax a) (Syntax a)
    -- ^
    --   >>> pretty @(Syntax Void) (And "x" "y")
    --   x && y
    | Or (Syntax a) (Syntax a)
    -- ^
    --   >>> pretty @(Syntax Void) (Or "x" "y")
    --   x || y
    | If (Syntax a) (Syntax a) (Syntax a)
    -- ^
    --   >>> pretty @(Syntax Void) (If "x" "y" "z")
    --   if x then y else z
    | Natural Natural
    -- ^
    --   >>> pretty @(Syntax Void) (Natural 1)
    --   1
    | Times (Syntax a) (Syntax a)
    -- ^
    --   >>> pretty @(Syntax Void) (Times "x" "y")
    --   x * y
    | Plus (Syntax a) (Syntax a)
    -- ^
    --   >>> pretty @(Syntax Void) (Plus "x" "y")
    --   x + y
    | NaturalFold
    -- ^
    --   >>> pretty @(Syntax Void) NaturalFold
    --   Natural/fold
    | Embed a
    deriving stock (Functor, Show)

instance IsString (Syntax a) where
    fromString string = Variable (fromString string) 0

instance Pretty a => Pretty (Syntax a) where
    pretty = prettyExpression

-- | Pretty-print an expression
prettyExpression :: Pretty a => Syntax a -> Doc b
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

prettyTimesExpression :: Pretty a => Syntax a -> Doc b
prettyTimesExpression (Times left right) =
    prettyTimesExpression left <> " * " <> prettyPlusExpression right
prettyTimesExpression other =
    prettyPlusExpression other

prettyPlusExpression :: Pretty a => Syntax a -> Doc b
prettyPlusExpression (Plus left right) =
    prettyPlusExpression left <> " + " <> prettyOrExpression right
prettyPlusExpression other =
    prettyOrExpression other

prettyOrExpression :: Pretty a => Syntax a -> Doc b
prettyOrExpression (Or left right) =
    prettyOrExpression left <> " || " <> prettyAndExpression right
prettyOrExpression other =
    prettyAndExpression other

prettyAndExpression :: Pretty a => Syntax a -> Doc b
prettyAndExpression (And left right) =
    prettyAndExpression left <> " && " <> prettyApplicationExpression right
prettyAndExpression other =
    prettyApplicationExpression other

prettyApplicationExpression :: Pretty a => Syntax a -> Doc b
prettyApplicationExpression (Application function argument) =
        prettyApplicationExpression function
    <>  " "
    <>  prettyFieldExpression argument
prettyApplicationExpression (Merge record) =
        "merge "
    <>  prettyFieldExpression record
prettyApplicationExpression other =
    prettyFieldExpression other

prettyFieldExpression :: Pretty a => Syntax a -> Doc b
prettyFieldExpression (Field record key) =
    prettyFieldExpression record <> "." <> pretty key
prettyFieldExpression other =
    prettyPrimitiveExpression other

prettyPrimitiveExpression :: Pretty a => Syntax a -> Doc b
prettyPrimitiveExpression (Variable name index)
    | index == 0 = pretty name
    | otherwise  = pretty name <> "@" <> pretty index
prettyPrimitiveExpression (Alternative name) =
    pretty name
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
prettyPrimitiveExpression (Embed a) =
    pretty a
prettyPrimitiveExpression other =
    "(" <> prettyExpression other <> ")"

{-| The assignment part of a @let@ binding

    >>> pretty @(Binding Void) (Binding "x" Nothing "y")
    let x = y
    >>> pretty @(Binding Void) (Binding "x" (Just "X") "y")
    let x : X = y
-}
data Binding a = Binding Text (Maybe Type) (Syntax a)
    deriving stock (Functor, Show)

instance Pretty a => Pretty (Binding a) where
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
