{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-| This module contains the syntax tree used for the surface syntax (i.e. the
    result of parsing), representing the code as the user wrote it.
-}

module Grace.Syntax
    ( -- * Syntax
      Syntax(..)
    , Node(..)
    , Scalar(..)
    , Binding(..)
    ) where

import Data.Bifunctor (Bifunctor(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.String (IsString(..))
import Data.Text (Text)
import Prettyprinter (Doc, Pretty(..))
import Grace.Type (Type)
import Numeric.Natural (Natural)

import qualified Grace.Type as Type

{- $setup

   >>> :set -XOverloadedStrings
   >>> :set -XTypeApplications
   >>> import Data.Void (Void)
-}

-- | The surface syntax for the language
data Syntax s a = Syntax { location :: s, node :: Node s a }
    deriving stock (Eq, Foldable, Functor, Show, Traversable)

instance Bifunctor Syntax where
    first f Syntax{ location, node } =
        Syntax{ location = f location, node = first f node }

    second = fmap

instance IsString (Syntax () a) where
    fromString string = Syntax { location = (), node = fromString string }

instance Pretty a => Pretty (Syntax s a) where
    pretty = prettySyntax prettyExpression

prettySyntax :: Pretty a => (Node s a -> Doc b) -> Syntax s a -> Doc b
prettySyntax prettyNode Syntax{ node } = prettyNode node

-- | The constructors for `Syntax`
data Node s a
    = Variable Text Int
    -- ^
    --   >>> pretty @(Node () Void) (Variable "x" 0)
    --   x
    --   >>> pretty @(Node () Void) (Variable "x" 1)
    --   x@1
    | Lambda s Text (Syntax s a)
    -- ^
    --   >>> pretty @(Node () Void) (Lambda () "x" "x")
    --   \x -> x
    | Application (Syntax s a) (Syntax s a)
    -- ^
    --   >>> pretty @(Node () Void) (Application "f" "x")
    --   f x
    | Annotation (Syntax s a) (Type s)
    -- ^
    --   >>> pretty @(Node () Void) (Annotation "x" "A")
    --   x : A
    | Let (NonEmpty (Binding s a)) (Syntax s a)
    -- ^
    --   >>> pretty @(Node () Void) (Let (Binding () "x" Nothing "y" :| []) "z")
    --   let x = y in z
    --   >>> pretty @(Node () Void) (Let (Binding () "x" (Just "X") "y" :| []) "z")
    --   let x : X = y in z
    --   >>> pretty @(Node () Void) (Let (Binding () "a" Nothing "b" :| [ Binding () "c" Nothing "d" ]) "e")
    --   let a = b let c = d in e
    | List [Syntax s a]
    -- ^
    --   >>> pretty @(Node () Void) (List [ "x", "y", "z" ])
    --   [ x, y, z ]
    | Record [(Text, Syntax s a)]
    -- ^
    --   >>> pretty @(Node () Void) (Record [ ("x", "a"), ("y", "b") ])
    --   { x: a, y: b }
    | Field (Syntax s a) s Text
    -- ^
    --   >>> pretty @(Node () Void) (Field "x" () "a")
    --   x.a
    | Alternative Text
    -- ^
    --   >>> pretty @(Node () Void) (Alternative "Nil")
    --   Nil
    | Merge (Syntax s a)
    -- ^
    --   >>> pretty @(Node () Void) (Merge "x")
    --   merge x
    | And (Syntax s a) s (Syntax s a)
    -- ^
    --   >>> pretty @(Node () Void) (And "x" () "y")
    --   x && y
    | Or (Syntax s a) s (Syntax s a)
    -- ^
    --   >>> pretty @(Node () Void) (Or "x" () "y")
    --   x || y
    | If (Syntax s a) (Syntax s a) (Syntax s a)
    -- ^
    --   >>> pretty @(Node () Void) (If "x" "y" "z")
    --   if x then y else z
    | Times (Syntax s a) s (Syntax s a)
    -- ^
    --   >>> pretty @(Node () Void) (Times "x" () "y")
    --   x * y
    | Plus (Syntax s a) s (Syntax s a)
    -- ^
    --   >>> pretty @(Node () Void) (Plus "x" () "y")
    --   x + y
    | NaturalFold
    -- ^
    --   >>> pretty @(Node () Void) NaturalFold
    --   Natural/fold
    | Append (Syntax s a) s (Syntax s a)
    -- ^
    --   >>> pretty @(Node () Void) (Append "x" () "y")
    --   x ++ y
    | Scalar Scalar
    | Embed a
    deriving stock (Eq, Foldable, Functor, Show, Traversable)

instance Bifunctor Node where
    first _ (Variable name index) =
        Variable name index
    first f (Lambda location name body) =
        Lambda (f location) name (first f body)
    first f (Application function argument) =
        Application (first f function) (first f argument)
    first f (Annotation annotated annotation) =
        Annotation (first f annotated) (fmap f annotation)
    first f (Let bindings body) =
        Let (fmap (first f) bindings) (first f body)
    first f (List elements) =
        List (fmap (first f) elements)
    first f (Record keyValues) =
        Record (fmap adapt keyValues)
      where
        adapt (key, value) = (key, first f value)
    first f (Field record location key) =
        Field (first f record) (f location) key
    first _ (Alternative name) =
        Alternative name
    first f (Merge record) =
        Merge (first f record)
    first f (And left location right) =
        And (first f left) (f location) (first f right)
    first f (Or left location right) =
        Or (first f left) (f location) (first f right)
    first f (If predicate ifTrue ifFalse) =
        If (first f predicate) (first f ifTrue) (first f ifFalse)
    first f (Times left location right) =
        Times (first f left) (f location) (first f right)
    first f (Plus left location right) =
        Plus (first f left) (f location) (first f right)
    first _ NaturalFold =
        NaturalFold
    first f (Append left location right) =
        Append (first f left) (f location) (first f right)
    first _ (Scalar scalar) =
        Scalar scalar
    first _ (Embed a) =
        Embed a

    second = fmap

instance IsString (Node s a) where
    fromString string = Variable (fromString string) 0

instance Pretty a => Pretty (Node s a) where
    pretty = prettyExpression

-- | A scalar value
data Scalar
    = Double Double
    -- ^
    --   >>> pretty (Double 1.0)
    --   1.0
    | Integer Integer
    -- ^
    --   >>> pretty (Integer 1)
    --   1
    | Natural Natural
    -- ^
    --   >>> pretty (Natural 1)
    --   1
    | Text Text
    -- ^
    --   >>> pretty (Text "a\n")
    --   "a\n"
    | True
    -- ^
    --   >>> pretty Grace.Syntax.True
    --   true
    | False
    -- ^
    --   >>> pretty Grace.Syntax.False
    --   false
    deriving (Eq, Show)

instance Pretty Scalar where
    pretty Grace.Syntax.True  = "true"
    pretty Grace.Syntax.False = "false"
    pretty (Double number)    = pretty number
    pretty (Integer number)   = pretty number
    pretty (Natural number)   = pretty number
    pretty (Text text)        = Type.prettyTextLiteral text

-- | Pretty-print an expression
prettyExpression :: Pretty a => Node s a -> Doc b
prettyExpression (Lambda _ name body) =
    "\\" <> pretty name <> " -> " <> prettySyntax prettyExpression body
prettyExpression (Let bindings body) =
    foldMap pretty bindings <> "in " <> prettySyntax prettyExpression body
prettyExpression (If predicate ifTrue ifFalse) =
        "if "
    <>  prettySyntax prettyExpression predicate
    <>  " then "
    <>  prettySyntax prettyExpression ifTrue
    <>  " else "
    <>  prettySyntax prettyExpression ifFalse
prettyExpression (Annotation annotated annotation) =
        prettySyntax prettyTimesExpression annotated
    <>  " : "
    <>  pretty annotation
prettyExpression other =
    prettyTimesExpression other

prettyTimesExpression :: Pretty a => Node s a -> Doc b
prettyTimesExpression (Times left _ right) =
        prettySyntax prettyTimesExpression left
    <>  " * "
    <>  prettySyntax prettyPlusExpression right
prettyTimesExpression other =
    prettyPlusExpression other

prettyPlusExpression :: Pretty a => Node s a -> Doc b
prettyPlusExpression (Plus left _ right) =
        prettySyntax prettyPlusExpression left
    <>  " + "
    <>  prettySyntax prettyOrExpression right
prettyPlusExpression other =
    prettyOrExpression other

prettyOrExpression :: Pretty a => Node s a -> Doc b
prettyOrExpression (Or left _ right) =
        prettySyntax prettyOrExpression left
    <>  " || "
    <>  prettySyntax prettyAndExpression right
prettyOrExpression other =
    prettyAndExpression other

prettyAndExpression :: Pretty a => Node s a -> Doc b
prettyAndExpression (And left _ right) =
        prettySyntax prettyAndExpression left
    <>  " && "
    <>  prettySyntax prettyAppendExpression right
prettyAndExpression other =
    prettyAppendExpression other

prettyAppendExpression :: Pretty a => Node s a -> Doc b
prettyAppendExpression (Append left _ right) =
        prettySyntax prettyAppendExpression left
    <>  " ++ "
    <>  prettySyntax prettyApplicationExpression right
prettyAppendExpression other =
    prettyApplicationExpression other

prettyApplicationExpression :: Pretty a => Node s a -> Doc b
prettyApplicationExpression (Application function argument) =
        prettySyntax prettyApplicationExpression function
    <>  " "
    <>  prettySyntax prettyFieldExpression argument
prettyApplicationExpression (Merge record) =
        "merge "
    <>  prettySyntax prettyFieldExpression record
prettyApplicationExpression other =
    prettyFieldExpression other

prettyFieldExpression :: Pretty a => Node s a -> Doc b
prettyFieldExpression (Field record _ key) =
        prettySyntax prettyFieldExpression record
    <>  "."
    <>  Type.prettyRecordLabel key
prettyFieldExpression other =
    prettyPrimitiveExpression other

prettyPrimitiveExpression :: Pretty a => Node s a -> Doc b
prettyPrimitiveExpression (Variable name index)
    | index == 0 = pretty name
    | otherwise  = pretty name <> "@" <> pretty index
prettyPrimitiveExpression (Alternative name) =
    pretty name
prettyPrimitiveExpression (List []) =
    "[ ]"
prettyPrimitiveExpression (List (element₀ : elements)) =
        "[ "
    <>  prettySyntax prettyExpression element₀
    <>  foldMap prettyElement elements
    <>  " ]"
  where
    prettyElement element = ", " <> prettySyntax prettyExpression element
prettyPrimitiveExpression (Record []) =
    "{ }"
prettyPrimitiveExpression (Record ((key₀, value₀) : keyValues)) =
        "{ "
    <>  Type.prettyRecordLabel key₀
    <>  ": "
    <>  prettySyntax prettyExpression value₀
    <>  foldMap prettyKeyValue keyValues
    <>  " }"
  where
    prettyKeyValue (key, value) =
            ", "
        <>  Type.prettyRecordLabel key
        <>  ": "
        <>  prettySyntax prettyExpression value
prettyPrimitiveExpression NaturalFold =
    "Natural/fold"
prettyPrimitiveExpression (Scalar scalar) =
    pretty scalar
prettyPrimitiveExpression (Embed a) =
    pretty a
prettyPrimitiveExpression other =
    "(" <> prettyExpression other <> ")"

{-| The assignment part of a @let@ binding

    >>> pretty @(Binding () Void) (Binding () "x" Nothing "y")
    let x = y
    >>> pretty @(Binding () Void) (Binding () "x" (Just "X") "y")
    let x : X = y
-}
data Binding s a = Binding
    { nameLocation :: s
    , name :: Text
    , annotation :: Maybe (Type s)
    , assignment :: Syntax s a
    } deriving stock (Eq, Foldable, Functor, Show, Traversable)

instance Bifunctor Binding where
    first f Binding{ nameLocation, annotation, assignment, .. } =
        Binding
            { nameLocation = f nameLocation
            , annotation = fmap (fmap f) annotation
            , assignment = first f assignment
            , ..
            }
    second = fmap

instance Pretty a => Pretty (Binding s a) where
    pretty Binding{ annotation = Nothing, .. } =
            "let "
        <>  pretty name
        <>  " = "
        <>  prettySyntax prettyExpression assignment
        <>  " "
    pretty Binding{ annotation = Just type_, .. } =
            "let "
        <>  pretty name
        <>  " : "
        <>  pretty type_
        <>  " = "
        <>  prettySyntax prettyExpression assignment
        <>  " "
