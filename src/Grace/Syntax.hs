{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE DeriveGeneric      #-}
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
    , Operator(..)
    , Builtin(..)
    , Binding(..)
    ) where

import Control.Lens (Plated(..))
import Data.Bifunctor (Bifunctor(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.String (IsString(..))
import Data.Text (Text)
import Prettyprinter (Doc, Pretty(..))
import GHC.Generics (Generic)
import Grace.Type (Type)
import Numeric.Natural (Natural)

import qualified Grace.Type         as Type
import qualified Prettyprinter      as Pretty

{- $setup

   >>> :set -XOverloadedStrings
   >>> :set -XTypeApplications
   >>> import Data.Void (Void)
-}

-- | The surface syntax for the language
data Syntax s a = Syntax { location :: s, node :: Node s a }
    deriving stock (Eq, Foldable, Functor, Generic, Show, Traversable)

instance Bifunctor Syntax where
    first f Syntax{ location, node } =
        Syntax{ location = f location, node = first f node }

    second = fmap

instance IsString (Syntax () a) where
    fromString string = Syntax { location = (), node = fromString string }

instance Pretty a => Pretty (Syntax s a) where
    pretty = prettySyntax prettyExpression

instance Plated (Syntax s a) where
    plate onSyntax Syntax{ node = oldNode, .. } = do
        newNode <- case oldNode of
            Variable name index -> do
                pure (Variable name index)
            Lambda s name oldBody -> do
                newBody <- onSyntax oldBody
                return (Lambda s name newBody)
            Application oldFunction oldArgument -> do
                newFunction <- onSyntax oldFunction
                newArgument <- onSyntax oldArgument
                return (Application newFunction newArgument)
            Annotation oldAnnotated annotation -> do
                newAnnotated <- onSyntax oldAnnotated
                return (Annotation newAnnotated annotation)
            Let oldBindings oldBody -> do
                let onBinding Binding{ assignment = oldAssignment, .. } = do
                        newAssignment <- onSyntax oldAssignment
                        return Binding{ assignment = newAssignment, .. }
                newBindings <- traverse onBinding oldBindings
                newBody <- onSyntax oldBody
                return (Let newBindings newBody)
            List oldElements -> do
                newElements <- traverse onSyntax oldElements
                return (List newElements)
            Record oldFieldValues -> do
                let onPair (field, oldValue) = do
                        newValue <- onSyntax oldValue
                        return (field, newValue)

                newFieldValues <- traverse onPair oldFieldValues
                return (Record newFieldValues)
            Field oldRecord s field -> do
                newRecord <- onSyntax oldRecord
                return (Field newRecord s field)
            Alternative name -> do
                pure (Alternative name)
            Merge oldRecord -> do
                newRecord <- onSyntax oldRecord
                return (Merge newRecord)
            If oldPredicate oldIfTrue oldIfFalse -> do
                newPredicate <- onSyntax oldPredicate
                newIfTrue <- onSyntax oldIfTrue
                newIfFalse <- onSyntax oldIfFalse
                return (If newPredicate newIfTrue newIfFalse)
            Scalar scalar -> do
                pure (Scalar scalar)
            Operator oldLeft s operator oldRight -> do
                newLeft <- onSyntax oldLeft
                newRight <- onSyntax oldRight
                return (Operator newLeft s operator newRight)
            Builtin builtin -> do
                pure (Builtin builtin)
            Embed a -> do
                pure (Embed a)

        return Syntax{ node = newNode, .. }

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
    --   { "x": a, "y": b }
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
    | If (Syntax s a) (Syntax s a) (Syntax s a)
    -- ^
    --   >>> pretty @(Node () Void) (If "x" "y" "z")
    --   if x then y else z
    | Scalar Scalar
    | Operator (Syntax s a) s Operator (Syntax s a)
    -- ^
    --   >>> pretty @(Node () Void) (Operator "x" () And "y")
    --   x && y
    --   >>> pretty @(Node () Void) (Operator "x" () Plus "y")
    --   x + y
    | Builtin Builtin
    | Embed a
    deriving stock (Eq, Foldable, Generic, Functor, Show, Traversable)

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
    first f (If predicate ifTrue ifFalse) =
        If (first f predicate) (first f ifTrue) (first f ifFalse)
    first _ (Scalar scalar) =
        Scalar scalar
    first f (Operator left location operator right) =
        Operator (first f left) (f location) operator (first f right)
    first _ (Builtin builtin) =
        Builtin builtin
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
    | Bool Bool
    -- ^
    --   >>> pretty (Bool True)
    --   true
    --   >>> pretty (Bool False)
    --   false
    | Null
    -- ^
    --   >>> pretty Null
    --   null
    deriving (Eq, Generic, Show)

instance Pretty Scalar where
    pretty (Bool True )     = "true"
    pretty (Bool False)     = "false"
    pretty (Double number)  = pretty number
    pretty (Integer number) = pretty number
    pretty (Natural number) = pretty number
    pretty (Text text)      = Type.prettyTextLiteral text
    pretty  Null            = "null"

-- | A binary infix operator
data Operator
    = And
    -- ^
    --   >>> pretty And
    --   &&
    | Or
    -- ^
    --   >>> pretty Or
    --   ||
    | Plus
    -- ^
    --   >>> pretty Plus
    --   +
    | Times
    -- ^
    --   >>> pretty Times
    --   *
    | Append
    -- ^
    --   >>> pretty Append
    --   ++
    deriving (Eq, Generic, Show)

instance Pretty Operator where
    pretty And    = "&&"
    pretty Or     = "||"
    pretty Plus   = "+"
    pretty Times  = "*"
    pretty Append = "++"

-- | A built-in function
data Builtin
    = DoubleShow
    -- ^
    --   >>> pretty DoubleShow
    --   Double/show
    | ListFold
    -- ^
    --   >>> pretty ListFold
    --   List/fold
    | ListLength
    -- ^
    --   >>> pretty ListLength
    --   List/length
    | ListMap
    -- ^
    --   >>> pretty ListMap
    --   List/map
    | IntegerEven
    -- ^
    --   >>> pretty IntegerEven
    --   Integer/even
    | IntegerOdd
    -- ^
    --   >>> pretty IntegerOdd
    --   Integer/odd
    | NaturalFold
    -- ^
    --   >>> pretty NaturalFold
    --   Natural/fold
    deriving (Eq, Generic, Show)

instance Pretty Builtin where
    pretty DoubleShow  = "Double/show"
    pretty ListFold    = "List/fold"
    pretty ListLength  = "List/length"
    pretty ListMap     = "List/map"
    pretty IntegerEven = "Integer/even"
    pretty IntegerOdd  = "Integer/odd"
    pretty NaturalFold = "Natural/fold"


-- | Pretty-print an expression
prettyExpression :: Pretty a => Node s a -> Doc b
prettyExpression expression@Lambda{} =
    Pretty.group (Pretty.flatAlt long short)
  where
    short = prettyShort expression

    long = Pretty.align (prettyLong expression)

    prettyShort (Lambda _ name body) =
        "\\" <> pretty name <> " -> " <> prettySyntax prettyShort body
    prettyShort body =
        prettyExpression body

    prettyLong (Lambda _ name body) =
            "\\"
        <>  pretty name
        <>  " ->"
        <>  Pretty.hardline
        <>  prettySyntax prettyLong body
    prettyLong body =
        "  " <> prettyExpression body
prettyExpression (Let bindings body) = Pretty.group (Pretty.flatAlt long short)
  where
    short =
            foldMap (\binding -> pretty binding <> " ") bindings
        <>  "in " <> prettySyntax prettyExpression body

    long =
        Pretty.align
            (   foldMap (\binding -> pretty binding <> Pretty.hardline) bindings
            <>  "in  " <> prettySyntax prettyExpression body
            )
prettyExpression (If predicate ifTrue ifFalse) =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
            "if "
        <>  prettySyntax prettyExpression predicate
        <>  " then "
        <>  prettySyntax prettyExpression ifTrue
        <>  " else "
        <> prettySyntax prettyExpression ifFalse

    long =
        Pretty.align
            (   "if "
            <>  prettySyntax prettyExpression predicate
            <>  Pretty.hardline
            <>  "then "
            <>  prettySyntax prettyExpression ifTrue
            <>  Pretty.hardline
            <>  "else "
            <> prettySyntax prettyExpression ifFalse
            )
prettyExpression (Annotation annotated annotation) =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
            prettySyntax prettyTimesExpression annotated
        <>  " : "
        <>  pretty annotation

    long =
        Pretty.align
            (   prettySyntax prettyTimesExpression annotated
            <>  Pretty.hardline
            <>  "  : "
            <>  pretty annotation
            )
prettyExpression other =
    prettyTimesExpression other

prettyOperator
    :: Pretty a
    => Operator
    -> (Node s a -> Doc b)
    -> (Node s a -> Doc b)
prettyOperator operator0 prettyNext (Operator left _ operator1 right)
    | operator0 == operator1 =
            prettySyntax (prettyOperator operator0 prettyNext) left
        <>  " " <> pretty operator1 <> " "
        <> prettySyntax prettyNext right
prettyOperator _ prettyNext other =
    prettyNext other

prettyTimesExpression :: Pretty a => Node s a -> Doc b
prettyTimesExpression = prettyOperator Times prettyPlusExpression

prettyPlusExpression :: Pretty a => Node s a -> Doc b
prettyPlusExpression = prettyOperator Plus prettyOrExpression

prettyOrExpression :: Pretty a => Node s a -> Doc b
prettyOrExpression = prettyOperator Or prettyAndExpression

prettyAndExpression :: Pretty a => Node s a -> Doc b
prettyAndExpression = prettyOperator And prettyAppendExpression

prettyAppendExpression :: Pretty a => Node s a -> Doc b
prettyAppendExpression = prettyOperator Append prettyApplicationExpression

prettyApplicationExpression :: Pretty a => Node s a -> Doc b
prettyApplicationExpression expression
    | isApplication expression = Pretty.group (Pretty.flatAlt long short)
    | otherwise                = prettyFieldExpression expression
  where
    isApplication Application{} = True
    isApplication Merge{}       = True
    isApplication _             = False

    short = prettyShort expression

    long = Pretty.align (prettyLong expression)

    prettyShort (Application function argument) =
            prettySyntax prettyShort function
        <>  " "
        <>  prettySyntax prettyFieldExpression argument
    prettyShort (Merge record) =
            "merge " <>  prettySyntax prettyFieldExpression record
    prettyShort other =
        prettyFieldExpression other

    prettyLong (Application function argument) =
            prettySyntax prettyLong function
        <>  Pretty.hardline
        <>  "  "
        <>  prettySyntax prettyFieldExpression argument
    prettyLong (Merge record) =
            "merge"
        <>  Pretty.hardline
        <>  "  "
        <>  prettySyntax prettyFieldExpression record
    prettyLong other =
        prettyFieldExpression other

prettyFieldExpression :: Pretty a => Node s a -> Doc b
prettyFieldExpression expression@Field{}  =
    Pretty.group (Pretty.flatAlt long short)
  where
    short = prettyShort expression

    long = Pretty.align (prettyLong expression)

    prettyShort (Field record _ key) =
            prettySyntax prettyShort record
        <>  "."
        <>  Type.prettyRecordLabel False key
    prettyShort other =
        prettyPrimitiveExpression other

    prettyLong (Field record _ key) =
            prettySyntax prettyLong record
        <>  Pretty.hardline
        <>  "  ."
        <>  Type.prettyRecordLabel False key
    prettyLong record =
        prettyPrimitiveExpression record
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
prettyPrimitiveExpression (List (element : elements)) =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
            "[ "
        <>  prettySyntax prettyExpression element
        <>  foldMap (\e -> ", " <> prettySyntax prettyExpression e) elements
        <>  " ]"

    long =
        Pretty.align
            (    "[ "
            <>   prettyLongElement element
            <>   foldMap (\e -> ", " <> prettyLongElement e) elements
            <>   "]"
            )

    prettyLongElement e =
        prettySyntax prettyExpression e <> Pretty.hardline
prettyPrimitiveExpression (Record []) =
    "{ }"
prettyPrimitiveExpression (Record ((key₀, value₀) : keyValues)) =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
            "{ "
        <>  prettyShortKeyValue (key₀, value₀)
        <>  foldMap (\kv -> ", " <> prettyShortKeyValue kv) keyValues
        <>  " }"

    long =
        Pretty.align
            (   "{ "
            <>  prettyLongKeyValue (key₀, value₀)
            <>  foldMap (\kv -> ", " <> prettyLongKeyValue kv) keyValues
            <>  "}"
            )

    prettyShortKeyValue (key, value) =
            Type.prettyRecordLabel True key
        <>  ": "
        <>  prettySyntax prettyExpression value

    prettyLongKeyValue (key, value) =
            Type.prettyRecordLabel True key
        <>  ":"
        <>  Pretty.group (Pretty.flatAlt (Pretty.hardline <> "    ") " ")
        <>  prettySyntax prettyExpression value
        <>  Pretty.hardline
prettyPrimitiveExpression (Builtin builtin) =
    pretty builtin
prettyPrimitiveExpression (Scalar scalar) =
    pretty scalar
prettyPrimitiveExpression (Embed a) =
    pretty a
prettyPrimitiveExpression other = Pretty.group (Pretty.flatAlt long short)
  where
    short = "(" <> prettyExpression other <> ")"

    long =
        Pretty.align
            (   "( "
            <>  prettyExpression other
            <>  Pretty.hardline
            <>  ")"
            )

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
    } deriving stock (Eq, Foldable, Functor, Generic, Show, Traversable)

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
    pretty Binding{ annotation = Just type_, .. } =
            "let "
        <>  pretty name
        <>  " : "
        <>  pretty type_
        <>  " = "
        <>  prettySyntax prettyExpression assignment
