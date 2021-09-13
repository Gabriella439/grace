{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedLists    #-}
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
import Data.Scientific (Scientific)
import Data.Sequence (Seq((:<|)))
import Data.String (IsString(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Grace.Type (Type)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Natural (Natural)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)

import Grace.Pretty
    ( Pretty(..)
    , keyword
    , punctuation
    , label
    , scalar
    , builtin
    , operator
    )

import qualified Data.Text     as Text
import qualified Grace.Pretty  as Pretty
import qualified Grace.Type    as Type
import qualified Prettyprinter as Pretty

{- $setup

   >>> :set -XOverloadedStrings
   >>> :set -XOverloadedLists
   >>> :set -XTypeApplications
   >>> import Data.Void (Void)
-}

-- | The surface syntax for the language
data Syntax s a = Syntax { location :: s, node :: Node s a }
    deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance Bifunctor Syntax where
    first f Syntax{ location, node } =
        Syntax{ location = f location, node = first f node }

    second = fmap

instance IsString (Syntax () a) where
    fromString string = Syntax { location = (), node = fromString string }

instance Pretty a => Pretty (Syntax s a) where
    pretty = liftSyntax prettyExpression

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
            Scalar s -> do
                pure (Scalar s)
            Operator oldLeft s o oldRight -> do
                newLeft <- onSyntax oldLeft
                newRight <- onSyntax oldRight
                return (Operator newLeft s o newRight)
            Builtin b -> do
                pure (Builtin b)
            Embed a -> do
                pure (Embed a)

        return Syntax{ node = newNode, .. }

liftSyntax
    :: Pretty a => (Node s a -> Doc AnsiStyle) -> Syntax s a -> Doc AnsiStyle
liftSyntax pretty_ Syntax{ node } = pretty_ node

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
    | List (Seq (Syntax s a))
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
    deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

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
    first _ (Scalar s) =
        Scalar s
    first f (Operator left location o right) =
        Operator (first f left) (f location) o (first f right)
    first _ (Builtin b) =
        Builtin b
    first _ (Embed a) =
        Embed a

    second = fmap

instance IsString (Node s a) where
    fromString string = Variable (fromString string) 0

instance Pretty a => Pretty (Node s a) where
    pretty = prettyExpression

-- | A scalar value
data Scalar
    = Real Scientific
    -- ^
    --   >>> pretty (Real 1.0)
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
    deriving (Eq, Generic, Lift, Show)

instance Pretty Scalar where
    pretty (Bool True )     = scalar "true"
    pretty (Bool False)     = scalar "false"
    pretty (Real number)    = scalar (pretty number)
    pretty (Integer number) = scalar (pretty number)
    pretty (Natural number) = scalar (pretty number)
    pretty (Text text)      = scalar (Type.prettyTextLiteral text)
    pretty  Null            = scalar "null"

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
    deriving (Eq, Generic, Lift, Show)

instance Pretty Operator where
    pretty And    = operator "&&"
    pretty Or     = operator "||"
    pretty Plus   = operator "+"
    pretty Times  = operator "*"

-- | A built-in function
data Builtin
    = RealEqual
    -- ^
    --   >>> pretty RealEqual
    --   Real/equal
    | RealLessThan
    -- ^
    --   >>> pretty RealLessThan
    --   Real/lessThan
    | RealNegate
    -- ^
    --   >>> pretty RealNegate
    --   Real/negate
    | RealShow
    -- ^
    --   >>> pretty RealShow
    --   Real/show
    | ListDrop
    -- ^
    --   >>> pretty ListDrop
    --   List/drop
    | ListEqual
    -- ^
    --   >>> pretty ListEqual
    --   List/equal
    | ListFold
    -- ^
    --   >>> pretty ListFold
    --   List/fold
    | ListHead
    -- ^
    --   >>> pretty ListHead
    --   List/head
    | ListIndexed
    -- ^
    --   >>> pretty ListIndexed
    --   List/indexed
    | ListLast
    -- ^
    --   >>> pretty ListLast
    --   List/last
    | ListLength
    -- ^
    --   >>> pretty ListLength
    --   List/length
    | ListMap
    -- ^
    --   >>> pretty ListMap
    --   List/map
    | ListReverse
    -- ^
    --   >>> pretty ListReverse
    --   List/reverse
    | ListTake
    -- ^
    --   >>> pretty ListTake
    --   List/take
    | IntegerEven
    -- ^
    --   >>> pretty IntegerEven
    --   Integer/even
    | IntegerNegate
    -- ^
    --   >>> pretty IntegerNegate
    --   Integer/negate
    | IntegerOdd
    -- ^
    --   >>> pretty IntegerOdd
    --   Integer/odd
    | IntegerAbs
    -- ^
    --   >>> pretty IntegerAbs
    --   Integer/abs
    | JSONFold
    -- ^
    --   >>> pretty JSONFold
    --   JSON/fold
    | NaturalFold
    -- ^
    --   >>> pretty NaturalFold
    --   Natural/fold
    | TextEqual
    -- ^
    --   >>> pretty TextEqual
    --   Text/equal
    deriving (Bounded, Enum, Eq, Generic, Lift, Show)

instance Pretty Builtin where
    pretty RealEqual      = builtin "Real/equal"
    pretty RealLessThan   = builtin "Real/lessThan"
    pretty RealNegate     = builtin "Real/negate"
    pretty RealShow       = builtin "Real/show"
    pretty IntegerAbs     = builtin "Integer/abs"
    pretty IntegerEven    = builtin "Integer/even"
    pretty IntegerNegate  = builtin "Integer/negate"
    pretty IntegerOdd     = builtin "Integer/odd"
    pretty JSONFold       = builtin "JSON/fold"
    pretty ListDrop       = builtin "List/drop"
    pretty ListEqual      = builtin "List/equal"
    pretty ListFold       = builtin "List/fold"
    pretty ListHead       = builtin "List/head"
    pretty ListIndexed    = builtin "List/indexed"
    pretty ListLast       = builtin "List/last"
    pretty ListLength     = builtin "List/length"
    pretty ListMap        = builtin "List/map"
    pretty ListReverse    = builtin "List/reverse"
    pretty ListTake       = builtin "List/take"
    pretty NaturalFold    = builtin "Natural/fold"
    pretty TextEqual      = builtin "Text/equal"

-- | Pretty-print an expression
prettyExpression :: Pretty a => Node s a -> Doc AnsiStyle
prettyExpression expression@Lambda{} =
    -- Anywhere you see `Pretty.group (Pretty.flatAlt long short)` that means
    -- that the pretty-printer will first attempt to display `short` if that
    -- fits on one line, otherwise it will fall back to displaying `long`
    -- (which is typically a multi-line result)
    Pretty.group (Pretty.flatAlt long short)
  where
    short = prettyShort expression

    long = Pretty.align (prettyLong expression)

    prettyShort (Lambda _ name body) =
            punctuation "\\"
        <>  label (pretty name)
        <>  " "
        <>  punctuation "-> "
        <>  liftSyntax prettyShort body
    prettyShort body =
        prettyExpression body

    prettyLong (Lambda _ name body) =
            punctuation "\\"
        <>  label (pretty name)
        <>  " "
        <>  punctuation "->"
        <>  Pretty.hardline
        <>  liftSyntax prettyLong body
    prettyLong body =
        "  " <> prettyExpression body

prettyExpression (Let bindings body) = Pretty.group (Pretty.flatAlt long short)
  where
    short =
            foldMap (\binding -> pretty binding <> " ") bindings
        <>  keyword "in"
        <>  " "
        <>  liftSyntax prettyExpression body

    long =
        Pretty.align
            (   foldMap (\binding -> pretty binding <> Pretty.hardline <> Pretty.hardline) bindings
            <>  keyword "in"
            <>  "  "
            <>  liftSyntax prettyExpression body
            )
prettyExpression (If predicate ifTrue ifFalse) =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
            keyword "if"
        <>  " "
        <>  liftSyntax prettyExpression predicate
        <>  " "
        <>  keyword "then"
        <>  " "
        <>  liftSyntax prettyExpression ifTrue
        <>  " "
        <>  keyword "else"
        <>  " "
        <> liftSyntax prettyExpression ifFalse

    long =
        Pretty.align
            (   keyword "if"
            <>  " "
            <>  liftSyntax prettyExpression predicate
            <>  Pretty.hardline
            <>  keyword "then"
            <>  " "
            <>  liftSyntax prettyExpression ifTrue
            <>  Pretty.hardline
            <>  keyword "else"
            <>  " "
            <> liftSyntax prettyExpression ifFalse
            )
prettyExpression (Annotation annotated annotation) =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
            liftSyntax prettyTimesExpression annotated
        <>  " "
        <>  operator ":"
        <>  " "
        <>  pretty annotation

    long =
        Pretty.align
            (   liftSyntax prettyTimesExpression annotated
            <>  Pretty.hardline
            <>  "  "
            <>  operator ":"
            <>  " "
            <>  pretty annotation
            )
prettyExpression other =
    prettyTimesExpression other

prettyOperator
    :: Pretty a
    => Operator
    -> (Node s a -> Doc AnsiStyle)
    -> (Node s a -> Doc AnsiStyle)
prettyOperator operator0 prettyNext expression@(Operator _ _ operator1 _)
    | operator0 == operator1 = Pretty.group (Pretty.flatAlt long short)
  where
    short = prettyShort expression

    long = Pretty.align (prettyLong expression)

    prettyShort (Operator left _ op right)
        | operator0 == op =
                liftSyntax prettyShort left
            <>  " "
            <>  pretty op
            <>  " "
            <>  liftSyntax prettyNext right
    prettyShort other =
        prettyNext other

    prettyLong (Operator left _ op right)
        | operator0 == op =
                liftSyntax prettyLong left
            <>  Pretty.hardline
            <>  pretty op
            <>  pretty (Text.replicate spacing " ")
            <>  liftSyntax prettyNext right
    prettyLong other =
            pretty (Text.replicate indent " ")
        <>  prettyNext other

    operatorWidth = Text.length (Pretty.toText operator0)

    alignment = 2

    align n = ((n `div` alignment) + 1) * alignment

    indent = align operatorWidth

    spacing = indent - operatorWidth
prettyOperator _ prettyNext other =
    prettyNext other

prettyTimesExpression :: Pretty a => Node s a -> Doc AnsiStyle
prettyTimesExpression = prettyOperator Times prettyPlusExpression

prettyPlusExpression :: Pretty a => Node s a -> Doc AnsiStyle
prettyPlusExpression = prettyOperator Plus prettyOrExpression

prettyOrExpression :: Pretty a => Node s a -> Doc AnsiStyle
prettyOrExpression = prettyOperator Or prettyAndExpression

prettyAndExpression :: Pretty a => Node s a -> Doc AnsiStyle
prettyAndExpression = prettyOperator And prettyApplicationExpression

prettyApplicationExpression :: Pretty a => Node s a -> Doc AnsiStyle
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
            liftSyntax prettyShort function
        <>  " "
        <>  liftSyntax prettyFieldExpression argument
    prettyShort (Merge record) =
            keyword "merge" <> " " <> liftSyntax prettyFieldExpression record
    prettyShort other =
        prettyFieldExpression other

    prettyLong (Application function argument) =
            liftSyntax prettyLong function
        <>  Pretty.hardline
        <>  "  "
        <>  liftSyntax prettyFieldExpression argument
    prettyLong (Merge record) =
            keyword "merge"
        <>  Pretty.hardline
        <>  "  "
        <>  liftSyntax prettyFieldExpression record
    prettyLong other =
        prettyFieldExpression other

prettyFieldExpression :: Pretty a => Node s a -> Doc AnsiStyle
prettyFieldExpression expression@Field{}  =
    Pretty.group (Pretty.flatAlt long short)
  where
    short = prettyShort expression

    long = Pretty.align (prettyLong expression)

    prettyShort (Field record _ key) =
            liftSyntax prettyShort record
        <>  operator "."
        <>  Type.prettyRecordLabel False key
    prettyShort other =
        prettyPrimitiveExpression other

    prettyLong (Field record _ key) =
            liftSyntax prettyLong record
        <>  Pretty.hardline
        <>  "  "
        <>  operator "."
        <>  Type.prettyRecordLabel False key
    prettyLong record =
        prettyPrimitiveExpression record
prettyFieldExpression other =
    prettyPrimitiveExpression other

prettyPrimitiveExpression :: Pretty a => Node s a -> Doc AnsiStyle
prettyPrimitiveExpression (Variable name index)
    | index == 0 = label (pretty name)
    | otherwise  = label (pretty name) <> "@" <> scalar (pretty index)
prettyPrimitiveExpression (Alternative name) =
    label (pretty name)
prettyPrimitiveExpression (List []) =
    punctuation "[" <> " " <> punctuation "]"
prettyPrimitiveExpression (List (element :<| elements)) =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
            punctuation "["
        <>  " "
        <>  liftSyntax prettyExpression element
        <>  foldMap (\e -> punctuation "," <> " " <> liftSyntax prettyExpression e) elements
        <>  " "
        <>  punctuation "]"

    long =
        Pretty.align
            (    "[ "
            <>   prettyLongElement element
            <>   foldMap (\e -> punctuation "," <> " " <> prettyLongElement e) elements
            <>   "]"
            )

    prettyLongElement e =
        liftSyntax prettyExpression e <> Pretty.hardline
prettyPrimitiveExpression (Record []) =
    punctuation "{" <> " " <> punctuation "}"
prettyPrimitiveExpression (Record (keyValue : keyValues)) =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
            punctuation "{"
        <>  " "
        <>  prettyShortKeyValue keyValue
        <>  foldMap (\kv -> punctuation "," <> " " <> prettyShortKeyValue kv) keyValues
        <>  " "
        <>  punctuation "}"

    long =
        Pretty.align
            (   punctuation "{"
            <>  " "
            <>  prettyLongKeyValue keyValue
            <>  foldMap (\kv -> punctuation "," <> " " <> prettyLongKeyValue kv) keyValues
            <>  punctuation "}"
            )

    prettyShortKeyValue (key, value) =
            Type.prettyRecordLabel True key
        <>  operator ":"
        <>  " "
        <>  liftSyntax prettyExpression value

    prettyLongKeyValue (key, value) =
            Type.prettyRecordLabel True key
        <>  operator ":"
        <>  Pretty.group (Pretty.flatAlt (Pretty.hardline <> "    ") " ")
        <>  liftSyntax prettyExpression value
        <>  Pretty.hardline
prettyPrimitiveExpression (Builtin b) =
    pretty b
prettyPrimitiveExpression (Scalar s) =
    pretty s
prettyPrimitiveExpression (Embed a) =
    pretty a
prettyPrimitiveExpression other = Pretty.group (Pretty.flatAlt long short)
  where
    short = punctuation "(" <> prettyExpression other <> punctuation ")"

    long =
        Pretty.align
            (   punctuation "("
            <>  " "
            <>  prettyExpression other
            <>  Pretty.hardline
            <>  punctuation ")"
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
    } deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

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
        Pretty.group (Pretty.flatAlt long short)
      where
        long =
            Pretty.align
                (   keyword "let"
                <>  " "
                <>  label (pretty name)
                <>  Pretty.hardline
                <>  "      "
                <>  punctuation "="
                <>  " "
                <>  pretty assignment
                )

        short = keyword "let"
            <>  " "
            <>  label (pretty name)
            <>  " "
            <>  punctuation "="
            <>  " "
            <>  pretty assignment
    pretty Binding{ annotation = Just type_, .. } =
        Pretty.group (Pretty.flatAlt long short)
      where
        long =
            Pretty.align
                (   keyword "let"
                <>  " "
                <>  label (pretty name)
                <>  Pretty.hardline
                <>  "      "
                <>  operator ":"
                <>  " "
                <>  pretty type_
                <>  Pretty.hardline
                <>  "      "
                <>  punctuation "="
                <>  " "
                <>  pretty assignment
                )
        short =
                keyword "let"
            <>  " "
            <>  label (pretty name)
            <>  " "
            <>  operator ":"
            <>  " "
            <>  pretty type_
            <>  " "
            <>  punctuation "="
            <>  " "
            <>  pretty assignment
