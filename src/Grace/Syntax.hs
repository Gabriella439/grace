{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

{-| This module contains the syntax tree used for the surface syntax (i.e. the
    result of parsing), representing the code as the user wrote it.
-}

module Grace.Syntax
    ( -- * Syntax
      Syntax(..)
    , Chunks(..)
    , types
    , Scalar(..)
    , Operator(..)
    , Builtin(..)
    , FieldName(..)
    , NameBinding(..)
    , Binding(..)
    ) where

import Control.Lens (Plated(..), Traversal')
import Data.Bifunctor (Bifunctor(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Scientific (Scientific)
import Data.Sequence (Seq((:<|)))
import Data.String (IsString(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Grace.Compat ()  -- For an orphan instance for Lift (Seq a)
import Grace.Pretty (Pretty(..), keyword, label, punctuation)
import Grace.Type (Type)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Natural (Natural)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)

import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified Grace.Pretty as Pretty
import qualified Grace.Type as Type
import qualified Prettyprinter as Pretty

{- $setup

   >>> :set -XOverloadedStrings
   >>> :set -XOverloadedLists
   >>> :set -XTypeApplications
   >>> import Data.Void (Void)
-}

-- | The surface syntax for the language
data Syntax s a
    = Variable { location :: s, name :: Text, index :: Int }
    -- ^
    --   >>> pretty @(Syntax () Void) (Variable () "x" 0)
    --   x
    --   >>> pretty @(Syntax () Void) (Variable () "x" 1)
    --   x@1
    | Lambda { location :: s, nameBinding :: NameBinding s, body :: Syntax s a }
    -- ^
    --   >>> pretty @(Syntax () Void) (Lambda () "x" "x")
    --   \x -> x
    --   >>> pretty @(Syntax () Void) (Lambda () (NameBinding () "x" (Just "A")) "x")
    --   \(x : A) -> x
    | Application { location :: s, function :: Syntax s a, argument :: Syntax s a }
    -- ^
    --   >>> pretty @(Syntax () Void) (Application () "f" "x")
    --   f x
    | Annotation { location :: s, annotated :: Syntax s a, annotation :: Type s }
    -- ^
    --   >>> pretty @(Syntax () Void) (Annotation () "x" "A")
    --   x : A
    | Let { location :: s, bindings :: NonEmpty (Binding s a), body :: Syntax s a }
    -- ^
    --   >>> pretty @(Syntax () Void) (Let () (Binding () "x" [] Nothing "y" :| []) "z")
    --   let x = y in z
    --   >>> pretty @(Syntax () Void) (Let () (Binding () "x" [NameBinding () "a" (Just "A")] (Just "X") "y" :| []) "z")
    --   let x (a : A) : X = y in z
    --   >>> pretty @(Syntax () Void) (Let () (Binding () "a" [] Nothing "b" :| [ Binding () "c" [] Nothing "d" ]) "e")
    --   let a = b let c = d in e
    | List { location :: s, elements :: Seq (Syntax s a) }
    -- ^
    --   >>> pretty @(Syntax () Void) (List () [ "x", "y", "z" ])
    --   [ x, y, z ]
    | Record { location :: s, fieldValues :: [(Text, Syntax s a)] }
    -- ^
    --   >>> pretty @(Syntax () Void) (Record () [ ("x", "a"), ("y", "b") ])
    --   { "x": a, "y": b }
    | Field { location :: s, record :: Syntax s a, fieldLocation :: s, field :: Text }
    -- ^
    --   >>> pretty @(Syntax () Void) (Field () "x" () "a")
    --   x.a
    | Alternative { location :: s, name :: Text }
    -- ^
    --   >>> pretty @(Syntax () Void) (Alternative () "Nil")
    --   Nil
    | Merge { location :: s, handlers :: Syntax s a }
    -- ^
    --   >>> pretty @(Syntax () Void) (Merge () "x")
    --   merge x
    | If { location :: s, predicate :: Syntax s a, ifTrue :: Syntax s a, ifFalse :: Syntax s a }
    -- ^
    --   >>> pretty @(Syntax () Void) (If () "x" "y" "z")
    --   if x then y else z
    | Text { location :: s, chunks :: Chunks s a }
    -- ^
    --   >>> pretty @(Syntax () Void) (Text () "a\n")
    --   "a\n"
    --   >>> pretty @(Syntax () Void) (Text () (Chunks "a" [("x", "b")]))
    --   "a${x}b"
    | Prompt{ location :: s, arguments :: Syntax s a, schema :: Maybe (Type s) }
    | Scalar { location :: s, scalar :: Scalar }
    | Operator { location :: s, left :: Syntax s a, operatorLocation :: s, operator :: Operator, right :: Syntax s a }
    -- ^
    --   >>> pretty @(Syntax () Void) (Operator () "x" () And "y")
    --   x && y
    --   >>> pretty @(Syntax () Void) (Operator () "x" () Plus "y")
    --   x + y
    | Builtin { location :: s, builtin :: Builtin }
    | Embed { location :: s, embedded :: a }
    deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance Applicative (Syntax ()) where
    pure embedded = Embed{ location = (), embedded }

    (<*>) = Monad.ap

instance Monad (Syntax ()) where
    Variable{..} >>= _ =
        Variable{..}
    Lambda{ body, .. } >>= f =
        Lambda{ body = body >>= f, .. }
    Application{ function, argument, .. } >>= f =
        Application{ function = function >>= f, argument = argument >>= f, .. }
    Annotation{ annotated, .. } >>= f =
        Annotation{ annotated = annotated >>= f, .. }
    Let{ bindings, body, .. } >>= f =
        Let{ bindings = fmap onBinding bindings, body = body >>= f, .. }
      where
        onBinding Binding{ assignment, .. } =
            Binding{ assignment = assignment >>= f, .. }
    List{ elements, .. } >>= f =
        List{ elements = fmap (>>= f) elements, .. }
    Record{ fieldValues, .. } >>= f =
        Record{ fieldValues = fmap (fmap (>>= f)) fieldValues, .. }
    Field{ record, .. } >>= f =
        Field{ record = record >>= f, .. }
    Alternative{..} >>= _ =
        Alternative{..}
    Merge{ handlers, .. } >>= f =
        Merge{ handlers = handlers >>= f, .. }
    If{ predicate, ifTrue, ifFalse, .. } >>= f =
        If  { predicate = predicate >>= f
            , ifTrue = ifTrue >>= f
            , ifFalse = ifFalse >>= f
            , ..
            }
    Text{ chunks = Chunks text₀ rest, .. } >>= f =
        Text{ chunks = Chunks text₀ (fmap onChunk rest), .. }
      where
        onChunk (interpolation, text) = (interpolation >>= f, text)
    Prompt{ arguments, ..} >>= f =
        Prompt{ arguments = arguments >>= f, ..}
    Scalar{..} >>= _ =
        Scalar{..}
    Operator{ left, right, .. } >>= f =
        Operator{ left = left >>= f, right = right >>= f, .. }
    Builtin{..} >>= _ =
        Builtin{..}
    Embed{ embedded } >>= f =
        f embedded

instance Plated (Syntax s a) where
    plate onSyntax syntax =
        case syntax of
            Variable{..} -> do
                pure Variable{..}
            Lambda{ body = oldBody, ..} -> do
                newBody <- onSyntax oldBody
                return Lambda{ body = newBody, .. }
            Application{ function = oldFunction, argument = oldArgument, .. } -> do
                newFunction <- onSyntax oldFunction
                newArgument <- onSyntax oldArgument
                return Application{ function = newFunction, argument = newArgument, .. }
            Annotation{ annotated = oldAnnotated, .. } -> do
                newAnnotated <- onSyntax oldAnnotated
                return Annotation{ annotated = newAnnotated, .. }
            Let{ bindings = oldBindings, body = oldBody, .. } -> do
                let onBinding Binding{ assignment = oldAssignment, .. } = do
                        newAssignment <- onSyntax oldAssignment
                        return Binding{ assignment = newAssignment, .. }
                newBindings <- traverse onBinding oldBindings
                newBody <- onSyntax oldBody
                return Let{ bindings = newBindings, body = newBody, .. }
            List{ elements = oldElements, .. } -> do
                newElements <- traverse onSyntax oldElements
                return List{ elements = newElements, .. }
            Record{ fieldValues = oldFieldValues, .. } -> do
                let onPair (field, oldValue) = do
                        newValue <- onSyntax oldValue
                        return (field, newValue)

                newFieldValues <- traverse onPair oldFieldValues
                return Record{ fieldValues = newFieldValues, .. }
            Field{ record = oldRecord, .. } -> do
                newRecord <- onSyntax oldRecord
                return Field{ record = newRecord, .. }
            Alternative{..} -> do
                pure Alternative{..}
            Merge{ handlers = oldHandlers, .. } -> do
                newHandlers <- onSyntax oldHandlers
                return Merge{ handlers = newHandlers, .. }
            If{ predicate = oldPredicate, ifTrue = oldIfTrue, ifFalse = oldIfFalse, .. } -> do
                newPredicate <- onSyntax oldPredicate
                newIfTrue <- onSyntax oldIfTrue
                newIfFalse <- onSyntax oldIfFalse
                return If{ predicate = newPredicate, ifTrue = newIfTrue, ifFalse = newIfFalse, .. }
            Text{ chunks = Chunks text₀ rest, .. } -> do
                let onChunk (interpolation, text) = do
                        newInterpolation <- onSyntax interpolation
                        return (newInterpolation, text)
                newRest <- traverse onChunk rest
                return Text{ chunks = Chunks text₀ newRest, .. }
            Prompt{ arguments = oldArguments, .. } -> do
                newArguments <- onSyntax oldArguments
                return Prompt{ arguments = newArguments, .. }
            Scalar{..} -> do
                pure Scalar{..}
            Operator{ left = oldLeft, right = oldRight, .. } -> do
                newLeft <- onSyntax oldLeft
                newRight <- onSyntax oldRight
                return Operator{ left = newLeft, right = newRight, .. }
            Builtin{..} -> do
                pure Builtin{..}
            Embed{..} -> do
                pure Embed{..}

instance Bifunctor Syntax where
    first f Variable{..} =
        Variable{ location = f location, ..}
    first f Lambda{..} =
        Lambda{ location = f location, nameBinding = fmap f nameBinding, body = first f body, .. }
    first f Application{..} =
        Application{ location = f location, function = first f function, argument = first f argument, .. }
    first f Annotation{..} =
        Annotation{ location = f location, annotated = first f annotated, annotation = fmap f annotation, .. }
    first f Let{..} =
        Let{ location = f location, bindings = fmap (first f) bindings, body = first f body, .. }
    first f List{..} =
        List{ location = f location, elements = fmap (first f) elements, .. }
    first f Record{..} =
        Record{ location = f location, fieldValues = fmap adapt fieldValues, .. }
      where
        adapt (field, value) = (field, first f value)
    first f Field{..} =
        Field{ location = f location, record = first f record, fieldLocation = f fieldLocation, .. }
    first f Alternative{..} =
        Alternative{ location = f location, .. }
    first f Merge{..} =
        Merge{ location = f location, handlers = first f handlers, .. }
    first f If{..} =
        If{ location = f location, predicate = first f predicate, ifTrue = first f ifTrue, ifFalse = first f ifFalse, .. }
    first f Text{..} =
        Text{ location = f location, chunks = first f chunks }
    first f Prompt{..} =
        Prompt{ location = f location, arguments = first f arguments, schema = fmap (fmap f) schema }
    first f Scalar{..} =
        Scalar{ location = f location, .. }
    first f Operator{..} =
        Operator{ location = f location, left = first f left, operatorLocation = f operatorLocation, right = first f right, .. }
    first f Builtin{..} =
        Builtin{ location = f location, .. }
    first f Embed{..} =
        Embed{ location = f location, .. }

    second = fmap

instance IsString (Syntax () a) where
    fromString string =
        Variable{ location = (), name = fromString string, index = 0 }

instance Pretty a => Pretty (Syntax s a) where
    pretty = prettyExpression

-- | A text literal with interpolated expressions
data Chunks s a = Chunks Text [(Syntax s a, Text)]
    deriving (Eq, Foldable, Functor, Lift, Show, Traversable)

instance Monoid (Chunks s a) where
    mempty = Chunks mempty mempty

instance Semigroup (Chunks s a) where
    Chunks text₀ rest₀ <> Chunks text₂ rest₂ = case unsnoc rest₀ of
        Nothing -> Chunks (text₀ <> text₂) rest₂
        Just (rest₁, (syntax, text₁)) ->
            Chunks text₀ (rest₁ <> ((syntax, text₁ <> text₂) : rest₂))
      where
        unsnoc [ ] = Nothing
        unsnoc [x] = Just ([], x)
        unsnoc (x : xs) = do
            (i, l) <- unsnoc xs
            return (x : i, l)

instance Bifunctor Chunks where
    first f (Chunks text₀ rest) = Chunks text₀ (fmap (first (first f)) rest)

    second = fmap

instance IsString (Chunks s a) where
    fromString string = Chunks (fromString string) []

instance Pretty a => Pretty (Chunks s a) where
    pretty (Chunks text₀ rest) =
            Pretty.scalar ("\"" <> Type.prettyTextBody text₀)
        <>  foldMap prettyInterpolation rest
        <>  Pretty.scalar "\""
      where
        prettyInterpolation (syntax, text) =
                Pretty.scalar "${"
            <>  pretty syntax
            <>  Pretty.scalar ("}" <> Type.prettyTextBody text)

-- | `Traversal'` from a `Syntax` to its immediate `Type` annotations
types :: Traversal' (Syntax s a) (Type s)
types k Lambda{ nameBinding = NameBinding{ annotation = Just t, .. }, .. } =
    fmap (\t' -> Lambda{ nameBinding = NameBinding{ annotation = Just t', .. }, .. }) (k t)
types k Annotation{ annotation = t, .. } =
    fmap (\t' -> Annotation{ annotation = t', .. }) (k t)
types k Prompt{ schema = Just t, .. } =
    fmap (\t' -> Prompt{ schema = Just t', .. }) (k t)
types k Let{ bindings = bs, .. } =
    fmap (\bs' -> Let{ bindings = bs', .. }) (traverse onBinding bs)
  where
    onBinding Binding{ annotation = Just t, .. } =
        fmap (\t' -> Binding{ annotation = Just t', .. }) (k t)
    onBinding binding = pure binding
types _ e = pure e

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
    pretty (Bool True )     = Pretty.scalar "true"
    pretty (Bool False)     = Pretty.scalar "false"
    pretty (Real number)    = Pretty.scalar (pretty number)
    pretty (Integer number) = Pretty.scalar (pretty number)
    pretty (Natural number) = Pretty.scalar (pretty number)
    pretty  Null            = Pretty.scalar "null"

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
    pretty And    = Pretty.operator "&&"
    pretty Or     = Pretty.operator "||"
    pretty Plus   = Pretty.operator "+"
    pretty Times  = Pretty.operator "*"

-- | A built-in function
data Builtin
    = Some
    -- ^
    --   >>> pretty Some
    --   some
    | RealEqual
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
    pretty Some           = Pretty.builtin "some"
    pretty RealEqual      = Pretty.builtin "Real/equal"
    pretty RealLessThan   = Pretty.builtin "Real/lessThan"
    pretty RealNegate     = Pretty.builtin "Real/negate"
    pretty RealShow       = Pretty.builtin "Real/show"
    pretty IntegerAbs     = Pretty.builtin "Integer/abs"
    pretty IntegerEven    = Pretty.builtin "Integer/even"
    pretty IntegerNegate  = Pretty.builtin "Integer/negate"
    pretty IntegerOdd     = Pretty.builtin "Integer/odd"
    pretty JSONFold       = Pretty.builtin "JSON/fold"
    pretty ListDrop       = Pretty.builtin "List/drop"
    pretty ListEqual      = Pretty.builtin "List/equal"
    pretty ListFold       = Pretty.builtin "List/fold"
    pretty ListHead       = Pretty.builtin "List/head"
    pretty ListIndexed    = Pretty.builtin "List/indexed"
    pretty ListLast       = Pretty.builtin "List/last"
    pretty ListLength     = Pretty.builtin "List/length"
    pretty ListMap        = Pretty.builtin "List/map"
    pretty ListReverse    = Pretty.builtin "List/reverse"
    pretty ListTake       = Pretty.builtin "List/take"
    pretty NaturalFold    = Pretty.builtin "Natural/fold"
    pretty TextEqual      = Pretty.builtin "Text/equal"

-- | Pretty-print an expression
prettyExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyExpression expression@Lambda{} =
    -- Anywhere you see `Pretty.group (Pretty.flatAlt long short)` that means
    -- that the pretty-printer will first attempt to display `short` if that
    -- fits on one line, otherwise it will fall back to displaying `long`
    -- (which is typically a multi-line result)
    Pretty.group (Pretty.flatAlt long short)
  where
    short = punctuation "\\" <> prettyShort expression

    long = Pretty.align (prettyLong expression)

    prettyShort Lambda{..} =
            pretty nameBinding
        <>  " "
        <>  prettyShort body
    prettyShort body =
            punctuation "-> "
        <>  prettyExpression body

    prettyLong Lambda{..} =
            punctuation "\\"
        <>  pretty nameBinding
        <>  " "
        <>  punctuation "->"
        <>  Pretty.hardline
        <>  prettyLong body
    prettyLong body =
        "  " <> prettyExpression body

prettyExpression Let{..} = Pretty.group (Pretty.flatAlt long short)
  where
    short =
            foldMap (\binding -> pretty binding <> " ") bindings
        <>  keyword "in"
        <>  " "
        <>  prettyExpression body

    long =
        Pretty.align
            (   foldMap (\binding -> pretty binding <> Pretty.hardline <> Pretty.hardline) bindings
            <>  keyword "in"
            <>  "  "
            <>  prettyExpression body
            )
prettyExpression If{..} =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
            keyword "if"
        <>  " "
        <>  prettyExpression predicate
        <>  " "
        <>  keyword "then"
        <>  " "
        <>  prettyExpression ifTrue
        <>  " "
        <>  keyword "else"
        <>  " "
        <> prettyExpression ifFalse

    long =
        Pretty.align
            (   keyword "if"
            <>  " "
            <>  prettyExpression predicate
            <>  Pretty.hardline
            <>  keyword "then"
            <>  " "
            <>  prettyExpression ifTrue
            <>  Pretty.hardline
            <>  keyword "else"
            <>  " "
            <> prettyExpression ifFalse
            )
prettyExpression Text{..} = pretty chunks
prettyExpression Prompt{ schema = Just schema, ..} = Pretty.group (Pretty.flatAlt long short)
  where
    short =
            keyword "prompt"
        <>  " "
        <>  prettyFieldExpression arguments
        <>  " "
        <>  Pretty.operator ":"
        <>  " "
        <>  pretty schema

    long =
        Pretty.align
            (   keyword "prompt"
            <>  Pretty.hardline
            <>  "  "
            <>  prettyFieldExpression arguments
            <>  Pretty.hardline
            <>  "  "
            <>  Pretty.operator ":"
            <>  " "
            <>  pretty schema
            )
prettyExpression Annotation{..} =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
            prettyTimesExpression annotated
        <>  " "
        <>  Pretty.operator ":"
        <>  " "
        <>  pretty annotation

    long =
        Pretty.align
            (   prettyTimesExpression annotated
            <>  Pretty.hardline
            <>  "  "
            <>  Pretty.operator ":"
            <>  " "
            <>  pretty annotation
            )
prettyExpression other =
    prettyTimesExpression other

prettyOperator
    :: Pretty a
    => Operator
    -> (Syntax s a -> Doc AnsiStyle)
    -> (Syntax s a -> Doc AnsiStyle)
prettyOperator operator0 prettyNext expression@Operator{ operator = operator1 }
    | operator0 == operator1 = Pretty.group (Pretty.flatAlt long short)
  where
    short = prettyShort expression

    long = Pretty.align (prettyLong expression)

    prettyShort Operator{..}
        | operator0 == operator =
                prettyShort left
            <>  " "
            <>  pretty operator
            <>  " "
            <>  prettyNext right
    prettyShort other =
        prettyNext other

    prettyLong Operator{..}
        | operator0 == operator =
                prettyLong left
            <>  Pretty.hardline
            <>  pretty operator
            <>  pretty (Text.replicate spacing " ")
            <>  prettyNext right
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

prettyTimesExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyTimesExpression = prettyOperator Times prettyPlusExpression

prettyPlusExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyPlusExpression = prettyOperator Plus prettyOrExpression

prettyOrExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyOrExpression = prettyOperator Or prettyAndExpression

prettyAndExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyAndExpression = prettyOperator And prettyApplicationExpression

prettyApplicationExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyApplicationExpression expression
    | isApplication expression = Pretty.group (Pretty.flatAlt long short)
    | otherwise                = prettyFieldExpression expression
  where
    isApplication Application{} = True
    isApplication Merge{}       = True
    isApplication Prompt{}      = True
    isApplication _             = False

    short = prettyShort expression

    long = Pretty.align (prettyLong expression)

    prettyShort Application{..} =
            prettyShort function
        <>  " "
        <>  prettyFieldExpression argument
    prettyShort Merge{..} =
            keyword "merge" <> " " <> prettyFieldExpression handlers
    prettyShort Prompt{ schema = Nothing, .. } =
            keyword "prompt" <> " " <> prettyFieldExpression arguments
    prettyShort other =
        prettyFieldExpression other

    prettyLong Application{..} =
            prettyLong function
        <>  Pretty.hardline
        <>  "  "
        <>  prettyFieldExpression argument
    prettyLong Merge{..} =
            keyword "merge"
        <>  Pretty.hardline
        <>  "  "
        <>  prettyFieldExpression handlers
    prettyLong Prompt{..} =
            keyword "prompt"
        <>  Pretty.hardline
        <>  "  "
        <>  prettyFieldExpression arguments
    prettyLong other =
        prettyFieldExpression other

prettyFieldExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyFieldExpression expression@Field{}  =
    Pretty.group (Pretty.flatAlt long short)
  where
    short = prettyShort expression

    long = Pretty.align (prettyLong expression)

    prettyShort Field{..} =
            prettyShort record
        <>  Pretty.operator "."
        <>  Type.prettyRecordLabel False field
    prettyShort other =
        prettyPrimitiveExpression other

    prettyLong Field{..} =
            prettyLong record
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.operator "."
        <>  Type.prettyRecordLabel False field
    prettyLong record =
        prettyPrimitiveExpression record
prettyFieldExpression other =
    prettyPrimitiveExpression other

prettyPrimitiveExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyPrimitiveExpression Variable{..}
    | index == 0 = label (pretty name)
    | otherwise  = label (pretty name) <> "@" <> Pretty.scalar (pretty index)
prettyPrimitiveExpression Alternative{..} =
    Type.prettyAlternativeLabel name
prettyPrimitiveExpression List{ elements = [] } =
    punctuation "[" <> " " <> punctuation "]"
prettyPrimitiveExpression List{ elements = element :<| elements } =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
            punctuation "["
        <>  " "
        <>  prettyExpression element
        <>  foldMap (\e -> punctuation "," <> " " <> prettyExpression e) elements
        <>  " "
        <>  punctuation "]"

    long =
        Pretty.align
            (    "[ "
            <>   prettyLongElement element
            <>   foldMap (\e -> punctuation "," <> " " <> prettyLongElement e) elements
            <>   "]"
            )

    prettyLongElement e = prettyExpression e <> Pretty.hardline
prettyPrimitiveExpression Record{ fieldValues = [] } =
    punctuation "{" <> " " <> punctuation "}"
prettyPrimitiveExpression Record { fieldValues = fieldValue : fieldValues } =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
            punctuation "{"
        <>  " "
        <>  prettyShortFieldValue fieldValue
        <>  foldMap (\fv -> punctuation "," <> " " <> prettyShortFieldValue fv) fieldValues
        <>  " "
        <>  punctuation "}"

    long =
        Pretty.align
            (   punctuation "{"
            <>  " "
            <>  prettyLongFieldValue fieldValue
            <>  foldMap (\fv -> punctuation "," <> " " <> prettyLongFieldValue fv) fieldValues
            <>  punctuation "}"
            )

    prettyShortFieldValue (field, value) =
            Type.prettyRecordLabel True field
        <>  Pretty.operator ":"
        <>  " "
        <>  prettyExpression value

    prettyLongFieldValue (field, value) =
            Type.prettyRecordLabel True field
        <>  Pretty.operator ":"
        <>  Pretty.group (Pretty.flatAlt (Pretty.hardline <> "    ") " ")
        <>  prettyExpression value
        <>  Pretty.hardline
prettyPrimitiveExpression Builtin{..} =
    pretty builtin
prettyPrimitiveExpression Scalar{..} =
    pretty scalar
prettyPrimitiveExpression Embed{..} =
    pretty embedded
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

{-| A bound field name

    >>> pretty (FieldName () "x" Nothing)
    x
-}
data FieldName s = FieldName
    { fieldNameLocation :: s
    , name :: Text
    , annotation :: Maybe (Type s)
    } deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance IsString (FieldName ()) where
    fromString string = FieldName
        { fieldNameLocation = ()
        , name = fromString string
        , annotation = Nothing
        }

instance Pretty (FieldName s) where
    pretty FieldName{ name, annotation = Nothing } =
        label (pretty name)
    pretty FieldName{ name, annotation = Just type_ } =
        label (pretty name) <> " " <> punctuation ":" <> " " <> pretty type_

{-| A bound variable, possibly with a type annotation

    >>> pretty (NameBinding () "x" Nothing)
    x
    >>> pretty (NameBinding () "x" (Just "X"))
    (x : X)
    >>> pretty (FieldNamesBinding () [])
    { }
    >>> pretty (FieldNamesBinding () [ "x", "y" ])
    { x, y }
-}
data NameBinding s
    = NameBinding
        { nameLocation :: s
        , name :: Text
        , annotation :: Maybe (Type s)
        }
    | FieldNamesBinding
        { fieldNamesLocation :: s
        , fieldNames :: [FieldName s]
        }
    deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance IsString (NameBinding ()) where
    fromString string =
        NameBinding
            { nameLocation = ()
            , name = fromString string
            , annotation = Nothing
            }

instance Pretty (NameBinding s) where
    pretty NameBinding{ annotation = Nothing, .. } =
        label (pretty name)
    pretty NameBinding{ annotation = Just type_, .. } =
            punctuation "("
        <>  label (pretty name)
        <>  " "
        <>  Pretty.operator ":"
        <>  " "
        <>  pretty type_
        <>  punctuation ")"
    pretty FieldNamesBinding{ fieldNames = [ ] } =
            punctuation "{"
        <>  " "
        <>  punctuation "}"
    pretty FieldNamesBinding{ fieldNames = fieldName : fieldNames } =
            punctuation "{"
        <>  " "
        <>  pretty fieldName
        <>  foldMap (\f -> punctuation "," <> " " <> pretty f) fieldNames
        <>  " "
        <>  punctuation "}"

{-| The assignment part of a @let@ binding

    >>> pretty @(Binding () Void) (Binding () "x" [] Nothing "y")
    let x = y
    >>> pretty @(Binding () Void) (Binding () "x" [] (Just "X") "y")
    let x : X = y
    >>> pretty @(Binding () Void) (Binding () "x" [NameBinding () "a" (Just "A")] (Just "X") "y")
    let x (a : A) : X = y
-}
data Binding s a = Binding
    { nameLocation :: s
    , name :: Text
    , nameBindings :: [NameBinding s]
    , annotation :: Maybe (Type s)
    , assignment :: Syntax s a
    } deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance Bifunctor Binding where
    first f Binding{ nameLocation, annotation, assignment, .. } =
        Binding
            { nameLocation = f nameLocation
            , nameBindings = fmap (fmap f) nameBindings
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
                <>  foldMap (\nameBinding -> "      " <> pretty nameBinding <> Pretty.hardline) nameBindings
                <>  "      "
                <>  punctuation "="
                <>  " "
                <>  pretty assignment
                )

        short = keyword "let"
            <>  " "
            <>  label (pretty name)
            <>  " "
            <>  foldMap (\nameBinding -> pretty nameBinding <> " ") nameBindings
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
                <>  foldMap (\nameBinding -> "      " <> pretty nameBinding <> Pretty.hardline) nameBindings
                <>  "      "
                <>  Pretty.operator ":"
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
            <>  foldMap (\nameBinding -> pretty nameBinding <> " ") nameBindings
            <>  Pretty.operator ":"
            <>  " "
            <>  pretty type_
            <>  " "
            <>  punctuation "="
            <>  " "
            <>  pretty assignment
