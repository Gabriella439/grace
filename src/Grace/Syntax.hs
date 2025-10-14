{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}

{-| This module contains the syntax tree used for the surface syntax (i.e. the
    result of parsing), representing the code as the user wrote it.
-}

module Grace.Syntax
    ( -- * Syntax
      Syntax(..)
    , usedIn
    , effects
    , types
    , complete
    , Chunks(..)
    , Field(..)
    , Smaller(..)
    , Scalar(..)
    , Operator(..)
    , Builtin(..)
    , FieldName(..)
    , NameBinding(..)
    , Binding(..)
    ) where

import Control.Lens (Getting, Plated(..), Traversal')
import Data.Aeson (ToJSON(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Generics.Sum (_As)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid (Any)
import Data.Scientific (Scientific)
import Data.Sequence (Seq((:<|)))
import Data.String (IsString(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Grace.Compat ()  -- For an orphan instance for Lift (Seq a)
import Grace.Context (Context)
import Grace.Pretty (Pretty(..), keyword, punctuation)
import Grace.Type (Type)
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Natural (Natural)
import Prettyprinter.Render.Terminal (AnsiStyle)

import Prettyprinter.Internal
    ( Doc
        ( Annotated
        , Cat
        , Column
        , Fail
        , FlatAlt
        , Line
        , Nest
        , Nesting
        , Union
        , WithPageWidth
        )
    )

import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Grace.Context as Context
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
    = Variable { location :: s, name :: Text }
    -- ^
    --   >>> pretty @(Syntax () Void) (Variable () "x")
    --   x
    | Lambda { location :: s, nameBinding :: NameBinding s a, body :: Syntax s a }
    -- ^
    --   >>> pretty @(Syntax () Void) (Lambda () "x" "x")
    --   \x -> x
    --   >>> pretty @(Syntax () Void) (Lambda () (NameBinding () "x" (Just "A") Nothing) "x")
    --   \(x : A) -> x
    --   >>> pretty @(Syntax () Void) (Lambda () (NameBinding () "x" Nothing (Just "a")) "x")
    --   \(x = a) -> x
    --   >>> pretty @(Syntax () Void) (Lambda () (NameBinding () "x" (Just "A") (Just "a")) "x")
    --   \(x : A = a) -> x
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
    --   >>> pretty @(Syntax () Void) (Let () (Binding () "x" [NameBinding () "a" (Just "A") Nothing, NameBinding () "b" Nothing (Just "e")] (Just "X") "y" :| []) "z")
    --   let x (a : A) (b = e) : X = y in z
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
    | Project { location :: s, larger :: Syntax s a, smaller :: Smaller s }
    -- ^
    --   >>> pretty @(Syntax () Void) (Project () "x" "a")
    --   x.a
    | Alternative { location :: s, name :: Text, argument :: Syntax s a }
    -- ^
    --   >>> pretty @(Syntax () Void) (Alternative () "Foo" "a")
    --   Foo a
    | Fold { location :: s, handlers :: Syntax s a }
    -- ^
    --   >>> pretty @(Syntax () Void) (Fold () "x")
    --   fold x
    | If { location :: s, predicate :: Syntax s a, ifTrue :: Syntax s a, ifFalse :: Syntax s a }
    -- ^
    --   >>> pretty @(Syntax () Void) (If () "x" "y" "z")
    --   if x then y else z
    | Text { location :: s, chunks :: Chunks s a }
    -- ^
    --   >>> pretty @(Syntax () Void) (Text () "a")
    --   "a"
    --   >>> pretty @(Syntax () Void) (Text () (Chunks "a" [("x", "b")]))
    --   "a${x}b"
    | Prompt{ location :: s, import_ :: Bool, arguments :: Syntax s a, schema :: Maybe (Type s) }
    | HTTP{ location :: s, import_ :: Bool, arguments :: Syntax s a, schema :: Maybe (Type s) }
    | Read{ location :: s, import_ :: Bool, arguments :: Syntax s a, schema :: Maybe (Type s) }
    | GitHub{ location :: s, import_ :: Bool, arguments :: Syntax s a, schema :: Maybe (Type s) }
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
    Variable{ location, name } >>= _ =
        Variable{ location, name }

    Lambda{ location, nameBinding, body } >>= f = Lambda
        { location
        , nameBinding = onNameBinding nameBinding
        , body = body >>= f
        }
      where
        onNameBinding NameBinding{ nameLocation, name, annotation, assignment} =
            NameBinding
                { nameLocation
                , name
                , annotation
                , assignment = fmap (>>= f) assignment
                }
        onNameBinding FieldNamesBinding{ fieldNamesLocation, fieldNames } =
            FieldNamesBinding
                { fieldNamesLocation
                , fieldNames = fmap onFieldName fieldNames
                }

        onFieldName
            FieldName{ fieldNameLocation, name, annotation, assignment } =
                FieldName
                    { fieldNameLocation
                    , name
                    , annotation
                    , assignment = fmap (>>= f) assignment
                    }
    Application{ location, function, argument } >>= f = Application
        { location
        , function = function >>= f
        , argument = argument >>= f
        }

    Annotation{ location, annotated, annotation } >>= f =
        Annotation{ location, annotated = annotated >>= f, annotation }

    Let{ location, bindings, body } >>= f =
        Let{ location, bindings = fmap onBinding bindings, body = body >>= f }
      where
        onBinding
            Binding{ nameLocation, name, nameBindings, annotation, assignment } =
                Binding
                    { name
                    , nameLocation
                    , nameBindings = fmap onNameBinding nameBindings
                    , annotation
                    , assignment = assignment >>= f
                    }

        onNameBinding
            NameBinding{ nameLocation, name, annotation, assignment } =
                NameBinding
                    { nameLocation
                    , name
                    , annotation
                    , assignment = fmap (>>= f) assignment
                    }
        onNameBinding FieldNamesBinding{ fieldNamesLocation, fieldNames } =
            FieldNamesBinding
                { fieldNamesLocation
                , fieldNames = fmap onFieldName fieldNames
                }

        onFieldName
            FieldName{ fieldNameLocation, name, annotation, assignment } =
                FieldName
                    { fieldNameLocation
                    , name
                    , annotation
                    , assignment = fmap (>>= f) assignment
                    }

    List{ location, elements } >>= f =
        List{ location, elements = fmap (>>= f) elements }

    Record{ location, fieldValues } >>= f =
        Record{ location, fieldValues = fmap (fmap (>>= f)) fieldValues }

    Project{ location, larger, smaller } >>= f =
        Project{ location, larger = larger >>= f, smaller }

    Alternative{ location, name, argument } >>= f =
        Alternative{ location, name, argument = argument >>= f }

    Fold{ location, handlers } >>= f =
        Fold{ location, handlers = handlers >>= f }

    If{ location, predicate, ifTrue, ifFalse } >>= f = If
        { location
        , predicate = predicate >>= f
        , ifTrue = ifTrue >>= f
        , ifFalse = ifFalse >>= f
        }

    Text{ location, chunks = Chunks text₀ rest } >>= f =
        Text{ location, chunks = Chunks text₀ (fmap onChunk rest) }
      where
        onChunk (interpolation, text) = (interpolation >>= f, text)

    Prompt{ location, import_, arguments, schema } >>= f =
        Prompt{ location, import_, arguments = arguments >>= f, schema }

    HTTP{ location, import_, arguments, schema } >>= f =
        HTTP{ location, import_, arguments = arguments >>= f, schema }

    Read{ location, import_, arguments, schema } >>= f =
        Read{ location, import_, arguments = arguments >>= f, schema }

    GitHub{ location, import_, arguments, schema } >>= f =
        GitHub{ location, import_, arguments = arguments >>= f, schema }

    Scalar{ location, scalar } >>= _ =
        Scalar{ location, scalar }

    Operator{ location, left, operatorLocation, operator, right } >>= f =
        Operator
            { location
            , left = left >>= f
            , operatorLocation
            , operator
            , right = right >>= f
            }

    Builtin{ location, builtin } >>= _ =
        Builtin{ location, builtin }

    Embed{ embedded } >>= f =
        f embedded

instance Plated (Syntax s a) where
    plate onSyntax syntax =
        case syntax of
            Variable{ location, name } -> do
                pure Variable{ location, name }

            Lambda{ location, nameBinding, body } -> do
                newBody <- onSyntax body

                return Lambda{ location, nameBinding, body = newBody }

            Application{ location, function, argument } -> do
                newFunction <- onSyntax function
                newArgument <- onSyntax argument

                return Application
                    { location
                    , function = newFunction
                    , argument = newArgument
                    }

            Annotation{ location, annotated, annotation } -> do
                newAnnotated <- onSyntax annotated

                return Annotation
                    { location
                    , annotated = newAnnotated
                    , annotation
                    }

            Let{ location, bindings, body } -> do
                let onFieldName
                        FieldName{ fieldNameLocation, name, annotation, assignment } = do
                            newAssignment <- traverse onSyntax assignment

                            return FieldName
                                { fieldNameLocation
                                , name
                                , annotation
                                , assignment = newAssignment
                                }

                let onNameBinding
                        NameBinding{ nameLocation, name, annotation, assignment } = do
                            newAssignment <- traverse onSyntax assignment

                            return NameBinding
                                { nameLocation
                                , name
                                , annotation
                                , assignment = newAssignment
                                }

                    onNameBinding
                        FieldNamesBinding{ fieldNamesLocation, fieldNames } = do
                            newFieldNames <- traverse onFieldName fieldNames

                            return FieldNamesBinding
                                { fieldNamesLocation
                                , fieldNames = newFieldNames
                                }

                let onBinding
                        Binding{ nameLocation, name, nameBindings, annotation, assignment } = do
                            newAssignment <- onSyntax assignment

                            newNameBindings <- traverse onNameBinding nameBindings

                            return Binding
                                { nameLocation
                                , name
                                , nameBindings = newNameBindings
                                , annotation
                                , assignment = newAssignment
                                }

                newBindings <- traverse onBinding bindings

                newBody <- onSyntax body

                return Let
                    { location
                    , bindings = newBindings
                    , body = newBody
                    }

            List{ location, elements } -> do
                newElements <- traverse onSyntax elements

                return List{ location, elements = newElements }

            Record{ location, fieldValues } -> do
                let onPair (field, value) = do
                        newValue <- onSyntax value

                        return (field, newValue)

                newFieldValues <- traverse onPair fieldValues

                return Record{ location, fieldValues = newFieldValues }

            Project{ location, larger, smaller } -> do
                newLarger <- onSyntax larger

                return Project{ location, larger = newLarger, smaller }

            Alternative{ location, name, argument } -> do
                newArgument <- onSyntax argument

                pure Alternative{ name, location, argument = newArgument }

            Fold{ location, handlers } -> do
                newHandlers <- onSyntax handlers

                return Fold{ location, handlers = newHandlers }

            If{ location, predicate, ifTrue, ifFalse } -> do
                newPredicate <- onSyntax predicate
                newIfTrue    <- onSyntax ifTrue
                newIfFalse   <- onSyntax ifFalse

                return If
                    { location
                    , predicate = newPredicate
                    , ifTrue = newIfTrue
                    , ifFalse = newIfFalse
                    }

            Text{ location, chunks = Chunks text₀ rest } -> do
                let onChunk (interpolation, text) = do
                        newInterpolation <- onSyntax interpolation

                        return (newInterpolation, text)

                newRest <- traverse onChunk rest

                return Text{ location, chunks = Chunks text₀ newRest }

            Prompt{ location, import_, arguments, schema } -> do
                newArguments <- onSyntax arguments

                return Prompt{ location, import_, arguments = newArguments, schema }

            HTTP{ location, import_, arguments, schema } -> do
                newArguments <- onSyntax arguments

                return HTTP{ location, import_, arguments = newArguments, schema }

            Read{ location, import_, arguments, schema } -> do
                newArguments <- onSyntax arguments

                return Read{ location, import_, arguments = newArguments, schema }

            GitHub{ location, import_, arguments, schema } -> do
                newArguments <- onSyntax arguments

                return GitHub{ location, import_, arguments = newArguments, schema }

            Scalar{ location, scalar } -> do
                pure Scalar{ location, scalar }

            Operator{ location, left, operatorLocation, operator, right } -> do
                newLeft  <- onSyntax left
                newRight <- onSyntax right

                return Operator
                    { location
                    , left = newLeft
                    , operatorLocation
                    , operator
                    , right = newRight
                    }

            Builtin{ location, builtin } -> do
                pure Builtin{ location, builtin }

            Embed{ location, embedded } -> do
                pure Embed{ location, embedded }

instance Bifunctor Syntax where
    first f Variable{ location, name } =
        Variable{ name, location = f location }

    first f Lambda{ location, nameBinding, body } = Lambda
        { location = f location
        , nameBinding = first f nameBinding
        , body = first f body
        }

    first f Application{ location, function, argument } = Application
        { location = f location
        , function = first f function
        , argument = first f argument
        }

    first f Annotation{ location, annotated, annotation } = Annotation
        { location = f location
        , annotated = first f annotated
        , annotation = fmap f annotation
        }

    first f Let{ location, bindings, body } = Let
        { location = f location
        , bindings = fmap (first f) bindings
        , body = first f body
        }

    first f List{ location, elements } =
        List{ location = f location, elements = fmap (first f) elements }

    first f Record{ location, fieldValues } =
        Record{ location = f location, fieldValues = fmap adapt fieldValues }
      where
        adapt (field, value) = (field, first f value)

    first f Project{ location, larger, smaller } = Project
        { location = f location
        , larger = first f larger
        , smaller = fmap f smaller
        }

    first f Alternative{ location, name, argument } =
        Alternative{ location = f location, name, argument = first f argument }

    first f Fold{ location, handlers } =
        Fold{ location = f location, handlers = first f handlers }

    first f If{ location, predicate, ifTrue, ifFalse } = If
        { location = f location
        , predicate = first f predicate
        , ifTrue = first f ifTrue
        , ifFalse = first f ifFalse
        }

    first f Text{ location, chunks } =
        Text{ location = f location, chunks = first f chunks }

    first f Prompt{ location, import_, arguments, schema } = Prompt
        { location = f location
        , import_
        , arguments = first f arguments
        , schema = fmap (fmap f) schema
        }

    first f HTTP{ location, import_, arguments, schema } = HTTP
        { location = f location
        , import_
        , arguments = first f arguments
        , schema = fmap (fmap f) schema
        }

    first f Read{ location, import_, arguments, schema } = Read
        { location = f location
        , import_
        , arguments = first f arguments
        , schema = fmap (fmap f) schema
        }

    first f GitHub{ location, import_, arguments, schema } = GitHub
        { location = f location
        , import_
        , arguments = first f arguments
        , schema = fmap (fmap f) schema
        }

    first f Scalar{ location, scalar } =
        Scalar{ location = f location, scalar }

    first f Operator{ location, left, operatorLocation, operator, right } =
        Operator
            { location = f location
            , left = first f left
            , operatorLocation = f operatorLocation
            , operator
            , right = first f right
            }

    first f Builtin{ location, builtin } =
        Builtin{ location = f location, builtin }

    first f Embed{ location, embedded } =
        Embed{ location = f location, embedded }

    second = fmap

instance IsString (Syntax () a) where
    fromString string =
        Variable{ location = (), name = fromString string }

instance Pretty a => Pretty (Syntax s a) where
    pretty = prettyExpression

-- | Returns whether the given variable is used within the expression
usedIn :: Text -> Syntax s a -> Bool
usedIn name₀ Variable{ name = name₁ } =
     name₀ == name₁
usedIn name₀ Lambda{ nameBinding = NameBinding{ name = name₁ }, body } =
    (name₀ /= name₁) && usedIn name₀ body
usedIn name₀ Lambda{ nameBinding = FieldNamesBinding{ fieldNames }, body } =
    (name₀ `notElem` fmap toName fieldNames) && usedIn name₀ body
  where
    toName FieldName{ name = name₁ } = name₁
usedIn name₀ Application{ function, argument } =
    usedIn name₀ function || usedIn name₀ argument
usedIn name₀ Annotation{ annotated } =
    usedIn name₀ annotated
usedIn name₀ Let{ bindings = Binding{ name = name₁, assignment } :| [], body } =
    usedIn name₀ assignment || (name₀ /= name₁ && usedIn name₀ body)
usedIn name₀ Let{ location, bindings = Binding{ name = name₁, assignment } :| (b : bs), body } =
    usedIn name₀ assignment || (name₀ /= name₁ && usedIn name₀ Let{ location, bindings = b :| bs, body })
usedIn name₀ List{ elements } =
    any (usedIn name₀) elements
usedIn name₀ Record{ fieldValues } =
    any (usedIn name₀ . snd) fieldValues
usedIn name₀ Project{ larger } =
    usedIn name₀ larger
usedIn name₀ Alternative{ argument } =
    usedIn name₀ argument
usedIn name₀ Fold{ handlers } =
    usedIn name₀ handlers
usedIn name₀ If{ predicate, ifTrue, ifFalse } =
    usedIn name₀ predicate && usedIn name₀ ifTrue && usedIn name₀ ifFalse
usedIn name₀ Text{ chunks = Chunks _ pairs } =
    any (usedIn name₀ . fst) pairs
usedIn _ Scalar{ } =
    False
usedIn name₀ Prompt{ arguments } =
    usedIn name₀ arguments
usedIn name₀ HTTP{ arguments } =
    usedIn name₀ arguments
usedIn name₀ Read{ arguments } =
    usedIn name₀ arguments
usedIn name₀ GitHub{ arguments } =
    usedIn name₀ arguments
usedIn name₀ Operator{ left, right } =
    usedIn name₀ left && usedIn name₀ right
usedIn _ Builtin{ } =
    False
usedIn _ Embed{ } =
    False

-- | `Getting` that matches all effects within a `Syntax` tree
effects :: Getting Any (Syntax s a) ()
effects = Lens.cosmos . effect
  where
    effect =
            (_As @"Prompt" . Lens.to (\_ -> ()))
        <>  (_As @"HTTP"   . Lens.to (\_ -> ()))
        <>  (_As @"GitHub" . Lens.to (\_ -> ()))

-- | A text literal with interpolated expressions
data Chunks s a = Chunks Text [(Syntax s a, Text)]
    deriving stock (Eq, Foldable, Functor, Lift, Show, Traversable)

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
    pretty (Chunks text₀ rest) = Pretty.flatAlt long short
      where
        short =
            (   Pretty.punctuation "\""
            <>  Pretty.scalar (Type.prettyTextBody False text₀)
            <>  foldMap (prettyInterpolation False) rest
            <>  Pretty.punctuation "\""
            )

        long =
            (if multiline then Pretty.align else id)
                (   Pretty.punctuation prefix
                <>  Pretty.scalar (Type.prettyTextBody multiline text₀)
                <>  foldMap (prettyInterpolation multiline) rest
                <>  Pretty.punctuation "\""
                )

        prefix
            | multiline = "\"\n"
            | otherwise = "\""

        multiline =
            Text.any (== '\n') text₀ || any (Text.any (== '\n') . snd) rest

        prettyInterpolation m (syntax, text) =
                Pretty.punctuation "${"
            <>  flatten (pretty syntax)
            <>  Pretty.punctuation "}"
            <>  Pretty.scalar (Type.prettyTextBody m text)

-- | A field of a record
data Field s = Field{ fieldLocation :: s, field :: Text }
    deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance IsString (Field ()) where
    fromString string = Field{ fieldLocation = (), field = fromString string }

-- | A projection of one or more fields
data Smaller s
    = Single{ single :: Field s }
    | Multiple{ multipleLocation :: s, multiple :: [Field s] }
    | Index{ index :: Integer }
    | Slice{ begin :: Maybe Integer, end :: Maybe Integer }
    deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance IsString (Smaller ()) where
    fromString string = Single{ single = fromString string }

-- | @Traversal'@ from a `Syntax` to its immediate `Type`
types :: Traversal' (Syntax s a) (Type s)
types onType
    Lambda{ location, nameBinding = NameBinding{ nameLocation, name, annotation, assignment }, body } = do
        newAnnotation <- traverse onType annotation

        return Lambda
            { location
            , nameBinding = NameBinding
                { nameLocation
                , name
                , annotation = newAnnotation
                , assignment
                }
            , body
            }
types onType Annotation{ location, annotated, annotation } = do
    newAnnotation <- onType annotation

    return Annotation{ location, annotated, annotation = newAnnotation }
types onType Prompt{ location, import_, arguments, schema } = do
    newSchema <- traverse onType schema

    return Prompt{ location, import_, arguments, schema = newSchema }
types onType HTTP{ location, import_, arguments, schema } = do
    newSchema <- traverse onType schema

    return HTTP{ location, import_, arguments, schema = newSchema }
types onType Read{ location, import_, arguments, schema } = do
    newSchema <- traverse onType schema

    return Read{ location, import_, arguments, schema = newSchema }
types onType GitHub{ location, import_, arguments, schema } = do
    newSchema <- traverse onType schema

    return GitHub{ location, import_, arguments, schema = newSchema }
types onType Let{ location, bindings, body } = do
    newBindings <- traverse onBinding bindings

    return Let{ location, bindings = newBindings, body }
  where
    onBinding
        Binding{ nameLocation, name, nameBindings, annotation, assignment } = do
            newNameBindings <- traverse onNameBinding nameBindings

            newAnnotation <- traverse onType annotation

            return Binding
                { nameLocation
                , name
                , nameBindings = newNameBindings
                , annotation = newAnnotation
                , assignment
                }

    onNameBinding NameBinding{ nameLocation, name, annotation, assignment } = do
        newAnnotation <- traverse onType annotation

        return NameBinding
            { nameLocation
            , name
            , annotation = newAnnotation
            , assignment
            }
    onNameBinding FieldNamesBinding{ fieldNamesLocation, fieldNames } = do
        newFieldNames <- traverse onFieldName fieldNames

        return FieldNamesBinding
            { fieldNamesLocation
            , fieldNames = newFieldNames
            }

    onFieldName
        FieldName{ fieldNameLocation, name, annotation, assignment } = do
            newAnnotation <- traverse onType annotation

            return FieldName
                { fieldNameLocation
                , name
                , annotation = newAnnotation
                , assignment
                }
types _ e = pure e

-- | Complete all `Type` annotations in a `Syntax` tree using the provided
-- `Context`
complete :: Context s -> Syntax s a -> Syntax s a
complete context = Lens.transform (Lens.over types (Context.complete context))

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
    | Key Text
    deriving stock (Eq, Generic, Lift, Show)

instance ToJSON Scalar where
    toJSON (Real n) = toJSON n
    toJSON (Integer n) = toJSON n
    toJSON (Natural n) = toJSON n
    toJSON (Bool b) = toJSON b
    toJSON Null = Aeson.Null
    toJSON (Key _) = Aeson.Null

instance Pretty Scalar where
    pretty (Bool True )     = Pretty.scalar "true"
    pretty (Bool False)     = Pretty.scalar "false"
    pretty (Real number)    = Pretty.scalar (pretty number)
    pretty (Integer number) = Pretty.scalar (pretty number)
    pretty (Natural number) = Pretty.scalar (pretty number)
    pretty  Null            = Pretty.scalar "null"
    pretty (Key _)          = Pretty.scalar "🔒"

-- | A binary infix operator
data Operator
    = Or
    -- ^
    --   >>> pretty Or
    --   ||
    | And
    -- ^
    --   >>> pretty And
    --   &&
    | Equal
    -- ^
    --   >>> pretty Equal
    --   ==
    | NotEqual
    -- ^
    --   >>> pretty NotEqual
    --   !=
    | LessThan
    -- ^
    --   >>> pretty LessThan
    --   <
    | LessThanOrEqual
    -- ^
    --   >>> pretty LessThanOrEqual
    --   <=
    | GreaterThan
    -- ^
    --   >>> pretty GreaterThan
    --   >
    | GreaterThanOrEqual
    -- ^
    --   >>> pretty GreaterThanOrEqual
    --   >=
    | Plus
    -- ^
    --   >>> pretty Plus
    --   +
    | Minus
    -- ^
    --   >>> pretty Minus
    --   -
    | Times
    -- ^
    --   >>> pretty Times
    --   *
    | Modulus
    -- ^
    --   >>> pretty Modulus
    --   %
    | Divide
    -- ^
    --   >>> pretty Divide
    --   /
    deriving stock (Eq, Generic, Lift, Show)

instance Pretty Operator where
    pretty And                = Pretty.operator "&&"
    pretty Or                 = Pretty.operator "||"
    pretty Equal              = Pretty.operator "=="
    pretty NotEqual           = Pretty.operator "!="
    pretty LessThan           = Pretty.operator "<"
    pretty LessThanOrEqual    = Pretty.operator "<="
    pretty GreaterThan        = Pretty.operator ">"
    pretty GreaterThanOrEqual = Pretty.operator ">="
    pretty Plus               = Pretty.operator "+"
    pretty Minus              = Pretty.operator "-"
    pretty Times              = Pretty.operator "*"
    pretty Modulus            = Pretty.operator "%"
    pretty Divide             = Pretty.operator "/"

-- | A built-in function
data Builtin
    = Abs
    -- ^
    --   >>> pretty Abs
    --   abs
    | Indexed
    -- ^
    --   >>> pretty Indexed
    --   indexed
    | Length
    -- ^
    --   >>> pretty Length
    --   length
    | Map
    -- ^
    --   >>> pretty Map
    --   map
    | Reveal
    -- ^
    --   >>> pretty Reveal
    --   reveal
    | Show
    -- ^
    --   >>> pretty Show
    --   show
    | Some
    -- ^
    --   >>> pretty Some
    --   some
    | YAML
    -- ^
    --   >>> pretty YAML
    --   yaml
    deriving stock (Bounded, Enum, Eq, Generic, Lift, Show)

instance Pretty Builtin where
    pretty Abs     = Pretty.builtin "abs"
    pretty Indexed = Pretty.builtin "indexed"
    pretty Length  = Pretty.builtin "length"
    pretty Map     = Pretty.builtin "map"
    pretty Reveal  = Pretty.builtin "reveal"
    pretty Show    = Pretty.builtin "show"
    pretty Some    = Pretty.builtin "some"
    pretty YAML    = Pretty.builtin "yaml"

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

    long = prettyLong expression

    prettyShort Lambda{ nameBinding, body } =
            pretty nameBinding
        <>  " "
        <>  prettyShort body
    prettyShort body =
            punctuation "->"
        <>  " "
        <>  prettyExpression body

    prettyLong Lambda{ nameBinding, body } =
            punctuation "\\"
        <>  pretty nameBinding
        <>  " "
        <>  punctuation "->"
        <>  Pretty.hardline
        <>  prettyLong body
    prettyLong body =
        "  " <> Pretty.nest 2 (prettyExpression body)

prettyExpression Let{ bindings, body }
    = Pretty.group (Pretty.flatAlt long short)
  where
    short = foldMap (\binding -> pretty binding <> " ") bindings
        <>  keyword "in"
        <>  " "
        <>  prettyExpression body

    long =  foldMap (\binding -> pretty binding <> Pretty.hardline <> Pretty.hardline) bindings
        <>  keyword "in"
        <>  "  "
        <>  Pretty.nest 4 (prettyExpression body)
prettyExpression If{ predicate, ifTrue, ifFalse } =
    Pretty.group (Pretty.flatAlt long short)
  where
    short = keyword "if"
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

    long =  keyword "if"
        <>  "  "
        <>  Pretty.nest 4 (prettyExpression predicate)
        <>  Pretty.hardline
        <>  keyword "then"
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.nest 2 (prettyExpression ifTrue)
        <>  Pretty.hardline
        <>  keyword "else"
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.nest 2 (prettyExpression ifFalse)
prettyExpression Prompt{ arguments, import_, schema = Just schema } =
    Pretty.group (Pretty.flatAlt long short)
  where
    short = prefix
        <>  keyword "prompt"
        <>  " "
        <>  prettyProjectExpression arguments
        <>  " "
        <>  Pretty.operator ":"
        <>  " "
        <>  pretty schema

    long =  prefix
        <>  keyword "prompt"
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.nest 2 (prettyProjectExpression arguments)
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.operator ":"
        <>  " "
        <>  Pretty.nest 4 (pretty schema)

    prefix = if import_ then keyword "import" <> " " else mempty
prettyExpression HTTP{ arguments, import_, schema = Just schema } =
    Pretty.group (Pretty.flatAlt long short)
  where
    short = prefix
        <>  keyword "http"
        <>  " "
        <>  prettyProjectExpression arguments
        <>  " "
        <>  Pretty.operator ":"
        <>  " "
        <>  pretty schema

    long =  prefix
        <>  keyword "http"
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.nest 2 (prettyProjectExpression arguments)
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.operator ":"
        <>  " "
        <>  Pretty.nest 4 (pretty schema)

    prefix = if import_ then keyword "import" <> " " else mempty
prettyExpression Read{ arguments, import_, schema = Just schema } =
    Pretty.group (Pretty.flatAlt long short)
  where
    short = prefix
        <>  keyword "read"
        <>  " "
        <>  prettyProjectExpression arguments
        <>  " "
        <>  Pretty.operator ":"
        <>  " "
        <>  pretty schema

    long =  keyword "read"
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.nest 2 (prettyProjectExpression arguments)
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.operator ":"
        <>  " "
        <>  Pretty.nest 4 (pretty schema)

    prefix = if import_ then keyword "import" <> " " else mempty
prettyExpression GitHub{ arguments, import_, schema = Just schema } =
    Pretty.group (Pretty.flatAlt long short)
  where
    short = prefix
        <>  keyword "github"
        <>  " "
        <>  prettyProjectExpression arguments
        <>  " "
        <>  Pretty.operator ":"
        <>  " "
        <>  pretty schema

    long =  keyword "github"
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.nest 2 (prettyProjectExpression arguments)
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.operator ":"
        <>  " "
        <>  Pretty.nest 4 (pretty schema)

    prefix = if import_ then keyword "import" <> " " else mempty
prettyExpression Annotation{ annotated, annotation } =
    Pretty.group (Pretty.flatAlt long short)
  where
    short = prettyOperatorExpression annotated
        <>  " "
        <>  Pretty.operator ":"
        <>  " "
        <>  pretty annotation

    long =  prettyOperatorExpression annotated
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.operator ":"
        <>  " "
        <>  Pretty.nest 4 (pretty annotation)
prettyExpression other =
    prettyOperatorExpression other

prettyOperator
    :: Pretty a
    => Operator
    -> (Syntax s a -> Doc AnsiStyle)
    -> (Syntax s a -> Doc AnsiStyle)
prettyOperator operator0 prettyNext expression@Operator{ operator = operator1 }
    | operator0 == operator1 = Pretty.group (Pretty.flatAlt long short)
  where
    short = prettyShort expression

    long = pretty (Text.replicate indent " ") <> prettyLong expression

    prettyShort Operator{ left, operator, right }
        | operator0 == operator =
                prettyShort left
            <>  " "
            <>  pretty operator
            <>  " "
            <>  prettyShort right
    prettyShort other =
        prettyNext other

    prettyLong Operator{ left, operator, right }
        | operator0 == operator =
                Pretty.nest indent (prettyLong left)
            <>  Pretty.hardline
            <>  pretty operator
            <>  pretty (Text.replicate spacing " ")
            <>  prettyLong right
    prettyLong other =
            pretty (Text.replicate indent " ")
        <>  Pretty.nest indent (prettyNext other)

    operatorWidth = Text.length (Pretty.toText operator0)

    alignment = 2

    align n = ((n `div` alignment) + 1) * alignment

    indent = align operatorWidth

    spacing = indent - operatorWidth
prettyOperator _ prettyNext other =
    prettyNext other

prettyOperatorExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyOperatorExpression = prettyOrExpression

prettyOrExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyOrExpression = prettyOperator Or prettyAndExpression

prettyAndExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyAndExpression = prettyOperator And prettyEqualExpression

prettyEqualExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyEqualExpression = prettyOperator Equal prettyNotEqualExpression

prettyNotEqualExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyNotEqualExpression = prettyOperator NotEqual prettyLessThanExpression

prettyLessThanExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyLessThanExpression = prettyOperator LessThan prettyLessThanOrEqualExpression

prettyLessThanOrEqualExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyLessThanOrEqualExpression = prettyOperator LessThanOrEqual prettyGreaterThanExpression

prettyGreaterThanExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyGreaterThanExpression = prettyOperator GreaterThan prettyGreaterThanOrEqualExpression

prettyGreaterThanOrEqualExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyGreaterThanOrEqualExpression = prettyOperator GreaterThanOrEqual prettyPlusExpression

prettyPlusExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyPlusExpression = prettyOperator Plus prettyMinusExpression

prettyMinusExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyMinusExpression = prettyOperator Minus prettyTimesExpression

prettyTimesExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyTimesExpression = prettyOperator Times prettyModulusExpression

prettyModulusExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyModulusExpression = prettyOperator Modulus prettyDivideExpression

prettyDivideExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyDivideExpression = prettyOperator Divide prettyApplicationExpression

prettyApplicationExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyApplicationExpression expression
    | isApplication expression = Pretty.group (Pretty.flatAlt long short)
    | otherwise                = prettyProjectExpression expression
  where
    isApplication Application{} = True
    isApplication Fold{}        = True
    isApplication Prompt{}      = True
    isApplication HTTP{}        = True
    isApplication Read{}        = True
    isApplication GitHub{}      = True
    isApplication _             = False

    short = prettyShort expression

    long = prettyLong expression

    prettyShort Application{ function, argument } =
            prettyShort function
        <>  " "
        <>  prettyProjectExpression argument
    prettyShort Fold{ handlers } =
        keyword "fold" <> " " <> prettyProjectExpression handlers
    prettyShort Prompt{ arguments, import_, schema = Nothing } =
        prefix <> keyword "prompt" <> " " <> prettyProjectExpression arguments
      where
        prefix = if import_ then keyword "import" <> " " else mempty
    prettyShort HTTP{ arguments, import_, schema = Nothing } =
        prefix <> keyword "http" <> " " <> prettyProjectExpression arguments
      where
        prefix = if import_ then keyword "import" <> " " else mempty
    prettyShort Read{ arguments, import_, schema = Nothing } =
        prefix <> keyword "read" <> " " <> prettyProjectExpression arguments
      where
        prefix = if import_ then keyword "import" <> " " else mempty
    prettyShort GitHub{ arguments, import_, schema = Nothing } =
        prefix <> keyword "github" <> " " <> prettyProjectExpression arguments
      where
        prefix = if import_ then keyword "import" <> " " else mempty
    prettyShort other =
        prettyProjectExpression other

    prettyLong Application{ function, argument } =
            prettyLong function
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.nest 2 (prettyProjectExpression argument)
    prettyLong Fold{ handlers } =
            keyword "fold"
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.nest 2 (prettyProjectExpression handlers)
    prettyLong Prompt{ import_, arguments } =
            prefix
        <>  keyword "prompt"
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.nest 2 (prettyProjectExpression arguments)
      where
        prefix = if import_ then keyword "import" <> " " else mempty
    prettyLong HTTP{ import_, arguments } =
            prefix
        <>  keyword "http"
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.nest 2 (prettyProjectExpression arguments)
      where
        prefix = if import_ then keyword "import" <> " " else mempty
    prettyLong Read{ import_, arguments } =
            prefix
        <>  keyword "read"
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.nest 2 (prettyProjectExpression arguments)
      where
        prefix = if import_ then keyword "import" <> " " else mempty
    prettyLong GitHub{ import_, arguments } =
            prefix
        <>  keyword "read"
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.nest 2 (prettyProjectExpression arguments)
      where
        prefix = if import_ then keyword "import" <> " " else mempty
    prettyLong other =
        prettyProjectExpression other

prettyProjectExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyProjectExpression expression = case expression of
    Project{ } -> Pretty.group (Pretty.flatAlt long short)
    _          -> prettyAlternativeExpression expression
  where
    short = prettyShort expression

    long = prettyLong expression

    prettyShort Project{ larger, smaller = Single{ single = Field{ field } } } =
            prettyShort larger
        <>  Pretty.operator "."
        <>  Type.prettyRecordLabel False field
    prettyShort Project{ larger, smaller = Multiple{ multiple = [ ] } } =
            prettyShort larger
        <>  Pretty.operator "."
        <>  Pretty.punctuation "{"
        <>  " "
        <>  Pretty.punctuation "}"
    prettyShort Project{ larger, smaller = Multiple{ multiple = Field{ field = f₀ }  : fs } } =
            prettyShort larger
        <>  Pretty.operator "."
        <>  Pretty.punctuation "{"
        <>  " "
        <>  Type.prettyRecordLabel False f₀
        <>  foldMap (\Field{ field = f } -> Pretty.punctuation "," <> " " <> Type.prettyRecordLabel False f) fs
        <>  " "
        <>  Pretty.punctuation "}"
    prettyShort Project{ larger, smaller = Index{ index } } =
            prettyShort larger
        <>  Pretty.operator "."
        <>  Pretty.scalar (pretty index)
    prettyShort other =
        prettyAlternativeExpression other

    prettyLong Project{ larger, smaller = Single{ single = Field{ field } } } =
            prettyLong larger
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.operator "."
        <>  Type.prettyRecordLabel False field
    prettyLong Project{ larger, smaller = Multiple{ multiple = [ ] } } =
            prettyLong larger
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.operator "."
        <>  Pretty.punctuation "{"
        <>  " "
        <>  Pretty.punctuation "}"
    prettyLong Project{ larger, smaller = Multiple{ multiple = Field{ field = f₀ } : fs  } } =
            prettyLong larger
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.operator "."
        <>  " "
        <>  Pretty.nest 4
            (   Pretty.punctuation "{"
            <>  " "
            <>  Type.prettyRecordLabel False f₀
            <>  foldMap (\Field{ field = f } -> Pretty.hardline <> Pretty.punctuation "," <> " " <> Type.prettyRecordLabel False f) fs
            <>  Pretty.hardline
            <>  Pretty.punctuation "}"
            )
    prettyLong Project{ larger, smaller = Index{ index } } =
            prettyLong larger
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.operator "."
        <>  Pretty.scalar (pretty index)
    prettyLong record =
        prettyAlternativeExpression record

prettyAlternativeExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyAlternativeExpression Alternative{ name, argument = argument@Record{ } } =
    Pretty.group (Pretty.flatAlt long short)
  where
    short = Type.prettyAlternativeLabel name
        <>  prettyPrimitiveExpression argument

    long =  Type.prettyAlternativeLabel name
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.nest 2 (prettyPrimitiveExpression argument)
prettyAlternativeExpression Alternative{ name, argument } =
    Pretty.group (Pretty.flatAlt long short)
  where
    short = Type.prettyAlternativeLabel name
        <>  " "
        <>  prettyPrimitiveExpression argument

    long =  Type.prettyAlternativeLabel name
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.nest 2 (prettyPrimitiveExpression argument)
prettyAlternativeExpression other =
    prettyPrimitiveExpression other

prettyPrimitiveExpression :: Pretty a => Syntax s a -> Doc AnsiStyle
prettyPrimitiveExpression Variable{ name } = Type.prettyLabel name
prettyPrimitiveExpression List{ elements = [] } =
    punctuation "[" <> " " <> punctuation "]"
prettyPrimitiveExpression List{ elements = element :<| elements } =
    Pretty.group (Pretty.flatAlt long short)
  where
    short = punctuation "["
        <>  " "
        <>  prettyExpression element
        <>  foldMap (\e -> punctuation "," <> " " <> prettyExpression e) elements
        <>  " "
        <>  punctuation "]"

    long =   punctuation "["
        <>   " "
        <>   prettyLongElement element
        <>   foldMap (\e -> punctuation "," <> " " <> prettyLongElement e) elements
        <>   punctuation "]"

    prettyLongElement e = Pretty.nest 2 (prettyExpression e) <> Pretty.hardline
prettyPrimitiveExpression Record{ fieldValues = [] } =
    punctuation "{" <> " " <> punctuation "}"
prettyPrimitiveExpression Record { fieldValues = fieldValue : fieldValues } =
    Pretty.group (Pretty.flatAlt long short)
  where
    short = punctuation "{"
        <>  " "
        <>  prettyShortFieldValue fieldValue
        <>  foldMap (\fv -> punctuation "," <> " " <> prettyShortFieldValue fv) fieldValues
        <>  " "
        <>  punctuation "}"

    long =  punctuation "{"
        <>  " "
        <>  prettyLongFieldValue fieldValue
        <>  foldMap (\fv -> punctuation "," <> " " <> prettyLongFieldValue fv) fieldValues
        <>  punctuation "}"

    prettyShortFieldValue (field, value) =
            Type.prettyRecordLabel True field
        <>  Pretty.operator ":"
        <>  " "
        <>  prettyExpression value

    prettyLongFieldValue (field, value) =
            Type.prettyRecordLabel True field
        <>  Pretty.operator ":"
        <>  Pretty.hardline
        <>  "    "
        <>  Pretty.nest 4 (prettyExpression value)
        <>  Pretty.hardline
prettyPrimitiveExpression Builtin{ builtin } =
    pretty builtin
prettyPrimitiveExpression Scalar{ scalar } =
    pretty scalar
prettyPrimitiveExpression Text{ chunks } = pretty chunks
prettyPrimitiveExpression Embed{ embedded } =
    pretty embedded
prettyPrimitiveExpression other = Pretty.group (Pretty.flatAlt long short)
  where
    short = punctuation "(" <> prettyExpression other <> punctuation ")"

    long =  punctuation "("
        <>  " "
        <>  Pretty.nest 2 (prettyExpression other)
        <>  Pretty.hardline
        <>  punctuation ")"

{-| A bound field name

    >>> pretty @(FieldName () Void) (FieldName () "x" Nothing Nothing)
    x
    >>> pretty @(FieldName () Void) (FieldName () "x" (Just "A") Nothing)
    x : A
    >>> pretty @(FieldName () Void) (FieldName () "x" Nothing (Just "a"))
    x = a
    >>> pretty @(FieldName () Void) (FieldName () "x" (Just "A") (Just "a"))
    x : A = a
-}
data FieldName s a = FieldName
    { fieldNameLocation :: s
    , name :: Text
    , annotation :: Maybe (Type s)
    , assignment :: Maybe (Syntax s a)
    } deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance Bifunctor FieldName where
    first f FieldName{ fieldNameLocation, name, annotation, assignment } =
        FieldName
            { fieldNameLocation = f fieldNameLocation
            , name
            , annotation = fmap (fmap f) annotation
            , assignment = fmap (first f) assignment
            }

    second = fmap

instance IsString (FieldName () a) where
    fromString string = FieldName
        { fieldNameLocation = ()
        , name = fromString string
        , annotation = Nothing
        , assignment = Nothing
        }

instance Pretty a => Pretty (FieldName s a) where
    pretty FieldName{ name, annotation, assignment } =
            Type.prettyRecordLabel False name
        <>  foldMap renderAnnotation annotation
        <>  foldMap renderAssignment assignment
      where
        renderAnnotation a =
            " " <> punctuation ":" <> " " <> pretty a

        renderAssignment a =
            " " <> punctuation "=" <> " " <> pretty a

{-| A bound variable, possibly with a type annotation

    >>> pretty @(NameBinding () Void) (NameBinding () "x" Nothing Nothing)
    x
    >>> pretty @(NameBinding () Void) (NameBinding () "x" (Just "A") Nothing)
    (x : A)
    >>> pretty @(NameBinding () Void) (NameBinding () "x" Nothing (Just "a"))
    (x = a)
    >>> pretty @(NameBinding () Void) (NameBinding () "x" (Just "A") (Just "a"))
    (x : A = a)
    >>> pretty @(NameBinding () Void) (FieldNamesBinding () [])
    { }
    >>> pretty @(NameBinding () Void) (FieldNamesBinding () [ "x", "y" ])
    { x, y }
-}
data NameBinding s a
    = NameBinding
        { nameLocation :: s
        , name :: Text
        , annotation :: Maybe (Type s)
        , assignment :: Maybe (Syntax s a)
        }
    | FieldNamesBinding
        { fieldNamesLocation :: s
        , fieldNames :: [FieldName s a]
        }
    deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance Bifunctor NameBinding where
    first f NameBinding{ nameLocation, name, annotation, assignment } =
        NameBinding{ nameLocation = f nameLocation, name, annotation = fmap (fmap f) annotation, assignment = fmap (first f) assignment }
    first f FieldNamesBinding{ fieldNamesLocation, fieldNames } =
        FieldNamesBinding{ fieldNamesLocation = f fieldNamesLocation, fieldNames = fmap (first f) fieldNames }

    second = fmap

instance IsString (NameBinding () a) where
    fromString string =
        NameBinding
            { nameLocation = ()
            , name = fromString string
            , annotation = Nothing
            , assignment = Nothing
            }

instance Pretty a => Pretty (NameBinding s a) where
    pretty NameBinding{ name, annotation = Nothing, assignment = Nothing } =
        Type.prettyLabel name
    pretty NameBinding{ name, annotation, assignment } =
            punctuation "("
        <>  Type.prettyLabel name
        <>  foldMap renderAnnotation annotation
        <>  foldMap renderAssignment assignment
        <>  punctuation ")"
      where
        renderAnnotation a =
            " " <> Pretty.operator ":" <> " " <> pretty a

        renderAssignment a =
            " " <> Pretty.operator "=" <> " " <> pretty a

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
    >>> pretty @(Binding () Void) (Binding () "x" [NameBinding () "a" (Just "A") Nothing] (Just "X") "y")
    let x (a : A) : X = y
-}
data Binding s a = Binding
    { nameLocation :: s
    , name :: Text
    , nameBindings :: [NameBinding s a]
    , annotation :: Maybe (Type s)
    , assignment :: Syntax s a
    } deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance Bifunctor Binding where
    first f
        Binding{ nameLocation, name, nameBindings, annotation, assignment } =
            Binding
                { nameLocation = f nameLocation
                , name
                , nameBindings = fmap (first f) nameBindings
                , annotation = fmap (fmap f) annotation
                , assignment = first f assignment
                }

    second = fmap

instance Pretty a => Pretty (Binding s a) where
    pretty Binding{ name, nameBindings, annotation = Nothing, assignment } =
        Pretty.group (Pretty.flatAlt long short)
      where
        long =  keyword "let"
            <>  " "
            <>  Type.prettyLabel name
            <>  Pretty.hardline
            <>  foldMap (\nameBinding -> "      " <> Pretty.nest 6 (pretty nameBinding) <> Pretty.hardline) nameBindings
            <>  "      "
            <>  punctuation "="
            <>  " "
            <>  Pretty.nest 8 (pretty assignment)

        short = keyword "let"
            <>  " "
            <>  Type.prettyLabel name
            <>  " "
            <>  foldMap (\nameBinding -> pretty nameBinding <> " ") nameBindings
            <>  punctuation "="
            <>  " "
            <>  pretty assignment
    pretty Binding{ name, nameBindings, annotation = Just type_, assignment } =
        Pretty.group (Pretty.flatAlt long short)
      where
        long =  keyword "let"
            <>  " "
            <>  Type.prettyLabel name
            <>  Pretty.hardline
            <>  foldMap (\nameBinding -> "      " <> Pretty.nest 6 (pretty nameBinding) <> Pretty.hardline) nameBindings
            <>  "      "
            <>  Pretty.operator ":"
            <>  " "
            <>  Pretty.nest 8 (pretty type_)
            <>  Pretty.hardline
            <>  "      "
            <>  punctuation "="
            <>  " "
            <>  Pretty.nest 8 (pretty assignment)

        short = keyword "let"
            <>  " "
            <>  Type.prettyLabel name
            <>  " "
            <>  foldMap (\nameBinding -> pretty nameBinding <> " ") nameBindings
            <>  Pretty.operator ":"
            <>  " "
            <>  pretty type_
            <>  " "
            <>  punctuation "="
            <>  " "
            <>  pretty assignment

flatten :: Doc ann -> Doc ann
flatten doc = case doc of
    FlatAlt _ y     -> flatten y
    Cat x y         -> Cat (flatten x) (flatten y)
    Nest i x        -> Nest i (flatten x)
    Line            -> Fail
    Union x _       -> flatten x
    Column f        -> Column (flatten . f)
    WithPageWidth f -> WithPageWidth (flatten . f)
    Nesting f       -> Nesting (flatten . f)
    Annotated ann x -> Annotated ann (flatten x)
    _               -> doc
