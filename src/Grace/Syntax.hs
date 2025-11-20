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
    , NameBinding(..)
    , Binding(..)
    , Definition(..)
    , BindMonad(..)
    , Assignment(..)
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
    | Lambda { location :: s, binding :: Binding s a, body :: Syntax s a }
    -- ^
    --   >>> pretty @(Syntax () Void) (Lambda () "x" "x")
    --   \x -> x
    --   >>> pretty @(Syntax () Void) (Lambda () (PlainBinding (NameBinding () "x" (Just "A") Nothing)) "x")
    --   \(x : A) -> x
    --   >>> pretty @(Syntax () Void) (Lambda () (PlainBinding (NameBinding () "x" Nothing (Just "a"))) "x")
    --   \(x = a) -> x
    --   >>> pretty @(Syntax () Void) (Lambda () (PlainBinding (NameBinding () "x" (Just "A") (Just "a"))) "x")
    --   \(x : A = a) -> x
    | Application { location :: s, function :: Syntax s a, argument :: Syntax s a }
    -- ^
    --   >>> pretty @(Syntax () Void) (Application () "f" "x")
    --   f x
    | Annotation { location :: s, annotated :: Syntax s a, annotation :: Type s }
    -- ^
    --   >>> pretty @(Syntax () Void) (Annotation () "x" "A")
    --   x : A
    | Let { location :: s, assignments :: NonEmpty (Assignment s a), body :: Syntax s a }
    -- ^
    --   >>> pretty @(Syntax () Void) (Let () (Define () (Definition () "x" [] Nothing "y") :| []) "z")
    --   let x = y in z
    --   >>> pretty @(Syntax () Void) (Let () (Define () (Definition () "x" [PlainBinding (NameBinding () "a" (Just "A") Nothing), PlainBinding (NameBinding () "b" Nothing (Just "e"))] (Just "X") "y") :| []) "z")
    --   let x (a : A) (b = e) : X = y in z
    --   >>> pretty @(Syntax () Void) (Let () (Define () (Definition () "a" [] Nothing "b") :| [ Define () (Definition () "c" [] Nothing "d") ]) "e")
    --   let a = b let c = d in e
    | List { location :: s, elements :: Seq (Syntax s a) }
    -- ^
    --   >>> pretty @(Syntax () Void) (List () [ "x", "y", "z" ])
    --   [ x, y, z ]
    | Record { location :: s, fieldValues :: [Definition s a] }
    -- ^
    --   >>> pretty @(Syntax () Void) (Record () [ Definition () "x" [] Nothing "a", Definition () "y" [] Nothing "b" ])
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
    | Show{ location :: s, export :: Bool, arguments :: Syntax s a, schema :: Maybe (Type s) }
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

    Lambda{ location, binding, body } >>= f = Lambda
        { location
        , binding = onBinding binding
        , body = body >>= f
        }
      where
        onBinding PlainBinding{ plain = NameBinding{ nameLocation, name, annotation, assignment} } =
            PlainBinding
                { plain = NameBinding
                    { nameLocation
                    , name
                    , annotation
                    , assignment = fmap (>>= f) assignment
                    }
                }
        onBinding RecordBinding{ fieldNamesLocation, fieldNames } =
            RecordBinding
                { fieldNamesLocation
                , fieldNames = fmap onFieldName fieldNames
                }

        onFieldName
            NameBinding{ nameLocation, name, annotation, assignment } =
                NameBinding
                    { nameLocation
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

    Let{ location, assignments, body } >>= f = Let
        { location
        , assignments = fmap onAssignment assignments
        , body = body >>= f
        }
      where
        onAssignment
            Define{ assignmentLocation, definition = Definition{ nameLocation, name, bindings, annotation, assignment } } = Define
                { assignmentLocation
                , definition = Definition
                    { name
                    , nameLocation
                    , bindings = fmap onBinding bindings
                    , annotation
                    , assignment = assignment >>= f
                    }
                }
        onAssignment
            Bind{ assignmentLocation, monad, binding, assignment } = Bind
                { assignmentLocation
                , monad
                , binding = onBinding binding
                , assignment = assignment >>= f
                }

        onBinding
            PlainBinding{ plain = NameBinding{ nameLocation, name, annotation, assignment } } =
                PlainBinding
                    { plain = NameBinding
                        { nameLocation
                        , name
                        , annotation
                        , assignment = fmap (>>= f) assignment
                        }
                    }
        onBinding RecordBinding{ fieldNamesLocation, fieldNames } =
            RecordBinding
                { fieldNamesLocation
                , fieldNames = fmap onFieldName fieldNames
                }

        onFieldName
            NameBinding{ nameLocation, name, annotation, assignment } =
                NameBinding
                    { nameLocation
                    , name
                    , annotation
                    , assignment = fmap (>>= f) assignment
                    }

    List{ location, elements } >>= f =
        List{ location, elements = fmap (>>= f) elements }

    Record{ location, fieldValues } >>= f =
        Record{ location, fieldValues = fmap onDefinition fieldValues }
      where
        onDefinition Definition{ nameLocation, name, bindings, annotation, assignment } =
            Definition
                { nameLocation
                , name
                , bindings = fmap onBinding bindings
                , annotation
                , assignment = assignment >>= f
                }

        onBinding
            PlainBinding{ plain = NameBinding{ nameLocation, name, annotation, assignment } } =
                PlainBinding
                    { plain = NameBinding
                        { nameLocation
                        , name
                        , annotation
                        , assignment = fmap (>>= f) assignment
                        }
                    }
        onBinding RecordBinding{ fieldNamesLocation, fieldNames } =
            RecordBinding
                { fieldNamesLocation
                , fieldNames = fmap onFieldName fieldNames
                }

        onFieldName
            NameBinding{ nameLocation, name, annotation, assignment } =
                NameBinding
                    { nameLocation
                    , name
                    , annotation
                    , assignment = fmap (>>= f) assignment
                    }

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

    Show{ location, export, arguments, schema } >>= f =
        Show{ location, export, arguments = arguments >>= f, schema }

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

            Lambda{ location, binding, body } -> do
                newBody <- onSyntax body

                return Lambda{ location, binding, body = newBody }

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

            Let{ location, assignments, body } -> do
                let onFieldName
                        NameBinding{ nameLocation, name, annotation, assignment } = do
                            newAssignment <- traverse onSyntax assignment

                            return NameBinding
                                { nameLocation
                                , name
                                , annotation
                                , assignment = newAssignment
                                }

                let onBinding
                        PlainBinding{ plain = NameBinding{ nameLocation, name, annotation, assignment } } = do
                            newAssignment <- traverse onSyntax assignment

                            return PlainBinding
                                { plain = NameBinding
                                    { nameLocation
                                    , name
                                    , annotation
                                    , assignment = newAssignment
                                    }
                                }

                    onBinding
                        RecordBinding{ fieldNamesLocation, fieldNames } = do
                            newFieldNames <- traverse onFieldName fieldNames

                            return RecordBinding
                                { fieldNamesLocation
                                , fieldNames = newFieldNames
                                }

                let onAssignment
                        Define{ assignmentLocation, definition = Definition{ nameLocation, name, bindings, annotation, assignment } } = do
                            newAssignment <- onSyntax assignment

                            newBindings <- traverse onBinding bindings

                            return Define
                                { assignmentLocation
                                , definition = Definition
                                    { nameLocation
                                    , name
                                    , bindings = newBindings
                                    , annotation
                                    , assignment = newAssignment
                                    }
                                }
                    onAssignment
                        Bind{ assignmentLocation, monad, binding, assignment } = do
                            newBinding <- onBinding binding

                            newAssignment <- onSyntax assignment

                            return Bind
                                { assignmentLocation
                                , monad
                                , binding = newBinding
                                , assignment = newAssignment
                                }

                newAssignments <- traverse onAssignment assignments

                newBody <- onSyntax body

                return Let
                    { location
                    , assignments = newAssignments
                    , body = newBody
                    }

            List{ location, elements } -> do
                newElements <- traverse onSyntax elements

                return List{ location, elements = newElements }

            Record{ location, fieldValues } -> do
                let onNameBinding
                        NameBinding{ nameLocation, name, annotation, assignment } = do
                            newAssignment <- traverse onSyntax assignment

                            return NameBinding
                                { nameLocation
                                , name
                                , annotation
                                , assignment = newAssignment
                                }

                let onBinding
                        PlainBinding{ plain } = do
                            newPlain <- onNameBinding plain

                            return PlainBinding{ plain = newPlain }
                    onBinding
                        RecordBinding{ fieldNamesLocation, fieldNames } = do
                            newFieldNames <- traverse onNameBinding fieldNames

                            return RecordBinding
                                { fieldNamesLocation
                                , fieldNames = newFieldNames
                                }

                let onDefinition Definition{ name, nameLocation, bindings, annotation, assignment } = do
                        newBindings <- traverse onBinding bindings

                        newAssignment <- onSyntax assignment

                        return Definition
                            { name
                            , nameLocation
                            , bindings = newBindings
                            , annotation
                            , assignment = newAssignment
                            }

                newFieldValues <- traverse onDefinition fieldValues

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

            Show{ location, export, arguments, schema } -> do
                newArguments <- onSyntax arguments

                return Show{ location, export, arguments = newArguments, schema }

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

    first f Lambda{ location, binding, body } = Lambda
        { location = f location
        , binding = first f binding
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

    first f Let{ location, assignments, body } = Let
        { location = f location
        , assignments = fmap (first f) assignments
        , body = first f body
        }

    first f List{ location, elements } =
        List{ location = f location, elements = fmap (first f) elements }

    first f Record{ location, fieldValues } =
        Record{ location = f location, fieldValues = fmap (first f) fieldValues }

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

    first f Show{ location, export, arguments, schema } = Show
        { location = f location
        , export
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
usedIn name₀ Lambda{ binding = PlainBinding{ plain = NameBinding{ name = name₁ } }, body } =
    (name₀ /= name₁) && usedIn name₀ body
usedIn name₀ Lambda{ binding = RecordBinding{ fieldNames }, body } =
    (name₀ `notElem` fmap toName fieldNames) && usedIn name₀ body
  where
    toName NameBinding{ name = name₁ } = name₁
usedIn name₀ Application{ function, argument } =
    usedIn name₀ function || usedIn name₀ argument
usedIn name₀ Annotation{ annotated } =
    usedIn name₀ annotated
usedIn name₀ Let{ assignments = Define{ definition = Definition{ name = name₁, assignment } } :| [], body } =
    usedIn name₀ assignment || (name₀ /= name₁ && usedIn name₀ body)
usedIn name₀ Let{ assignments = Bind{ binding, assignment } :| [], body } =
    usedIn name₀ assignment || (name₀ `notElem` toNames binding && usedIn name₀ body)
  where
    toName NameBinding{ name = name₁ } = name₁

    toNames PlainBinding{ plain } = [ toName plain ]
    toNames RecordBinding{ fieldNames } = map toName fieldNames
usedIn name₀ Let{ location, assignments = Define{ definition = Definition{ name = name₁, assignment } } :| (a : as), body } =
    usedIn name₀ assignment || (name₀ /= name₁ && usedIn name₀ Let{ location, assignments = a :| as, body })
usedIn name₀ Let{ location, assignments = Bind{ binding, assignment } :| (a : as), body } =
    usedIn name₀ assignment || (name₀ `notElem` toNames binding && usedIn name₀ Let{ location, assignments = a :| as, body })
  where
    toName NameBinding{ name = name₁ } = name₁

    toNames PlainBinding{ plain } = [ toName plain ]
    toNames RecordBinding{ fieldNames } = map toName fieldNames
usedIn name₀ List{ elements } =
    any (usedIn name₀) elements
usedIn name₀ Record{ fieldValues } = any onDefinition fieldValues
  where
    onDefinition Definition{ bindings, assignment } =
        name₀ `notElem` concatMap toNames bindings || usedIn name₀ assignment

    toName NameBinding{ name = name₁ } = name₁

    toNames PlainBinding{ plain } = [ toName plain ]
    toNames RecordBinding{ fieldNames } = map toName fieldNames
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
usedIn name₀ Show{ arguments } =
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
    Lambda{ location, binding = PlainBinding{ plain = NameBinding{ nameLocation, name, annotation, assignment } }, body } = do
        newAnnotation <- traverse onType annotation

        return Lambda
            { location
            , binding = PlainBinding
                { plain = NameBinding
                    { nameLocation
                    , name
                    , annotation = newAnnotation
                    , assignment
                    }
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
types onType Show{ location, export, arguments, schema } = do
    newSchema <- traverse onType schema

    return Show{ location, export, arguments, schema = newSchema }
types onType Let{ location, assignments, body } = do
    newAssignments <- traverse onAssignment assignments

    return Let{ location, assignments = newAssignments, body }
  where
    onAssignment
        Define{ assignmentLocation, definition = Definition{ nameLocation, name, bindings, annotation, assignment } } = do
            newBindings <- traverse onBinding bindings

            newAnnotation <- traverse onType annotation

            return Define
                { assignmentLocation
                , definition = Definition
                    { nameLocation
                    , name
                    , bindings = newBindings
                    , annotation = newAnnotation
                    , assignment
                    }
                }
    onAssignment
        Bind{ assignmentLocation, monad, binding, assignment } = do
            newBinding <- onBinding binding

            return Bind
                { assignmentLocation
                , monad
                , binding = newBinding
                , assignment
                }

    onBinding PlainBinding{ plain = NameBinding{ nameLocation, name, annotation, assignment } } = do
        newAnnotation <- traverse onType annotation

        return PlainBinding
            { plain = NameBinding
                { nameLocation
                , name
                , annotation = newAnnotation
                , assignment
                }
            }
    onBinding RecordBinding{ fieldNamesLocation, fieldNames } = do
        newFieldNames <- traverse onFieldName fieldNames

        return RecordBinding
            { fieldNamesLocation
            , fieldNames = newFieldNames
            }

    onFieldName
        NameBinding{ nameLocation, name, annotation, assignment } = do
            newAnnotation <- traverse onType annotation

            return NameBinding
                { nameLocation
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
    -- ^
    --   >>> pretty (Key "secret")
    --   🔒
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

    prettyShort Lambda{ binding, body } =
            pretty binding
        <>  " "
        <>  prettyShort body
    prettyShort body =
            punctuation "->"
        <>  " "
        <>  prettyExpression body

    prettyLong Lambda{ binding, body } =
            punctuation "\\"
        <>  pretty binding
        <>  " "
        <>  punctuation "->"
        <>  Pretty.hardline
        <>  prettyLong body
    prettyLong body =
        "  " <> Pretty.nest 2 (prettyExpression body)

prettyExpression Let{ assignments, body }
    = Pretty.group (Pretty.flatAlt long short)
  where
    short = foldMap (\assignment -> pretty assignment <> " ") assignments
        <>  keyword "in"
        <>  " "
        <>  prettyExpression body

    long =  foldMap (\assignment -> pretty assignment <> Pretty.hardline <> Pretty.hardline) assignments
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

    long =  prefix
        <>  keyword "read"
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

    long =  prefix
        <>  keyword "github"
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
    isApplication Show{}        = True
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
    prettyShort Show{ arguments, export, schema } =
        prefix <> keyword "show" <> " " <> prettyProjectExpression annotatedArguments
      where
        prefix = if export then keyword "export" <> " " else mempty

        annotatedArguments = case schema of
            Nothing -> arguments

            Just s -> Annotation
                { location = Type.location s
                , annotated = arguments
                , annotation = s
                }
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
        <>  keyword "github"
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.nest 2 (prettyProjectExpression arguments)
      where
        prefix = if import_ then keyword "import" <> " " else mempty
    prettyLong Show{ export, arguments, schema } =
            prefix
        <>  keyword "show"
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.nest 2 (prettyProjectExpression annotatedArguments)
      where
        prefix = if export then keyword "export" <> " " else mempty

        annotatedArguments = case schema of
            Nothing -> arguments

            Just s -> Annotation
                { location = Type.location s
                , annotated = arguments
                , annotation = s
                }
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

    prettyShortFieldValue Definition{ name, bindings, assignment } =
            Type.prettyRecordLabel True name
        <>  foldMap renderBinding bindings
        <>  Pretty.operator ":"
        <>  " "
        <>  prettyExpression assignment
      where
        renderBinding binding = " " <> pretty binding

    prettyLongFieldValue Definition{ name, bindings, assignment } =
            Type.prettyRecordLabel True name
        <>  foldMap renderBinding bindings
        <>  Pretty.operator ":"
        <>  Pretty.hardline
        <>  "    "
        <>  Pretty.nest 4 (prettyExpression assignment)
        <>  Pretty.hardline
      where
        renderBinding binding =
                Pretty.hardline
            <>  "    "
            <>  Pretty.nest 4
                    (   Pretty.punctuation "("
                    <>  " "
                    <>  Pretty.nest 2 (pretty binding)
                    <>  Pretty.hardline
                    <>  Pretty.punctuation ")"
                    )
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

    >>> pretty @(NameBinding () Void) (NameBinding () "x" Nothing Nothing)
    x
    >>> pretty @(NameBinding () Void) (NameBinding () "x" (Just "A") Nothing)
    x : A
    >>> pretty @(NameBinding () Void) (NameBinding () "x" Nothing (Just "a"))
    x = a
    >>> pretty @(NameBinding () Void) (NameBinding () "x" (Just "A") (Just "a"))
    x : A = a
-}
data NameBinding s a = NameBinding
    { nameLocation :: s
    , name :: Text
    , annotation :: Maybe (Type s)
    , assignment :: Maybe (Syntax s a)
    } deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance Bifunctor NameBinding where
    first f NameBinding{ nameLocation, name, annotation, assignment } =
        NameBinding
            { nameLocation = f nameLocation
            , name
            , annotation = fmap (fmap f) annotation
            , assignment = fmap (first f) assignment
            }

    second = fmap

instance IsString (NameBinding () a) where
    fromString string = NameBinding
        { nameLocation = ()
        , name = fromString string
        , annotation = Nothing
        , assignment = Nothing
        }

instance Pretty a => Pretty (NameBinding s a) where
    pretty NameBinding{ name, annotation, assignment } =
            Type.prettyRecordLabel False name
        <>  foldMap renderAnnotation annotation
        <>  foldMap renderAssignment assignment
      where
        renderAnnotation a =
            " " <> punctuation ":" <> " " <> pretty a

        renderAssignment a =
            " " <> punctuation "=" <> " " <> pretty a

{-| A bound variable, possibly with a type annotation

    >>> pretty @(Binding () Void) (PlainBinding (NameBinding () "x" Nothing Nothing))
    x
    >>> pretty @(Binding () Void) (PlainBinding (NameBinding () "x" (Just "A") Nothing))
    (x : A)
    >>> pretty @(Binding () Void) (PlainBinding (NameBinding () "x" Nothing (Just "a")))
    (x = a)
    >>> pretty @(Binding () Void) (PlainBinding (NameBinding () "x" (Just "A") (Just "a")))
    (x : A = a)
    >>> pretty @(Binding () Void) (RecordBinding () [])
    { }
    >>> pretty @(Binding () Void) (RecordBinding () [ "x", "y" ])
    { x, y }
-}
data Binding s a
    = PlainBinding{ plain :: NameBinding s a }
    | RecordBinding
        { fieldNamesLocation :: s
        , fieldNames :: [NameBinding s a]
        }
    deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance Bifunctor Binding where
    first f PlainBinding{ plain = NameBinding{ nameLocation, name, annotation, assignment } } =
        PlainBinding{ plain = NameBinding{ nameLocation = f nameLocation, name, annotation = fmap (fmap f) annotation, assignment = fmap (first f) assignment } }
    first f RecordBinding{ fieldNamesLocation, fieldNames } =
        RecordBinding{ fieldNamesLocation = f fieldNamesLocation, fieldNames = fmap (first f) fieldNames }

    second = fmap

instance IsString (Binding () a) where
    fromString string = PlainBinding
        { plain = NameBinding
            { nameLocation = ()
            , name = fromString string
            , annotation = Nothing
            , assignment = Nothing
            }
        }

instance Pretty a => Pretty (Binding s a) where
    pretty PlainBinding{ plain = NameBinding{ name, annotation = Nothing, assignment = Nothing } } =
        Type.prettyLabel name
    pretty PlainBinding{ plain = NameBinding{ name, annotation, assignment } } =
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

    pretty RecordBinding{ fieldNames = [ ] } =
            punctuation "{"
        <>  " "
        <>  punctuation "}"

    pretty RecordBinding{ fieldNames = fieldName : fieldNames } =
            punctuation "{"
        <>  " "
        <>  pretty fieldName
        <>  foldMap (\f -> punctuation "," <> " " <> pretty f) fieldNames
        <>  " "
        <>  punctuation "}"

data Definition s a = Definition
    { nameLocation :: s
    , name :: Text
    , bindings :: [Binding s a]
    , annotation :: Maybe (Type s)
    , assignment :: Syntax s a
    } deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance Bifunctor Definition where
    first f Definition{ nameLocation, name, bindings, annotation, assignment } =
        Definition
            { nameLocation = f nameLocation
            , name
            , bindings = fmap (first f) bindings
            , annotation = fmap (fmap f) annotation
            , assignment = first f assignment
            }

    second = fmap

-- | The monad that a `Bind` takes place in
data BindMonad = UnknownMonad | OptionalMonad | ListMonad
    deriving stock (Eq, Generic, Lift, Show)

instance Pretty BindMonad where
    pretty UnknownMonad  = "unknown"
    pretty OptionalMonad = Pretty.builtin "Optional"
    pretty ListMonad     = Pretty.builtin "List"

{-| The assignment part of a @let@ binding

    >>> pretty @(Assignment () Void) (Define () (Definition () "x" [] Nothing "y"))
    let x = y
    >>> pretty @(Assignment () Void) (Define () (Definition () "x" [] (Just "X") "y"))
    let x : X = y
    >>> pretty @(Assignment () Void) (Define () (Definition () "x" [PlainBinding (NameBinding () "a" (Just "A") Nothing)] (Just "X") "y"))
    let x (a : A) : X = y
-}
data Assignment s a
    = Define
        { assignmentLocation :: s
        , definition :: Definition s a
        }
    | Bind
        { assignmentLocation :: s
        , monad :: Maybe BindMonad
        , binding :: Binding s a
        , assignment :: Syntax s a
        }
    deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance Bifunctor Assignment where
    first f
        Define{ assignmentLocation, definition = Definition{ nameLocation, name, bindings, annotation, assignment } } = Define
            { assignmentLocation = f assignmentLocation
            , definition = Definition
                { nameLocation = f nameLocation
                , name
                , bindings = fmap (first f) bindings
                , annotation = fmap (fmap f) annotation
                , assignment = first f assignment
                }
            }
    first f
        Bind{ assignmentLocation, monad, binding, assignment } = Bind
            { assignmentLocation = f assignmentLocation
            , monad
            , binding = first f binding
            , assignment = first f assignment
            }

    second = fmap

instance Pretty a => Pretty (Assignment s a) where
    pretty Define{ definition = Definition{ name, bindings, annotation = Nothing, assignment } } =
        Pretty.group (Pretty.flatAlt long short)
      where
        long =  keyword "let"
            <>  " "
            <>  Type.prettyLabel name
            <>  Pretty.hardline
            <>  foldMap (\binding -> "      " <> Pretty.nest 6 (pretty binding) <> Pretty.hardline) bindings
            <>  "      "
            <>  punctuation "="
            <>  " "
            <>  Pretty.nest 8 (pretty assignment)

        short = keyword "let"
            <>  " "
            <>  Type.prettyLabel name
            <>  " "
            <>  foldMap (\binding -> pretty binding <> " ") bindings
            <>  punctuation "="
            <>  " "
            <>  pretty assignment
    pretty Define{ definition = Definition{ name, bindings, annotation = Just type_, assignment } } =
        Pretty.group (Pretty.flatAlt long short)
      where
        long =  keyword "let"
            <>  " "
            <>  Type.prettyLabel name
            <>  Pretty.hardline
            <>  foldMap (\binding -> "      " <> Pretty.nest 6 (pretty binding) <> Pretty.hardline) bindings
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
            <>  foldMap (\binding -> pretty binding <> " ") bindings
            <>  Pretty.operator ":"
            <>  " "
            <>  pretty type_
            <>  " "
            <>  punctuation "="
            <>  " "
            <>  pretty assignment
    pretty Bind{ monad = Nothing, binding, assignment } =
        Pretty.group (Pretty.flatAlt long short)
      where
        long =  keyword "let"
            <>  " "
            <>  pretty binding
            <>  Pretty.hardline
            <>  "      "
            <>  punctuation "="
            <>  " "
            <>  Pretty.nest 8 (pretty assignment)

        short = keyword "let"
            <>  " "
            <>  pretty binding
            <>  " "
            <>  punctuation "="
            <>  " "
            <>  pretty assignment
    pretty Bind{ monad = Just _, binding, assignment } =
        Pretty.group (Pretty.flatAlt long short)
      where
        long =  keyword "for"
            <>  " "
            <>  pretty binding
            <>  Pretty.hardline
            <>  "      "
            <>  punctuation "of"
            <>  " "
            <>  Pretty.nest 8 (pretty assignment)

        short = keyword "for"
            <>  " "
            <>  pretty binding
            <>  " "
            <>  punctuation "of"
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
