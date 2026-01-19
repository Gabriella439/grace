{-# LANGUAGE OverloadedLists #-}

{-| This module contains the `Value` type used internally for efficient
    evaluation of expressions
-}
module Grace.Value
    ( -- * Types
      Names(..)
    , Value(..)
    , location

      -- * Utilities
    , quote
    , fromJSON
    , toJSON
    , syntax
    , complete
    , effects
    ) where

import Control.Applicative (empty)
import Control.Lens (Getting, Plated(..), Traversal')
import Data.Aeson (FromJSON(..))
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Generics.Sum (_As)
import Data.Generics.Product (the)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid (Any)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Grace.Context (Context)
import Grace.Pretty (Pretty(..))
import Grace.Syntax (Builtin, Scalar, Syntax)

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.Sequence as Seq
import qualified Grace.Compat as Compat
import qualified Grace.Syntax as Syntax

{-| This is basically `Syntax.NameBinding` but with only the names and not
    the values or locations
-}
data Names location
    = Name location Text (Maybe (Value location))
    | FieldNames location [(location, Text, Maybe (Value location))]
    deriving stock (Eq, Functor, Show)

{-| This type represents a fully evaluated expression with no reducible
    sub-expressions

    There are two benefits to using a type separate from the surface syntax for
    this purpose:

    * To avoid wastefully reducing the same sub-expression multiple times

    * To use a more efficient representation for reduction purposes
-}
data Value location
    = Lambda location [(Text, Value location)] (Names location) (Syntax location Void)
      -- The `Lambda` constructor captures the environment at the time it is
      -- evaluated, so that evaluation can be lazily deferred until the function
      -- input is known.  This is essentially the key optimization that powers
      -- the fast normalization-by-evaluation algorithm.
    | Application location (Value location) (Value location)
    | List location (Seq (Value location))
    | Record location (InsOrdHashMap Text (location, Value location))
    | Alternative location Text (Value location)
    | Fold location (Value location)
    | Text location Text
    | Builtin location Builtin
    | Scalar location Scalar
    deriving stock (Eq, Generic, Show)

location :: Value location -> location
location (Lambda l _ _ _) = l
location (Application l _ _) = l
location (List l _) = l
location (Record l _) = l
location (Alternative l _ _) = l
location (Fold l _) = l
location (Text l _) = l
location (Builtin l _) = l
location (Scalar l _) = l

instance Functor Value where
    fmap f (Lambda location₀ environment names body) =
        Lambda (f location₀) (fmap (fmap (fmap f)) environment) (fmap f names) (first f body)
    fmap f (Application location₀ function argument) =
        Application (f location₀) (fmap f function) (fmap f argument)
    fmap f (List location₀ elements) =
        List (f location₀) (fmap (fmap f) elements)
    fmap f (Record location₀ fieldValues) =
        Record (f location₀) (fmap adapt fieldValues)
      where
        adapt (location₁, value) = (f location₁, fmap f value)
    fmap f (Alternative location₀ text argument) =
        Alternative (f location₀) text (fmap f argument)
    fmap f (Fold location₀ handlers) =
        Fold (f location₀) (fmap f handlers)
    fmap f (Text location₀ text) =
        Text (f location₀) text
    fmap f (Builtin location₀ builtin) =
        Builtin (f location₀) builtin
    fmap f (Scalar location₀ scalar) =
        Scalar (f location₀) scalar

instance Plated (Value location₀) where
    plate onValue value = case value of
        Lambda location₀ environment names body -> do
            pure (Lambda location₀ environment names body)
        Application location₀ function argument -> do
            newFunction <- onValue function
            newArgument <- onValue argument
            return (Application location₀ newFunction newArgument)
        List location₀ elements -> do
            newElements <- traverse onValue elements
            return (List location₀ newElements)
        Record location₀ fieldValues -> do
            let onElement (location₁, value₁) = do
                    newValue <- onValue value₁

                    return (location₁, newValue)

            newFieldValues <- traverse onElement fieldValues

            return (Record location₀ newFieldValues)
        Alternative location₀ text argument -> do
            newArgument <- onValue argument
            pure (Alternative location₀ text newArgument)
        Fold location₀ handlers -> do
            newHandlers <- onValue handlers
            return (Fold location₀ newHandlers)
        Text location₀ text -> do
            pure (Text location₀ text)
        Builtin location₀ builtin -> do
            pure (Builtin location₀ builtin)
        Scalar location₀ scalar -> do
            pure (Scalar location₀ scalar)

instance FromJSON (Value ()) where
    parseJSON value = pure (fromJSON value)

instance Pretty (Value s) where
    pretty value = pretty (quote value)

-- | Convert a `Value` back into the surface `Syntax`
quote :: Value location -> Syntax location Void
quote value = case value of
    Lambda location₀ env names_ body₀ ->
        foldl snoc newLambda env
      where
        binding = case names_ of
            Name nameLocation name assignment ->
                Syntax.PlainBinding
                    { plain = Syntax.NameBinding
                        { nameLocation
                        , name
                        , annotation = Nothing
                        , assignment = fmap quote assignment
                        }
                    }

            FieldNames fieldNamesLocation fieldNames ->
                Syntax.RecordBinding
                    { fieldNamesLocation
                    , fieldNames = do
                        (nameLocation, name, assignment) <- fieldNames
                        return Syntax.NameBinding
                            { nameLocation
                            , name
                            , annotation = Nothing
                            , assignment = fmap quote assignment
                            }
                    }

        newLambda = Syntax.Lambda
            { location = location₀
            , binding
            , body = body₀
            }

        toBinding n v = Syntax.Define
            { assignmentLocation = location₀
            , definition = Syntax.Definition
                { name = n
                , nameLocation = location₀
                , bindings = []
                , annotation = Nothing
                , assignment = quote v
                }
            }

        snoc e@Syntax.Let{ assignments = a :| as, body = body₁ } (n, v)
            | Syntax.usedIn n e = Syntax.Let
                { location = location₀
                , assignments = toBinding n v :| (a : as)
                , body = body₁
                }
            | otherwise = e
        snoc e (n, v)
            | Syntax.usedIn n e = Syntax.Let
                { location = location₀
                , assignments = toBinding n v :| []
                , body = e
                }
            | otherwise = e

    Application location₀ function argument ->
        Syntax.Application
            { location = location₀
            , function = quote function
            , argument = quote argument
            }

    List location₀ elements ->
        Syntax.List{ location = location₀, elements = fmap quote elements }

    Record location₀ fieldValues ->
        Syntax.Record
            { location = location₀
            , fieldValues = map adapt (HashMap.toList fieldValues)
            }
      where
        adapt (field, (nameLocation, value_)) = Syntax.Definition
            { nameLocation
            , name = field
            , bindings = []
            , annotation = Nothing
            , assignment = quote value_
            }

    Alternative location₀ name argument ->
        Syntax.Alternative
            { location = location₀
            , name
            , argument = quote argument
            }

    Fold location₀ handlers ->
        Syntax.Fold{ location = location₀, handlers = quote handlers }

    Text location₀ text ->
        Syntax.Text{ location = location₀, chunks = Syntax.Chunks text [] }

    Scalar location₀ scalar ->
        Syntax.Scalar{ location = location₀, scalar }

    Builtin location₀ builtin ->
        Syntax.Builtin{ location = location₀, builtin }

-- | Convert a JSON `Aeson.Value` to a `Value`
fromJSON :: Aeson.Value -> Value ()
fromJSON (Aeson.Object object) =
    Record () (Compat.fromAesonMap (fmap adapt object))
  where
    adapt json = ((), fromJSON json)
fromJSON (Aeson.Array array) =
    List () (Seq.fromList (toList (fmap fromJSON array)))
fromJSON (Aeson.String text) =
    Text () text
fromJSON (Aeson.Number scientific) =
    Scalar () (Syntax.Real scientific)
fromJSON (Aeson.Bool bool) = do
    Scalar () (Syntax.Bool bool)
fromJSON Aeson.Null =
    Scalar () Syntax.Null

-- | Convert a `Value` to the equivalent JSON `Aeson.Value`
toJSON :: Value location -> Maybe Aeson.Value
toJSON (Application _ (Builtin _ Syntax.Some) value) = do
    toJSON value
toJSON (List _ elements) = do
    newElements <- traverse toJSON elements

    return (Aeson.toJSON newElements)
toJSON (Record _ fieldValues) = do
    let adapt (_, value) = do
            toJSON value

    newFieldValues <- traverse adapt fieldValues

    return (Aeson.toJSON (Compat.toAesonMap newFieldValues))
toJSON (Text _ text) = do
    return (Aeson.toJSON text)
toJSON (Scalar _ scalar) = do
    return (Aeson.toJSON scalar)
toJSON _ = do
    empty

-- | @Traversal'@ from a `Value` to a 
syntax :: Traversal' (Value location) (Syntax location Void)
syntax = _As @"Lambda" . the @4

-- | Complete all `Type` annotations in a `Value` using the provided
-- `Context`
complete :: Context location -> Value location -> Value location
complete context = Lens.transform (Lens.over syntax (Syntax.complete context))

-- | Determines whether the `Value` has an effect
effects :: Getting Any (Value location) ()
effects = Lens.cosmos . syntax . Syntax.effects
