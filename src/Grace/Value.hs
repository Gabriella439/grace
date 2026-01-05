{-# LANGUAGE OverloadedLists #-}

{-| This module contains the `Value` type used internally for efficient
    evaluation of expressions
-}
module Grace.Value
    ( -- * Types
      Names(..)
    , Value(..)

      -- * Utilities
    , fromJSON
    , toJSON
    , syntax
    , complete
    , effects
    ) where

import Control.Applicative (empty)
import Control.Lens (Getting, Plated(..), Traversal')
import Data.Aeson (FromJSON(..))
import Data.Foldable (toList)
import Data.Generics.Sum (_As)
import Data.Generics.Product (the)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Monoid (Any)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Grace.Context (Context)
import Grace.Location (Location)
import Grace.Syntax (Builtin, Scalar, Syntax)

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.Sequence as Seq
import qualified Grace.Compat as Compat
import qualified Grace.Syntax as Syntax

{-| This is basically `Syntax.NameBinding` but with only the names and not
    the values or locations
-}
data Names = Name Text (Maybe Value) | FieldNames [(Text, Maybe Value)]
    deriving stock (Eq, Show)

{-| This type represents a fully evaluated expression with no reducible
    sub-expressions

    There are two benefits to using a type separate from the surface syntax for
    this purpose:

    * To avoid wastefully reducing the same sub-expression multiple times

    * To use a more efficient representation for reduction purposes
-}
data Value
    = Lambda [(Text, Value)] Names (Syntax Location Void)
      -- The `Lambda` constructor captures the environment at the time it is
      -- evaluated, so that evaluation can be lazily deferred until the function
      -- input is known.  This is essentially the key optimization that powers
      -- the fast normalization-by-evaluation algorithm.
    | Application Value Value
    | List (Seq Value)
    | Record (InsOrdHashMap Text Value)
    | Alternative Text Value
    | Fold Value
    | Text Text
    | Builtin Builtin
    | Scalar Scalar
    deriving stock (Eq, Generic, Show)

instance Plated Value where
    plate onValue value = case value of
        Lambda environment names body -> do
            pure (Lambda environment names body)
        Application function argument -> do
            newFunction <- onValue function
            newArgument <- onValue argument
            return (Application newFunction newArgument)
        List elements -> do
            newElements <- traverse onValue elements
            return (List newElements)
        Record fieldValues -> do
            newFieldValues <- traverse onValue fieldValues
            return (Record newFieldValues)
        Alternative text argument -> do
            newArgument <- onValue argument
            pure (Alternative text newArgument)
        Fold handlers -> do
            newHandlers <- onValue handlers
            return (Fold newHandlers)
        Text text -> do
            pure (Text text)
        Builtin builtin -> do
            pure (Builtin builtin)
        Scalar scalar -> do
            pure (Scalar scalar)

instance FromJSON Value where
    parseJSON value = pure (fromJSON value)

-- | Convert a JSON `Aeson.Value` to a `Value`
fromJSON :: Aeson.Value -> Value
fromJSON (Aeson.Object object) =
    Record (Compat.fromAesonMap (fmap fromJSON object))
fromJSON (Aeson.Array array) =
    List (Seq.fromList (toList (fmap fromJSON array)))
fromJSON (Aeson.String text) =
    Text text
fromJSON (Aeson.Number scientific) =
    Scalar (Syntax.Real scientific)
fromJSON (Aeson.Bool bool) = do
    Scalar (Syntax.Bool bool)
fromJSON Aeson.Null =
    Scalar Syntax.Null

-- | Convert a `Value` to the equivalent JSON `Aeson.Value`
toJSON :: Value -> Maybe Aeson.Value
toJSON (Application (Builtin Syntax.Some) value) = do
    toJSON value
toJSON (List elements) = do
    newElements <- traverse toJSON elements

    return (Aeson.toJSON newElements)
toJSON (Record fieldValues) = do
    newFieldValues <- traverse toJSON fieldValues

    return (Aeson.toJSON (Compat.toAesonMap newFieldValues))
toJSON (Text text) = do
    return (Aeson.toJSON text)
toJSON (Scalar scalar) = do
    return (Aeson.toJSON scalar)
toJSON _ = do
    empty

-- | @Traversal'@ from a `Value` to a 
syntax :: Traversal' Value (Syntax Location Void)
syntax = _As @"Lambda" . the @3

-- | Complete all `Type` annotations in a `Value` using the provided
-- `Context`
complete :: Context Location -> Value -> Value
complete context = Lens.transform (Lens.over syntax (Syntax.complete context))

-- | Determines whether the `Value` has an effect
effects :: Getting Any Value ()
effects = Lens.cosmos . syntax . Syntax.effects
