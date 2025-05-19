{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}

{-| This module contains the `Value` type used internally for efficient
    evaluation of expressions
-}
module Grace.Value
    ( -- * Value
      Closure(..)
    , Value(..)
    ) where

import Control.Lens.Plated (Plated(..))
import Data.Aeson (FromJSON(..))
import Data.Foldable (toList)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Void (Void)
import Grace.Location (Location)
import Grace.Syntax (Builtin, Scalar, Syntax)

import qualified Data.Aeson as Aeson
import qualified Data.Sequence as Seq
import qualified Grace.Compat as Compat
import qualified Grace.Syntax as Syntax

{-| A `Closure` captures the current evaluation environment in order to defer
    evaluation until the value of some bound variable is known

    You can think of @Closure name env expression@ as essentially the same thing
    as @\\value -> evaluate ((name, value) : env) e@, except stored using a
    first-order representation.  In fact, you convert to the latter
    representation using `Grace.Normalize.instantiate`.

    This provides efficiency comparable to a higher-order abstract syntax
    tree, except using a first-order representation.
-}
data Closure = Closure Text [(Text, Value)] (Syntax Location Void)
    deriving stock (Eq, Show)

{-| This type represents a fully evaluated expression with no reducible
    sub-expressions

    There are two benefits to using a type separate from the surface syntax for
    this purpose:

    * To avoid wastefully reducing the same sub-expression multiple times

    * To use a more efficient representation for reduction purposes
-}
data Value
    = Lambda Closure
      -- The `Lambda` constructor captures the environment at the time it is
      -- evaluated, so that evaluation can be lazily deferred until the function
      -- input is known.  This is essentially the key optimization that powers
      -- the fast normalization-by-evaluation algorithm.
    | Application Value Value
    | List (Seq Value)
    | Record (InsOrdHashMap Text Value)
    | Alternative Text
    | Merge Value
    | Text Text
    | Prompt Value
    | Builtin Builtin
    | Scalar Scalar
    deriving stock (Eq, Show)

instance FromJSON Value where
    parseJSON (Aeson.Object object) = do
        values <- traverse parseJSON object
        pure (Record (Compat.fromAesonMap values))
    parseJSON (Aeson.Array array) = do
        values <- traverse parseJSON array
        pure (List (Seq.fromList (toList values)))
    parseJSON (Aeson.String text) = do
        pure (Text text)
    parseJSON (Aeson.Number scientific) = do
        pure (Scalar (Syntax.Real scientific))
    parseJSON (Aeson.Bool bool) = do
        pure (Scalar (Syntax.Bool bool))
    parseJSON Aeson.Null = do
        pure (Scalar Syntax.Null)

instance Plated Value where
    plate onValue value = case value of
        Lambda closure -> do
            pure (Lambda closure)
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
        Alternative tag -> do
            pure (Alternative tag)
        Merge handlers -> do
            newHandlers <- onValue handlers
            return (Merge newHandlers)
        Text text -> do
            pure (Text text)
        Prompt arguments -> do
            newArguments <- onValue arguments
            return (Prompt newArguments)
        Builtin builtin -> do
            pure (Builtin builtin)
        Scalar scalar -> do
            pure (Scalar scalar)
