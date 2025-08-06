{-# LANGUAGE OverloadedLists #-}

{-| This module contains the `Value` type used internally for efficient
    evaluation of expressions
-}
module Grace.Value
    ( -- * Types
      Names(..)
    , Value(..)

      -- * Utilities
    , toJSON
    , fromJSON
    , effects

      -- * Exceptions
    , InvalidJSON(..)
    ) where

import Control.Applicative (empty)
import Control.Exception.Safe (Exception(..))
import Control.Lens (Getting, Plated(..))
import Data.Aeson (FromJSON(..))
import Data.Foldable (toList)
import Data.Generics.Sum (_As)
import Data.Generics.Product (the)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Monoid (Any)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics (Generic)
import Grace.Location (Location)
import Grace.Syntax (Builtin, Scalar, Syntax)
import Grace.Type (Type)

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.Scientific as Scientific
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Grace.Compat as Compat
import qualified Grace.Monotype as Monotype
import qualified Grace.Pretty as Pretty
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type

{-| This is basically `Syntax.NameBinding` but with only the names and not
    the values or locations
-}
data Names = Name Text | FieldNames [Text]
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
    | Alternative Text
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
        Alternative text -> do
            pure (Alternative text)
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

-- | Convert from JSON using the given type
fromJSON :: Type a -> Aeson.Value -> Either (InvalidJSON a) Value
fromJSON Type.Union{ Type.alternatives = Type.Alternatives alternativeTypes _ } (Aeson.Object [("contents", contents), ("tag", Aeson.String tag)])
    | Just alternativeType <- Prelude.lookup tag alternativeTypes = do
        value <- fromJSON alternativeType contents

        return (Application (Alternative tag) value)
fromJSON type_@Type.Record{ Type.fields = Type.Fields fieldTypes _ } value@(Aeson.Object object) = do
    let properties = HashMap.toList (Compat.fromAesonMap object)

    let process (key, fieldType) = case Prelude.lookup key properties of
            Just v -> do
                expression <- fromJSON fieldType v

                return (key, expression)
            Nothing -> do
                Left InvalidJSON{ value, type_ }

    textValues <- traverse process fieldTypes

    return (Record (HashMap.fromList textValues))
fromJSON Type.List{ Type.type_ } (Aeson.Array vector) = do
    elements <- traverse (fromJSON type_) vector
    return (List (Seq.fromList (toList elements)))
fromJSON Type.Scalar{ Type.scalar = Monotype.Text } (Aeson.String text) = do
    return (Text text)
fromJSON type_@Type.Scalar{ Type.scalar } value@(Aeson.Number scientific) =
    case Scientific.floatingOrInteger scientific of
        Left (_ :: Double)
            | scalar == Monotype.Real -> do
                return (Scalar (Syntax.Real scientific))
        Right (integer :: Integer)
            | 0 <= integer
            , scalar `elem` ([ Monotype.Natural, Monotype.Integer, Monotype.Real ] :: [Monotype.Scalar]) -> do
                return (Scalar (Syntax.Natural (fromInteger integer)))
            | scalar `elem` ([ Monotype.Integer, Monotype.Real ] :: [Monotype.Scalar]) -> do
                return (Scalar (Syntax.Integer integer))
        _ -> do
            Left InvalidJSON{ value, type_ }
fromJSON Type.Scalar{ Type.scalar = Monotype.Bool } (Aeson.Bool bool) =
    return (Scalar (Syntax.Bool bool))
fromJSON Type.Optional{ } Aeson.Null =
    return (Scalar Syntax.Null)
fromJSON Type.Scalar{ Type.scalar = Monotype.JSON } value = do
    let Just v = Aeson.Types.parseMaybe Aeson.parseJSON value
    return v
fromJSON type_ value = do
    Left InvalidJSON{ value, type_ }

-- | Invalid JSON output which didn't match the expected type
data InvalidJSON a = InvalidJSON
    { value :: Aeson.Value
    , type_ :: Type a
    } deriving stock (Show)

instance (Show a, Typeable a) => Exception (InvalidJSON a) where
    displayException InvalidJSON{ value, type_} =
        "Invalid JSON\n\
        \\n\
        \The server responded with the following JSON value:\n\
        \\n\
        \" <> string <> "\n\
        \\n\
        \… which does not match the following expected type:\n\
        \\n\
        \" <> Text.unpack (Pretty.toSmart type_)
      where
        bytes = ByteString.Lazy.toStrict (Aeson.encode value)

        string = case Encoding.decodeUtf8' bytes of
            Left  _    -> show bytes
            Right text -> Text.unpack text

-- | Determines whether the `Value` has an effect
effects :: Getting Any Value ()
effects = Lens.cosmos . _As @"Lambda" . the @3 . Syntax.effects
