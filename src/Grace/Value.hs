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
    , inferJSON
    , checkJSON
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
import qualified Data.Aeson.Encode.Pretty as Aeson.Pretty
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

-- | Convert from JSON, inferring the value purely from the JSON data
inferJSON :: Aeson.Value -> Value
inferJSON (Aeson.Object [("contents", contents), ("tag", Aeson.String tag)]) =
    Application (Alternative tag) value
  where
    value = inferJSON contents
inferJSON (Aeson.Object object) = Record (HashMap.fromList textValues)
  where
    properties = HashMap.toList (Compat.fromAesonMap object)

    textValues = fmap (fmap inferJSON) properties
inferJSON (Aeson.Array vector) = List (Seq.fromList (toList elements))
  where
    elements = fmap inferJSON vector
inferJSON (Aeson.String text) = Text text
inferJSON (Aeson.Number scientific) =
    case Scientific.floatingOrInteger scientific of
        Left (_ :: Double) ->
            Scalar (Syntax.Real scientific)
        Right (integer :: Integer)
            | 0 <= integer -> do
                Scalar (Syntax.Natural (fromInteger integer))
            | otherwise -> do
                Scalar (Syntax.Integer integer)
inferJSON (Aeson.Bool bool) =
    Scalar (Syntax.Bool bool)
inferJSON Aeson.Null =
    Scalar Syntax.Null

-- | Convert from JSON using the given type to aid in the conversion process
checkJSON :: Type a -> Aeson.Value -> Either (InvalidJSON a) Value
checkJSON = loop []
  where
    loop path Type.Union{ Type.alternatives = Type.Alternatives alternativeTypes _ } (Aeson.Object [("contents", contents), ("tag", Aeson.String tag)])
        | Just alternativeType <- Prelude.lookup tag alternativeTypes = do
            value <- loop ("contents" : path) alternativeType contents

            return (Application (Alternative tag) value)
    loop path Type.Record{ Type.fields = Type.Fields fieldTypes _ } (Aeson.Object object) = do
        let properties = HashMap.toList (Compat.fromAesonMap object)

        let process (key, property) = case Prelude.lookup key fieldTypes of
                Just fieldType -> do
                    expression <- loop (key : path) fieldType property

                    return [(key, expression)]
                Nothing -> do
                    return ([] :: [(Text, Value)])

        textValuess <- traverse process properties

        return (Record (HashMap.fromList (concat textValuess)))
    loop path type_@Type.Scalar{ scalar = Monotype.JSON } (Aeson.Object object) = do
        let properties = HashMap.toList (Compat.fromAesonMap object)

        let process (key, property) = do
                expression <- loop (key : path) type_ property

                return (key, expression)

        textValues <- traverse process properties

        return (Record (HashMap.fromList textValues))
    loop path Type.List{ Type.type_ } (Aeson.Array vector) = do
        elements <- traverse (loop ("*" : path) type_) vector

        return (List (Seq.fromList (toList elements)))
    loop path type_@Type.Scalar{ scalar = Monotype.JSON } (Aeson.Array vector) = do
        elements <- traverse (loop ("*" : path) type_) vector

        return (List (Seq.fromList (toList elements)))
    loop _ Type.Scalar{ scalar = Monotype.Text } (Aeson.String text) = do
        return (Text text)
    loop _ Type.Scalar{ scalar = Monotype.JSON } (Aeson.String text) = do
        return (Text text)
    loop _ Type.Scalar{ scalar = Monotype.Real } (Aeson.Number scientific) = do
        return (Scalar (Syntax.Real scientific))
    loop path type_@Type.Scalar{ scalar = Monotype.Integer } value@(Aeson.Number scientific) = do
        case Scientific.floatingOrInteger @Double @Integer scientific of
            Right integer -> do
                return (Scalar (Syntax.Integer integer))
            _ -> do
                Left InvalidJSON{ path, value, type_ }
    loop path type_@Type.Scalar{ scalar = Monotype.Natural } value@(Aeson.Number scientific) =
        case Scientific.floatingOrInteger @Double @Integer scientific of
            Right integer
                | 0 <= integer -> do
                    return (Scalar (Syntax.Natural (fromInteger integer)))
            _ -> do
                Left InvalidJSON{ path, value, type_ }
    loop _ Type.Scalar{ scalar = Monotype.JSON } (Aeson.Number scientific) =
        case Scientific.floatingOrInteger scientific of
            Left (_ :: Double) -> do
                return (Scalar (Syntax.Real scientific))
            Right (integer :: Integer)
                | 0 <= integer -> do
                    return (Scalar (Syntax.Natural (fromInteger integer)))
                | otherwise -> do
                    return (Scalar (Syntax.Integer integer))
    loop _ Type.Scalar{ Type.scalar = Monotype.Bool } (Aeson.Bool bool) =
        return (Scalar (Syntax.Bool bool))
    loop _ Type.Scalar{ Type.scalar = Monotype.JSON } (Aeson.Bool bool) =
        return (Scalar (Syntax.Bool bool))
    loop _ Type.Optional{ } Aeson.Null =
        return (Scalar Syntax.Null)
    loop path Type.Optional{ type_ } value = do
        result <- loop path type_ value
        return (Application (Builtin Syntax.Some) result)
    loop _ Type.Scalar{ scalar = Monotype.JSON } Aeson.Null =
        return (Scalar Syntax.Null)
    loop path type_ value = do
        Left InvalidJSON{ path, value, type_ }

-- | Invalid JSON output which didn't match the expected type
data InvalidJSON a = InvalidJSON
    { path :: [Text]
    , value :: Aeson.Value
    , type_ :: Type a
    } deriving stock (Show)

instance (Show a, Typeable a) => Exception (InvalidJSON a) where
    displayException InvalidJSON{ path, value, type_} =
        "Invalid JSON\n\
        \\n\
        \The following JSON value:\n\
        \\n\
        \" <> string <> "\n\
        \\n\
        \… does not match the following expected type:\n\
        \\n\
        \" <> Text.unpack (Pretty.toSmart type_) <> "\n\
        \\n\
        \… at the following location:\n\
        \\n\
        \" <> Text.unpack (Text.intercalate "." (reverse path))
      where
        bytes = Aeson.Pretty.encodePretty value

        string = case Encoding.decodeUtf8' (ByteString.Lazy.toStrict bytes) of
            Left  _    -> show bytes
            Right text -> Text.unpack text

-- | Determines whether the `Value` has an effect
effects :: Getting Any Value ()
effects = Lens.cosmos . _As @"Lambda" . the @3 . Syntax.effects
