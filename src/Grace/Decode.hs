{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists     #-}

{-| Use this module to decode Grace expressions into Haskell expressions

    Example usage:

    >>> decode (Value.Scalar (Syntax.Bool True)) :: Either DecodingError Bool
    Right True
-}
module Grace.Decode
    ( -- * Classes
      FromGrace(..)
    , ToGraceType(..)
    , GenericFromGrace(..)
    , GenericToGraceType(..)

      -- * Types
    , Key(..)

      -- * Exceptions
    , DecodingError(..)
    ) where

import Control.Exception.Safe (Exception)
import Control.Monad.State (State)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Void (Void)
import Data.Word (Word8, Word16, Word32, Word64)
import Grace.Type (Type)
import Grace.Value (Value(..))
import Numeric.Natural (Natural)

import qualified Data.Text as Text

import GHC.Generics
    ( Generic(..)
    , C
    , Constructor
    , D
    , K1(..)
    , M1(..)
    , Rep
    , S
    , Selector
    , U1(..)
    , V1
    , (:+:)(..)
    , (:*:)(..)
    )
import Grace.Marshal
    (Key(..), GenericToGraceType(..), ToGraceType(..), selector)

import qualified Control.Exception.Safe as Exception
import qualified Control.Monad.State as State
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.Scientific as Scientific
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Vector as Vector
import qualified GHC.Generics as Generics
import qualified Grace.Pretty as Grace.Pretty
import qualified Grace.Syntax as Syntax
import qualified Grace.Value as Value
import qualified Prettyprinter as Pretty

-- | Convert a Grace expression to a Haskell expression
class ToGraceType a => FromGrace a where
    decode :: Value -> Either DecodingError a
    default decode
        :: (Generic a, GenericFromGrace (Rep a))
        => Value -> Either DecodingError a
    decode = fmap (fmap to) (State.evalState genericDecode 0)

    -- | This is used for decoding record fields, which might not be present
    decodeMaybe :: Maybe Value -> Either DecodingError a
    decodeMaybe (Just value) = decode value
    decodeMaybe Nothing = Left TypeError
        { expectedType = expected @(Maybe a)
        , value = Value.Scalar Syntax.Null
        }

instance FromGrace Void
instance FromGrace ()
instance (FromGrace a, FromGrace b) => FromGrace (a, b)
instance (FromGrace a, FromGrace b) => FromGrace (Either a b)

instance FromGrace Bool where
    decode (Value.Scalar (Syntax.Bool bool)) = return bool
    decode value = Left TypeError{ expectedType = expected @Bool, value }

instance FromGrace Natural where
    decode (Value.Scalar (Syntax.Natural natural)) =
        return natural
    decode value =
        Left TypeError{ expectedType = expected @Natural, value }

decodeIntegral
    ::  forall a b
    .   (FromGrace a, Integral a, Integral b, Bounded b)
    =>  Value -> Either DecodingError b
decodeIntegral value = do
    integral <- decode @a value

    if      fromIntegral (minBound @b) <= integral
        &&  integral <= fromIntegral (maxBound @b)
        then return (fromIntegral @a @b integral)
        else Left RangeError

instance FromGrace Word where
    decode = decodeIntegral @Natural @Word

instance FromGrace Word8 where
    decode = decodeIntegral @Natural @Word8

instance FromGrace Word16 where
    decode = decodeIntegral @Natural @Word16

instance FromGrace Word32 where
    decode = decodeIntegral @Natural @Word32

instance FromGrace Word64 where
    decode = decodeIntegral @Natural @Word64

instance FromGrace Integer where
    decode (Value.Scalar (Syntax.Natural natural)) =
        return (fromIntegral natural)
    decode (Value.Scalar (Syntax.Integer integer)) =
        return integer
    decode value = Left TypeError
        { expectedType = expected @Integer
        , value = value
        }

instance FromGrace Int where
    decode = decodeIntegral @Integer @Int

instance FromGrace Int8 where
    decode = decodeIntegral @Integer @Int8

instance FromGrace Int16 where
    decode = decodeIntegral @Integer @Int16

instance FromGrace Int32 where
    decode = decodeIntegral @Integer @Int32

instance FromGrace Int64 where
    decode = decodeIntegral @Integer @Int64

instance FromGrace Scientific where
    decode (Value.Scalar (Syntax.Natural natural)) =
        return (fromIntegral natural)
    decode (Value.Scalar (Syntax.Integer integer)) =
        return (fromInteger integer)
    decode (Value.Scalar (Syntax.Real scientific)) =
        return scientific
    decode value =
        Left TypeError{ expectedType = expected @Scientific, value }

decodeRealFloat :: RealFloat a => Value -> Either DecodingError a
decodeRealFloat value = do
    scientific <- decode value

    case Scientific.toBoundedRealFloat scientific of
        Left _ -> Left RangeError
        Right realFloat -> return realFloat

instance FromGrace Double where
    decode = decodeRealFloat

instance FromGrace Float where
    decode = decodeRealFloat

instance FromGrace Text where
    decode (Value.Text text) = return text
    decode value = Left TypeError{ expectedType = expected @Text, value }

instance FromGrace Text.Lazy.Text where
    decode = fmap (fmap Text.Lazy.fromStrict) decode

instance {-# OVERLAPPING #-} FromGrace [Char] where
    decode = fmap (fmap Text.unpack) decode

instance FromGrace Key where
    decode (Value.Scalar (Syntax.Key text)) = return Key{ text }
    decode value = Left TypeError{ expectedType = expected @Key, value }

instance FromGrace Aeson.Value where
    decode value = case Value.toJSON value of
        Nothing ->
            Left TypeError{ expectedType = expected @Aeson.Value, value }
        Just json ->
            return json

instance FromGrace a => FromGrace (Seq a) where
    decode (Value.List seq_) = traverse decode seq_
    decode value = Left TypeError{ expectedType = expected @(Seq a), value }

instance FromGrace a => FromGrace [a] where
    decode = fmap (fmap Foldable.toList) (decode @(Seq a))

instance FromGrace a => FromGrace (Vector a) where
    decode = fmap (fmap Vector.fromList) decode

instance FromGrace a => FromGrace (Maybe a) where
    decode (Value.Scalar Syntax.Null) = do
        return Nothing
    decode (Value.Application (Value.Builtin Syntax.Some) value) = do
        a <- decode value
        return (Just a)
    decode value = do
        Left TypeError{ expectedType = expected @(Maybe a), value }

    decodeMaybe Nothing = do
        return Nothing
    decodeMaybe (Just value) = do
        decode value

{-| This is the underlying class that powers the `FromGrace` class's support for
    automatically deriving a `Generic` implementation
-}
class GenericToGraceType f => GenericFromGrace f where
    genericDecode :: State Int (Value -> Either DecodingError (f a))

instance GenericFromGrace V1 where
    genericDecode = do
        let expectedType = State.evalState (genericExpected @V1) 0

        let decode_ value = Left TypeError{ expectedType, value }

        return decode_

instance GenericFromGrace U1 where
    genericDecode = do
        let expectedType = State.evalState (genericExpected @U1) 0

        let decode_ (Value.Record []) = return U1
            decode_ value = Left TypeError{ expectedType, value }

        return decode_

instance GenericFromGrace f => GenericFromGrace (M1 D d f) where
    genericDecode = fmap (fmap (fmap M1)) genericDecode

instance GenericFromGrace f => GenericFromGrace (M1 C d f) where
    genericDecode = fmap (fmap (fmap M1)) genericDecode

instance (Selector s, FromGrace a) => GenericFromGrace (M1 S s (K1 i a)) where
    genericDecode = do
        let m1 :: M1 S s (K1 i a) r
            m1 = undefined

        name <- selector m1

        let decode_ value
                | Generics.selName m1 == "" =
                    fmap (M1 . K1) (decode value)
                | otherwise = case value of
                    Value.Record fieldValues ->
                        fmap (M1 . K1) (decodeMaybe (HashMap.lookup name fieldValues))
                    _ -> do
                        let expectedType =
                                State.evalState (genericExpected @(M1 S s (K1 i a))) 0

                        Left TypeError{ expectedType, value }

        return decode_

instance (Selector s₀, Selector s₁, FromGrace a₀, FromGrace a₁) => GenericFromGrace (M1 S s₀ (K1 i₀ a₀) :*: M1 S s₁ (K1 i₁ a₁)) where
    genericDecode = do
        name₀ <- selector (undefined :: M1 S s₀ (K1 i₀ a₀) r)
        name₁ <- selector (undefined :: M1 S s₁ (K1 i₁ a₁) r)

        let decode_ (Value.Record fieldValues) = do
                expression₀ <- decodeMaybe (HashMap.lookup name₀ fieldValues)
                expression₁ <- decodeMaybe (HashMap.lookup name₁ fieldValues)

                return (M1 (K1 expression₀) :*: M1 (K1 expression₁))
            decode_ value = do
                let expectedType =
                        State.evalState (genericExpected @(M1 S s₀ (K1 i₀ a₀) :*: M1 S s₁ (K1 i₁ a₁))) 0

                Left TypeError{ expectedType, value }

        return decode_

instance (Selector s, GenericFromGrace (f₀ :*: f₁), FromGrace a) => GenericFromGrace ((f₀ :*: f₁) :*: M1 S s (K1 i a)) where
    genericDecode = do
        decode₀ <- genericDecode

        name <- selector (undefined :: M1 S s (K1 i a) r)

        let decode_ value₀@(Value.Record fieldValues) = do
                expression₀ <- decode₀ value₀
                expression₁ <- decodeMaybe (HashMap.lookup name fieldValues)

                return (expression₀ :*: M1 (K1 expression₁))

            decode_ value = do
                let expectedType =
                        State.evalState (genericExpected @((f₀ :*: f₁) :*: M1 S s (K1 i a))) 0

                Left TypeError{ expectedType, value }

        return decode_

instance (Selector s, FromGrace a, GenericFromGrace (f₀ :*: f₁)) => GenericFromGrace (M1 S s (K1 i a) :*: (f₀ :*: f₁)) where
    genericDecode = do
        name <- selector (undefined :: M1 S s (K1 i a) r)

        decode₁ <- genericDecode

        let decode_ value₁@(Value.Record fieldValues) = do
                expression₀ <- decodeMaybe (HashMap.lookup name fieldValues)
                expression₁ <- decode₁ value₁

                return (M1 (K1 expression₀) :*: expression₁)

            decode_ value = do
                let expectedType =
                        State.evalState (genericExpected @(M1 S s (K1 i a) :*: (f₀ :*: f₁))) 0

                Left TypeError{ expectedType, value }

        return decode_

instance (GenericFromGrace (f₀ :*: f₁), GenericFromGrace (f₂ :*: f₃)) => GenericFromGrace ((f₀ :*: f₁) :*: (f₂ :*: f₃)) where
    genericDecode = do
        decode₀ <- genericDecode
        decode₁ <- genericDecode

        let decode_ value = do
                expression₀ <- decode₀ value
                expression₁ <- decode₁ value

                return (expression₀ :*: expression₁)

        return decode_

instance (Constructor c₀, Constructor c₁, GenericFromGrace f₀, GenericFromGrace f₁) => GenericFromGrace (M1 C c₀ f₀ :+: M1 C c₁ f₁) where
    genericDecode = do
        let name₀ = Text.pack (Generics.conName (undefined :: M1 C c₀ f₀ r))
        let name₁ = Text.pack (Generics.conName (undefined :: M1 C c₁ f₁ r))

        let decode₀ = State.evalState genericDecode 0
        let decode₁ = State.evalState genericDecode 0

        let decode_ v@(Value.Alternative name value)
                | name == name₀ = fmap (L1 . M1) (decode₀ value)
                | name == name₁ = fmap (R1 . M1) (decode₁ value)
                | otherwise = do
                    let expectedType =
                            State.evalState (genericExpected @(M1 C c₀ f₀ :+: M1 C c₁ f₁)) 0

                    Left TypeError{ expectedType, value = v }
            decode_ v = do
                let expectedType =
                            State.evalState (genericExpected @(M1 C c₀ f₀ :+: M1 C c₁ f₁)) 0

                Left TypeError{ expectedType, value = v }

        return decode_

instance (Constructor c, GenericFromGrace f₀, GenericFromGrace (f₁ :+: f₂)) => GenericFromGrace (M1 C c f₀ :+: (f₁ :+: f₂)) where
    genericDecode = do
        let name₀ = Text.pack (Generics.conName (undefined :: M1 C c f r))

        let decode₀ = State.evalState genericDecode 0
        let decode₁ = State.evalState genericDecode 0

        let decode_ (Value.Alternative name value₀)
                | name == name₀ = fmap (L1 . M1) (decode₀ value₀)
            decode_ value₁ = fmap R1 (decode₁ value₁)

        return decode_

instance (Constructor c, GenericFromGrace (f₀ :+: f₁), GenericFromGrace f₂) => GenericFromGrace ((f₀ :+: f₁) :+: M1 C c f₂) where
    genericDecode = do
        let name₁ = Text.pack (Generics.conName (undefined :: M1 C c f r))

        let decode₀ = State.evalState genericDecode 0
        let decode₁ = State.evalState genericDecode 0

        let decode_ (Value.Alternative name value₁)
                | name == name₁ = fmap (R1 . M1) (decode₁ value₁)
            decode_ value₀ = fmap L1 (decode₀ value₀)

        return decode_

instance (GenericFromGrace (f₀ :+: f₁), GenericFromGrace (f₂ :+: f₃)) => GenericFromGrace ((f₀ :+: f₁) :+: (f₂ :+: f₃)) where
    genericDecode = do
        let decode₀ = State.evalState genericDecode 0
        let decode₁ = State.evalState genericDecode 0

        let decode_ value = do
                let Right a <|> _ = Right a

                    _ <|> Right a = Right a

                    Left TypeError{ } <|> Left TypeError{ } = do
                        let expectedType =
                                State.evalState (genericExpected @((f₀ :+: f₁) :+: (f₂ :+: f₃))) 0

                        Left TypeError{ expectedType, value }

                    _ <|> _ = Left RangeError

                fmap L1 (decode₀ value) <|> fmap R1 (decode₁ value)

        return decode_

-- | Result of decoding
data DecodingError
    = TypeError{ expectedType :: Type (), value :: Value }
    -- ^ The input Grace expression has the wrong type
    | RangeError
    -- ^ The input Grace expression is out of bounds for the target Haskell type
    deriving stock (Eq, Show)

instance Exception DecodingError where
    displayException TypeError{ expectedType, value } =
        "Failed to decode Grace value into Haskell because of a type mismatch\n\
        \\n\
        \The following value:\n\
        \\n\
        \" <> Text.unpack (Grace.Pretty.toSmart ("  " <> Pretty.align (Grace.Pretty.pretty value))) <> "\n\
        \\n\
        \… could not be decoded into Haskell because it does not match this type:\n\
        \\n\
        \" <> Text.unpack (Grace.Pretty.toSmart ("  " <> Pretty.align (Grace.Pretty.pretty expectedType)))
    displayException RangeError =
        "Failed to decode value because the decoded result was out of bounds"
