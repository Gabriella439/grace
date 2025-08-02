{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

{-| Use this module to decode Grace expressions into Haskell expressions

    Example usage:

    >>> decode decoder (Value.Scalar (Syntax.Bool True)) :: Either DecodingError Bool
    Right True
-}
module Grace.Decode
    ( -- * Classes
      FromGrace(..)
    , GenericFromGrace(..)
    , Decoder(..)

      -- * Exceptions
    , DecodingError(..)
    ) where

import Control.Exception (Exception)
import Control.Monad.State (State)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Void (Void)
import Data.Word (Word8, Word16, Word32, Word64)
import Grace.Marshal (Key(..), selector)
import Grace.Type (Type(..))
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

import qualified Control.Exception as Exception
import qualified Control.Monad.State as State
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.Scientific as Scientific
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Vector as Vector
import qualified GHC.Generics as Generics
import qualified Grace.Monotype as Monotype
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type
import qualified Grace.Value as Value

-- | A `Decoder` represents the decoding logic for a value alongside its type
data Decoder a = Decoder
    { decode :: Value -> Either DecodingError a
    , expected :: Type ()
    } deriving stock (Functor)

-- | Result of decoding
data DecodingError
    = TypeError
    -- ^ The input Grace expression has the wrong type
    | RangeError
    -- ^ The input Grace expression is out of bounds for the target Haskell type
    deriving stock (Show)

instance Exception DecodingError where
    displayException TypeError =
        "Failed to decode value because the type did not match"
    displayException RangeError =
        "Failed to decode value because the decoded result was out of bounds"

-- | Convert a Grace expression to a Haskell expression
class FromGrace a where
    decoder :: Decoder a

    default decoder :: (Generic a, GenericFromGrace (Rep a)) => Decoder a
    decoder = fmap to (State.evalState genericDecoder 0)

    -- | This is used for decoding record fields, which might not be present
    decodeMaybe :: Maybe Value -> Either DecodingError a
    decodeMaybe (Just value) = decode decoder value
    decodeMaybe Nothing = Left TypeError

instance FromGrace Void
instance FromGrace ()
instance (FromGrace a, FromGrace b) => FromGrace (a, b)
instance (FromGrace a, FromGrace b) => FromGrace (Either a b)

instance FromGrace Bool where
    decoder = Decoder{ decode, expected }
      where
        decode (Value.Scalar (Syntax.Bool bool)) = return bool
        decode _ = Left TypeError

        expected = Type.Scalar{ location = (), scalar = Monotype.Bool }

instance FromGrace Natural where
    decoder = Decoder{ decode, expected }
      where
        decode (Value.Scalar (Syntax.Natural natural)) = return natural
        decode _ = Left TypeError

        expected = Type.Scalar{ location = (), scalar = Monotype.Natural }

decodeIntegral
    ::  forall a b
    .   (FromGrace a, Integral a, Integral b, Bounded b) -- (Integral integral, Bounded num, Num num)
    =>  Decoder b
decodeIntegral = Decoder{ decode, expected }
  where
    Decoder{ decode = decode₀, expected } = decoder @a

    decode value = do
        integral <- decode₀ value

        if fromIntegral (minBound @b) <= integral && integral <= fromIntegral (maxBound @b)
            then return (fromIntegral @a @b integral)
            else Left RangeError

instance FromGrace Word where
    decoder = decodeIntegral @Natural @Word

instance FromGrace Word8 where
    decoder = decodeIntegral @Natural @Word8

instance FromGrace Word16 where
    decoder = decodeIntegral @Natural @Word16

instance FromGrace Word32 where
    decoder = decodeIntegral @Natural @Word32

instance FromGrace Word64 where
    decoder = decodeIntegral @Natural @Word64

instance FromGrace Integer where
    decoder = Decoder{ decode, expected }
      where
        decode (Value.Scalar (Syntax.Integer integer)) = return integer
        decode _ = Left TypeError

        expected = Type.Scalar{ location = (), scalar = Monotype.Integer }

instance FromGrace Int where
    decoder = decodeIntegral @Integer @Int

instance FromGrace Int8 where
    decoder = decodeIntegral @Integer @Int8

instance FromGrace Int16 where
    decoder = decodeIntegral @Integer @Int16

instance FromGrace Int32 where
    decoder = decodeIntegral @Integer @Int32

instance FromGrace Int64 where
    decoder = decodeIntegral @Integer @Int64

instance FromGrace Scientific where
    decoder = Decoder{ decode, expected }
      where
        decode (Value.Scalar (Syntax.Real scientific)) = return scientific
        decode _ = Left TypeError

        expected = Type.Scalar{ location = (), scalar = Monotype.Real }

decodeRealFloat :: RealFloat a => Decoder a
decodeRealFloat = Decoder{ decode, expected }
  where
    Decoder{ decode = decode₀, expected } = decoder

    decode value = do
        scientific <- decode₀ value

        case Scientific.toBoundedRealFloat scientific of
            Left _ -> Left RangeError
            Right realFloat -> return realFloat

instance FromGrace Double where
    decoder = decodeRealFloat

instance FromGrace Float where
    decoder = decodeRealFloat

instance FromGrace Text where
    decoder = Decoder{ decode, expected }
      where
        decode (Value.Text text) = return text
        decode _ = Left TypeError

        expected = Type.Scalar{ location = (), scalar = Monotype.Text }

instance FromGrace Text.Lazy.Text where
    decoder = fmap Text.Lazy.fromStrict decoder

instance {-# OVERLAPPING #-} FromGrace [Char] where
    decoder = fmap Text.unpack decoder

instance FromGrace Key where
    decoder = Decoder{ decode, expected }
      where
        decode (Value.Scalar (Syntax.Key text)) = return Key{ text }
        decode _ = Left TypeError

        expected = Type.Scalar{ location = (), scalar = Monotype.Key }

instance FromGrace Aeson.Value where
    decoder = Decoder{ decode, expected }
      where
        decode value = case Value.toJSON value of
            Nothing   -> Left TypeError
            Just json -> return json

        expected = Type.Scalar{ location = (), scalar = Monotype.JSON }

instance FromGrace a => FromGrace (Seq a) where
    decoder = Decoder{ decode, expected }
      where
        Decoder{ decode = decode₀, expected = expected₀ } = decoder

        decode (Value.List seq_) = traverse decode₀ seq_
        decode _ = Left TypeError

        expected = Type.List{ location = (), type_ = expected₀ }

instance FromGrace a => FromGrace [a] where
    decoder = fmap Foldable.toList (decoder @(Seq a))

instance FromGrace a => FromGrace (Vector a) where
    decoder = fmap Vector.fromList decoder

instance FromGrace a => FromGrace (Maybe a) where
    decoder = Decoder{ decode, expected }
      where
        Decoder{ decode = decode₀, expected = expected₀ } = decoder

        decode (Value.Scalar Syntax.Null) = do
            return Nothing
        decode (Value.Application (Value.Builtin Syntax.Some) value) = do
            a <- decode₀ value
            return (Just a)
        decode _ = do
            Left TypeError

        expected = Type.Optional{ location = (), type_ = expected₀ }

    decodeMaybe Nothing = do
        return Nothing
    decodeMaybe (Just value) = do
        decode decoder value

{-| This is the underlying class that powers the `FromGrace` class's support for
    automatically deriving a `Generic` implementation
-}
class GenericFromGrace f where
    genericDecoder :: State Int (Decoder (f a))

instance GenericFromGrace V1 where
    genericDecoder = do
        let decode _ = error "Grace.Decode.genericDecoder: <> inhabited"

        let expected = Type.Union
                { location = ()
                , alternatives = Type.Alternatives [] Monotype.EmptyAlternatives
                }

        return Decoder{ decode, expected }

instance GenericFromGrace U1 where
    genericDecoder = do
        let decode (Value.Record []) = return U1
            decode _ = Left TypeError

        let expected = Type.Record
                { location = ()
                , fields = Type.Fields [] Monotype.EmptyFields
                }

        return Decoder{ decode, expected }

instance GenericFromGrace f => GenericFromGrace (M1 D d f) where
    genericDecoder = fmap (fmap M1) genericDecoder

instance GenericFromGrace f => GenericFromGrace (M1 C d f) where
    genericDecoder = fmap (fmap M1) genericDecoder

instance (Selector s, FromGrace a) => GenericFromGrace (M1 S s (K1 i a)) where
    genericDecoder = do
        let m1 :: M1 S s (K1 i a) r
            m1 = undefined

        name <- selector m1

        let Decoder{ decode = decode₀, expected = expected₀ } = decoder

        let decode value
                | Generics.selName m1 == "" =
                    fmap (M1 . K1) (decode₀ value)
                | otherwise = case value of
                    Value.Record fieldValues ->
                        fmap (M1 . K1) (decodeMaybe (HashMap.lookup name fieldValues))
                    _ ->
                        Left TypeError

        let expected
                | Generics.selName m1  == "" =
                    expected₀
                | otherwise =
                    Type.Record
                        { location = ()
                        , fields =
                            Type.Fields
                                [ (name, expected₀) ]
                                Monotype.EmptyFields
                        }

        return Decoder{ decode, expected }

instance (Selector s₀, Selector s₁, FromGrace a₀, FromGrace a₁) => GenericFromGrace (M1 S s₀ (K1 i₀ a₀) :*: M1 S s₁ (K1 i₁ a₁)) where
    genericDecoder = do
        name₀ <- selector (undefined :: M1 S s₀ (K1 i₀ a₀) r)
        name₁ <- selector (undefined :: M1 S s₁ (K1 i₁ a₁) r)

        let Decoder{ expected = expected₀ } = decoder @a₀
        let Decoder{ expected = expected₁ } = decoder @a₁

        let decode (Value.Record fieldValues) = do
                expression₀ <- decodeMaybe (HashMap.lookup name₀ fieldValues)
                expression₁ <- decodeMaybe (HashMap.lookup name₁ fieldValues)

                return (M1 (K1 expression₀) :*: M1 (K1 expression₁))
            decode _ = Left TypeError

        let expected = Type.Record
                { location = ()
                , fields = Type.Fields
                    [ (name₀, expected₀)
                    , (name₁, expected₁)
                    ]
                    Monotype.EmptyFields
                }

        return Decoder{ decode, expected }

instance (Selector s, GenericFromGrace (f₀ :*: f₁), FromGrace a) => GenericFromGrace ((f₀ :*: f₁) :*: M1 S s (K1 i a)) where
    genericDecoder = do
        Decoder{ decode = decode₀, expected = expected₀ } <- genericDecoder

        name <- selector (undefined :: M1 S s (K1 i a) r)

        let Decoder{ expected = expected₁ } = decoder @a

        let decode value₀@(Value.Record fieldValues) = do
                expression₀ <- decode₀ value₀
                expression₁ <- decodeMaybe (HashMap.lookup name fieldValues)

                return (expression₀ :*: M1 (K1 expression₁))

            decode _ = Left TypeError

        let expected = Type.Record
                { location = ()
                , fields = Type.Fields
                    ((name, expected₀) : unsafeExpectRecordType expected₁)
                    Monotype.EmptyFields
                }

        return Decoder{ decode, expected }

instance (Selector s, FromGrace a, GenericFromGrace (f₀ :*: f₁)) => GenericFromGrace (M1 S s (K1 i a) :*: (f₀ :*: f₁)) where
    genericDecoder = do
        name <- selector (undefined :: M1 S s (K1 i a) r)

        let Decoder{ expected = expected₀ } = decoder @a

        Decoder{ decode = decode₁, expected = expected₁ } <- genericDecoder

        let decode value₁@(Value.Record fieldValues) = do
                expression₀ <- decodeMaybe (HashMap.lookup name fieldValues)
                expression₁ <- decode₁ value₁

                return (M1 (K1 expression₀) :*: expression₁)

            decode _ = Left TypeError

        let expected = Type.Record
                { location = ()
                , fields = Type.Fields
                    ((name, expected₀) : unsafeExpectRecordType expected₁)
                    Monotype.EmptyFields
                }

        return Decoder{ decode, expected }

instance (GenericFromGrace (f₀ :*: f₁), GenericFromGrace (f₂ :*: f₃)) => GenericFromGrace ((f₀ :*: f₁) :*: (f₂ :*: f₃)) where
    genericDecoder = do
        Decoder{ decode = decode₀, expected = expected₀ } <- genericDecoder
        Decoder{ decode = decode₁, expected = expected₁ } <- genericDecoder

        let decode value = do
                expression₀ <- decode₀ value
                expression₁ <- decode₁ value

                return (expression₀ :*: expression₁)

        let expected = Type.Record
                { location = ()
                , fields = Type.Fields
                    (   unsafeExpectRecordType expected₀
                    <>  unsafeExpectRecordType expected₁
                    )
                    Monotype.EmptyFields
                }

        return Decoder{ decode, expected }

instance (Constructor c₀, Constructor c₁, GenericFromGrace f₀, GenericFromGrace f₁) => GenericFromGrace (M1 C c₀ f₀ :+: M1 C c₁ f₁) where
    genericDecoder = do
        let name₀ = Text.pack (Generics.conName (undefined :: M1 C c₀ f₀ r))
        let name₁ = Text.pack (Generics.conName (undefined :: M1 C c₁ f₁ r))

        let Decoder{ decode = decode₀, expected = expected₀ } =
                State.evalState genericDecoder 0
        let Decoder{ decode = decode₁, expected = expected₁ } =
                State.evalState genericDecoder 0

        let decode (Value.Application (Value.Alternative name) value)
                | name == name₀ = fmap (L1 . M1) (decode₀ value)
                | name == name₁ = fmap (R1 . M1) (decode₁ value)
                | otherwise = Left TypeError
            decode _ = Left TypeError

        let expected = Type.Union
                { location = ()
                , alternatives = Type.Alternatives
                    [ (name₀, expected₀), (name₁, expected₁) ]
                    Monotype.EmptyAlternatives
                }

        return Decoder{ decode, expected }

instance (Constructor c, GenericFromGrace f₀, GenericFromGrace (f₁ :+: f₂)) => GenericFromGrace (M1 C c f₀ :+: (f₁ :+: f₂)) where
    genericDecoder = do
        let name₀ = Text.pack (Generics.conName (undefined :: M1 C c f r))

        let Decoder{ decode = decode₀, expected = expected₀ } =
                State.evalState genericDecoder 0
        let Decoder{ decode = decode₁, expected = expected₁ } =
                State.evalState genericDecoder 0

        let decode (Value.Application (Value.Alternative name) value₀)
                | name == name₀ = fmap (L1 . M1) (decode₀ value₀)
            decode value₁ = fmap R1 (decode₁ value₁)

        let expected = Type.Union
                { location = ()
                , alternatives = Type.Alternatives
                    ((name₀, expected₀) : unsafeExpectUnionType expected₁)
                    Monotype.EmptyAlternatives
                }

        return Decoder{ decode, expected }

instance (Constructor c, GenericFromGrace (f₀ :+: f₁), GenericFromGrace f₂) => GenericFromGrace ((f₀ :+: f₁) :+: M1 C c f₂) where
    genericDecoder = do
        let name₁ = Text.pack (Generics.conName (undefined :: M1 C c f r))

        let Decoder{ decode = decode₀, expected = expected₀ } =
                State.evalState genericDecoder 0
        let Decoder{ decode = decode₁, expected = expected₁ } =
                State.evalState genericDecoder 0

        let decode (Value.Application (Value.Alternative name) value₁)
                | name == name₁ = fmap (R1 . M1) (decode₁ value₁)
            decode value₀ = fmap L1 (decode₀ value₀)

        let expected = Type.Union
                { location = ()
                , alternatives = Type.Alternatives
                    (unsafeExpectUnionType expected₀ <> [ (name₁, expected₁) ])
                    Monotype.EmptyAlternatives
                }

        return Decoder{ decode, expected }


instance (GenericFromGrace (f₀ :+: f₁), GenericFromGrace (f₂ :+: f₃)) => GenericFromGrace ((f₀ :+: f₁) :+: (f₂ :+: f₃)) where
    genericDecoder = do
        let Decoder{ decode = decode₀, expected = expected₀ } =
                State.evalState genericDecoder 0
        let Decoder{ decode = decode₁, expected = expected₁ } =
                State.evalState genericDecoder 0

        let Right a <|> _ = Right a
            _ <|> Right a = Right a
            Left TypeError <|> Left TypeError = Left TypeError
            _ <|> _ = Left RangeError

        let decode value = decode₀ value <|> decode₁ value

        let expected = Type.Union
                { location = ()
                , alternatives = Type.Alternatives
                    (unsafeExpectUnionType expected₀ <> unsafeExpectUnionType expected₁)
                    Monotype.EmptyAlternatives
                }

        return Decoder{ decode, expected }

unsafeExpectRecordType :: Type s -> [(Text, Type s)]
unsafeExpectRecordType Type.Record{ fields = Type.Fields fieldTypes _ } =
    fieldTypes
unsafeExpectRecordType _ =
    error "Grace.Decode.unsafeExpectRecordType: not a record"

unsafeExpectUnionType :: Type s -> [(Text, Type s)]
unsafeExpectUnionType Type.Union{ alternatives = Type.Alternatives alternativeTypes _ } =
    alternativeTypes
unsafeExpectUnionType _ =
    error "Grace.Decode.unsafeExpectUnionType: not a union"
