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

    >>> decode (Value.Scalar (Syntax.Bool True)) :: Either DecodingError Bool
    Right True
-}
module Grace.Decode
    ( -- * Classes
      FromGrace(..)
    , GenericFromGrace(..)

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
    decode :: Value -> Either DecodingError a

    default decode
        :: (Generic a, GenericFromGrace (Rep a))
        => Value -> Either DecodingError a
    decode = fmap (fmap to) (State.evalState genericDecode 0)

    expected :: Type ()
    default expected :: (Generic a, GenericFromGrace (Rep a)) => Type ()
    expected = State.evalState (genericExpected @(Rep a)) 0

    -- | This is used for decoding record fields, which might not be present
    decodeMaybe :: Maybe Value -> Either DecodingError a
    decodeMaybe (Just value) = decode value
    decodeMaybe Nothing = Left TypeError

instance FromGrace Void
instance FromGrace ()
instance (FromGrace a, FromGrace b) => FromGrace (a, b)
instance (FromGrace a, FromGrace b) => FromGrace (Either a b)

instance FromGrace Bool where
    decode (Value.Scalar (Syntax.Bool bool)) = return bool
    decode _ = Left TypeError

    expected = Type.Scalar{ location = (), scalar = Monotype.Bool }

instance FromGrace Natural where
    decode (Value.Scalar (Syntax.Natural natural)) = return natural
    decode _ = Left TypeError

    expected = Type.Scalar{ location = (), scalar = Monotype.Natural }

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

    expected = expected @Natural

instance FromGrace Word8 where
    decode = decodeIntegral @Natural @Word8

    expected = expected @Natural

instance FromGrace Word16 where
    decode = decodeIntegral @Natural @Word16

    expected = expected @Natural

instance FromGrace Word32 where
    decode = decodeIntegral @Natural @Word32

    expected = expected @Natural

instance FromGrace Word64 where
    decode = decodeIntegral @Natural @Word64

    expected = expected @Natural

instance FromGrace Integer where
    decode (Value.Scalar (Syntax.Integer integer)) = return integer
    decode _ = Left TypeError

    expected = Type.Scalar{ location = (), scalar = Monotype.Integer }

instance FromGrace Int where
    decode = decodeIntegral @Integer @Int

    expected = expected @Integer

instance FromGrace Int8 where
    decode = decodeIntegral @Integer @Int8

    expected = expected @Integer

instance FromGrace Int16 where
    decode = decodeIntegral @Integer @Int16

    expected = expected @Integer

instance FromGrace Int32 where
    decode = decodeIntegral @Integer @Int32

    expected = expected @Integer

instance FromGrace Int64 where
    decode = decodeIntegral @Integer @Int64

    expected = expected @Integer

instance FromGrace Scientific where
    decode (Value.Scalar (Syntax.Real scientific)) = return scientific
    decode _ = Left TypeError

    expected = Type.Scalar{ location = (), scalar = Monotype.Real }

decodeRealFloat :: RealFloat a => Value -> Either DecodingError a
decodeRealFloat value = do
    scientific <- decode value

    case Scientific.toBoundedRealFloat scientific of
        Left _ -> Left RangeError
        Right realFloat -> return realFloat

instance FromGrace Double where
    decode = decodeRealFloat

    expected = expected @Scientific

instance FromGrace Float where
    decode = decodeRealFloat

    expected = expected @Scientific

instance FromGrace Text where
    decode (Value.Text text) = return text
    decode _ = Left TypeError

    expected = Type.Scalar{ location = (), scalar = Monotype.Text }

instance FromGrace Text.Lazy.Text where
    decode = fmap (fmap Text.Lazy.fromStrict) decode

    expected = expected @Text

instance {-# OVERLAPPING #-} FromGrace [Char] where
    decode = fmap (fmap Text.unpack) decode

    expected = expected @Text

instance FromGrace Key where
    decode (Value.Scalar (Syntax.Key text)) = return Key{ text }
    decode _ = Left TypeError

    expected = Type.Scalar{ location = (), scalar = Monotype.Key }

instance FromGrace Aeson.Value where
    decode value = case Value.toJSON value of
        Nothing   -> Left TypeError
        Just json -> return json

    expected = Type.Scalar{ location = (), scalar = Monotype.JSON }

instance FromGrace a => FromGrace (Seq a) where
    decode (Value.List seq_) = traverse decode seq_
    decode _ = Left TypeError

    expected = Type.List{ location = (), type_ = expected @a }

instance FromGrace a => FromGrace [a] where
    decode = fmap (fmap Foldable.toList) (decode @(Seq a))

    expected = expected @(Seq a)

instance FromGrace a => FromGrace (Vector a) where
    decode = fmap (fmap Vector.fromList) decode

    expected = expected @[a]

instance FromGrace a => FromGrace (Maybe a) where
    decode (Value.Scalar Syntax.Null) = do
        return Nothing
    decode (Value.Application (Value.Builtin Syntax.Some) value) = do
        a <- decode value
        return (Just a)
    decode _ = do
        Left TypeError

    expected = Type.Optional{ location = (), type_ = expected @a }

    decodeMaybe Nothing = do
        return Nothing
    decodeMaybe (Just value) = do
        decode value

{-| This is the underlying class that powers the `FromGrace` class's support for
    automatically deriving a `Generic` implementation
-}
class GenericFromGrace f where
    genericDecode :: State Int (Value -> Either DecodingError (f a))

    genericExpected :: State Int (Type ())

instance GenericFromGrace V1 where
    genericDecode = do
        let decode_ _ = Left TypeError

        return decode_

    genericExpected = do
        return Type.Union
            { location = ()
            , alternatives = Type.Alternatives [] Monotype.EmptyAlternatives
            }

instance GenericFromGrace U1 where
    genericDecode = do
        let decode_ (Value.Record []) = return U1
            decode_ _ = Left TypeError

        return decode_

    genericExpected = do
        return Type.Record
            { location = ()
            , fields = Type.Fields [] Monotype.EmptyFields
            }

instance GenericFromGrace f => GenericFromGrace (M1 D d f) where
    genericDecode = fmap (fmap (fmap M1)) genericDecode

    genericExpected = genericExpected @f

instance GenericFromGrace f => GenericFromGrace (M1 C d f) where
    genericDecode = fmap (fmap (fmap M1)) genericDecode

    genericExpected = genericExpected @f

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
                    _ ->
                        Left TypeError

        return decode_

    genericExpected = do
        let m1 :: M1 S s (K1 i a) r
            m1 = undefined

        name <- selector m1

        if Generics.selName m1 == ""
            then do
                return (expected @a)
            else do
                return Type.Record
                    { location = ()
                    , fields =
                        Type.Fields [ (name, expected @a) ] Monotype.EmptyFields
                    }

instance (Selector s₀, Selector s₁, FromGrace a₀, FromGrace a₁) => GenericFromGrace (M1 S s₀ (K1 i₀ a₀) :*: M1 S s₁ (K1 i₁ a₁)) where
    genericDecode = do
        name₀ <- selector (undefined :: M1 S s₀ (K1 i₀ a₀) r)
        name₁ <- selector (undefined :: M1 S s₁ (K1 i₁ a₁) r)

        let decode_ (Value.Record fieldValues) = do
                expression₀ <- decodeMaybe (HashMap.lookup name₀ fieldValues)
                expression₁ <- decodeMaybe (HashMap.lookup name₁ fieldValues)

                return (M1 (K1 expression₀) :*: M1 (K1 expression₁))
            decode_ _ = Left TypeError

        return decode_

    genericExpected = do
        name₀ <- selector (undefined :: M1 S s₀ (K1 i₀ a₀) r)
        name₁ <- selector (undefined :: M1 S s₁ (K1 i₁ a₁) r)

        return Type.Record
            { location = ()
            , fields = Type.Fields
                [ (name₀, expected @a₀)
                , (name₁, expected @a₁)
                ]
                Monotype.EmptyFields
            }

instance (Selector s, GenericFromGrace (f₀ :*: f₁), FromGrace a) => GenericFromGrace ((f₀ :*: f₁) :*: M1 S s (K1 i a)) where
    genericDecode = do
        decode₀ <- genericDecode

        name <- selector (undefined :: M1 S s (K1 i a) r)

        let decode_ value₀@(Value.Record fieldValues) = do
                expression₀ <- decode₀ value₀
                expression₁ <- decodeMaybe (HashMap.lookup name fieldValues)

                return (expression₀ :*: M1 (K1 expression₁))

            decode_ _ = Left TypeError

        return decode_

    genericExpected = do
        expected₀ <- genericExpected @(f₀ :*: f₁)

        name <- selector (undefined :: M1 S s (K1 i a) r)

        return Type.Record
            { location = ()
            , fields = Type.Fields
                ((name, expected @a) : unsafeExpectRecordType expected₀)
                Monotype.EmptyFields
            }

instance (Selector s, FromGrace a, GenericFromGrace (f₀ :*: f₁)) => GenericFromGrace (M1 S s (K1 i a) :*: (f₀ :*: f₁)) where
    genericDecode = do
        name <- selector (undefined :: M1 S s (K1 i a) r)

        decode₁ <- genericDecode

        let decode_ value₁@(Value.Record fieldValues) = do
                expression₀ <- decodeMaybe (HashMap.lookup name fieldValues)
                expression₁ <- decode₁ value₁

                return (M1 (K1 expression₀) :*: expression₁)

            decode_ _ = Left TypeError

        return decode_

    genericExpected = do
        name <- selector (undefined :: M1 S s (K1 i a) r)

        expected₁ <- genericExpected @(f₀ :*: f₁)

        return Type.Record
            { location = ()
            , fields = Type.Fields
                ((name, expected @a) : unsafeExpectRecordType expected₁)
                Monotype.EmptyFields
            }

instance (GenericFromGrace (f₀ :*: f₁), GenericFromGrace (f₂ :*: f₃)) => GenericFromGrace ((f₀ :*: f₁) :*: (f₂ :*: f₃)) where
    genericDecode = do
        decode₀ <- genericDecode
        decode₁ <- genericDecode

        let decode_ value = do
                expression₀ <- decode₀ value
                expression₁ <- decode₁ value

                return (expression₀ :*: expression₁)

        return decode_

    genericExpected = do
        expected₀ <- genericExpected @(f₀ :*: f₁)
        expected₁ <- genericExpected @(f₂ :*: f₃)

        return Type.Record
            { location = ()
            , fields = Type.Fields
                (   unsafeExpectRecordType expected₀
                <>  unsafeExpectRecordType expected₁
                )
                Monotype.EmptyFields
            }

instance (Constructor c₀, Constructor c₁, GenericFromGrace f₀, GenericFromGrace f₁) => GenericFromGrace (M1 C c₀ f₀ :+: M1 C c₁ f₁) where
    genericDecode = do
        let name₀ = Text.pack (Generics.conName (undefined :: M1 C c₀ f₀ r))
        let name₁ = Text.pack (Generics.conName (undefined :: M1 C c₁ f₁ r))

        let decode₀ = State.evalState genericDecode 0
        let decode₁ = State.evalState genericDecode 0

        let decode_ (Value.Application (Value.Alternative name) value)
                | name == name₀ = fmap (L1 . M1) (decode₀ value)
                | name == name₁ = fmap (R1 . M1) (decode₁ value)
                | otherwise = Left TypeError
            decode_ _ = Left TypeError

        return decode_

    genericExpected = do
        let name₀ = Text.pack (Generics.conName (undefined :: M1 C c₀ f₀ r))
        let name₁ = Text.pack (Generics.conName (undefined :: M1 C c₁ f₁ r))

        let expected₀ = State.evalState (genericExpected @f₀) 0
        let expected₁ = State.evalState (genericExpected @f₁) 0

        return Type.Union
            { location = ()
            , alternatives = Type.Alternatives
                [ (name₀, expected₀), (name₁, expected₁) ]
                Monotype.EmptyAlternatives
            }

instance (Constructor c, GenericFromGrace f₀, GenericFromGrace (f₁ :+: f₂)) => GenericFromGrace (M1 C c f₀ :+: (f₁ :+: f₂)) where
    genericDecode = do
        let name₀ = Text.pack (Generics.conName (undefined :: M1 C c f r))

        let decode₀ = State.evalState genericDecode 0
        let decode₁ = State.evalState genericDecode 0

        let decode_ (Value.Application (Value.Alternative name) value₀)
                | name == name₀ = fmap (L1 . M1) (decode₀ value₀)
            decode_ value₁ = fmap R1 (decode₁ value₁)

        return decode_

    genericExpected = do
        let name₀ = Text.pack (Generics.conName (undefined :: M1 C c f r))

        let expected₀ = State.evalState (genericExpected @f₀         ) 0
        let expected₁ = State.evalState (genericExpected @(f₁ :+: f₂)) 0

        return Type.Union
            { location = ()
            , alternatives = Type.Alternatives
                ((name₀, expected₀) : unsafeExpectUnionType expected₁)
                Monotype.EmptyAlternatives
            }

instance (Constructor c, GenericFromGrace (f₀ :+: f₁), GenericFromGrace f₂) => GenericFromGrace ((f₀ :+: f₁) :+: M1 C c f₂) where
    genericDecode = do
        let name₁ = Text.pack (Generics.conName (undefined :: M1 C c f r))

        let decode₀ = State.evalState genericDecode 0
        let decode₁ = State.evalState genericDecode 0

        let decode_ (Value.Application (Value.Alternative name) value₁)
                | name == name₁ = fmap (R1 . M1) (decode₁ value₁)
            decode_ value₀ = fmap L1 (decode₀ value₀)

        return decode_

    genericExpected = do
        let name₁ = Text.pack (Generics.conName (undefined :: M1 C c f r))

        let expected₀ = State.evalState (genericExpected @(f₀ :+: f₁)) 0
        let expected₁ = State.evalState (genericExpected @f₂) 0

        return Type.Union
            { location = ()
            , alternatives = Type.Alternatives
                (unsafeExpectUnionType expected₀ <> [ (name₁, expected₁) ])
                Monotype.EmptyAlternatives
            }

instance (GenericFromGrace (f₀ :+: f₁), GenericFromGrace (f₂ :+: f₃)) => GenericFromGrace ((f₀ :+: f₁) :+: (f₂ :+: f₃)) where
    genericDecode = do
        let decode₀ = State.evalState genericDecode 0
        let decode₁ = State.evalState genericDecode 0

        let Right a <|> _ = Right a
            _ <|> Right a = Right a
            Left TypeError <|> Left TypeError = Left TypeError
            _ <|> _ = Left RangeError

        let decode_ value = decode₀ value <|> decode₁ value

        return decode_

    genericExpected = do
        let expected₀ = State.evalState (genericExpected @(f₀ :+: f₁)) 0
        let expected₁ = State.evalState (genericExpected @(f₂ :+: f₃)) 0

        return Type.Union
            { location = ()
            , alternatives = Type.Alternatives
                (unsafeExpectUnionType expected₀ <> unsafeExpectUnionType expected₁)
                Monotype.EmptyAlternatives
            }

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
