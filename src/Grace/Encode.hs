{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-| Use this module to encode Haskell expressions as Grace expressions

    Example usage:

    >>> encode True
    Scalar (Bool True)
-}
module Grace.Encode
    ( -- * Classes
      ToGrace(..)
    , GenericToGrace(..)
    ) where

import Control.Monad.State (State)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Void (Void)
import Data.Word (Word8, Word16, Word32, Word64)
import Grace.Marshal (Key(..))
import Grace.Value (Value(..))
import Numeric.Natural (Natural)

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

import qualified Control.Monad.State as State
import qualified Data.Scientific as Scientific
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Vector as Vector
import qualified GHC.Generics as Generics
import qualified Grace.Marshal as Marshal
import qualified Grace.Syntax as Syntax
import qualified Grace.Value as Value

-- | Convert a Haskell expression to a Grace expression
class ToGrace a where
    encode :: a -> Value

    default encode :: (Generic a, GenericToGrace (Rep a)) => a -> Value
    encode = State.evalState genericEncode 0 . from

instance ToGrace Void
instance ToGrace ()
instance (ToGrace a, ToGrace b) => ToGrace (a, b)
instance (ToGrace a, ToGrace b) => ToGrace (Either a b)

instance ToGrace Bool where
    encode bool = Value.Scalar (Syntax.Bool bool)

instance ToGrace Natural where
    encode natural = Value.Scalar (Syntax.Natural natural)

instance ToGrace Word where
    encode = encode @Natural . fromIntegral

instance ToGrace Word8 where
    encode = encode @Natural . fromIntegral

instance ToGrace Word16 where
    encode = encode @Natural . fromIntegral

instance ToGrace Word32 where
    encode = encode @Natural . fromIntegral

instance ToGrace Word64 where
    encode = encode @Natural . fromIntegral

instance ToGrace Integer where
    encode integer = Value.Scalar (Syntax.Integer integer)

instance ToGrace Int where
    encode = encode @Integer . fromIntegral

instance ToGrace Int8 where
    encode = encode @Integer . fromIntegral

instance ToGrace Int16 where
    encode = encode @Integer . fromIntegral

instance ToGrace Int32 where
    encode = encode @Integer . fromIntegral

instance ToGrace Int64 where
    encode = encode @Integer . fromIntegral

instance ToGrace Scientific where
    encode scientific = Value.Scalar (Syntax.Real scientific)

instance ToGrace Float where
    encode = encode @Scientific . Scientific.fromFloatDigits

instance ToGrace Double where
    encode = encode @Scientific . Scientific.fromFloatDigits

instance ToGrace Text where
    encode text = Value.Text text

instance ToGrace Text.Lazy.Text where
    encode = encode . Text.Lazy.toStrict

instance {-# OVERLAPPING #-} ToGrace [Char] where
    encode = encode . Text.pack

instance ToGrace Key where
    encode Key{ text } = Value.Scalar (Syntax.Key text)

instance ToGrace a => ToGrace (Seq a) where
    encode list = Value.List (fmap encode list)

instance ToGrace a => ToGrace [a] where
    encode = encode . Seq.fromList

instance ToGrace a => ToGrace (Vector a) where
    encode = encode . Vector.toList

instance ToGrace a => ToGrace (Maybe a) where
    encode (Just a) = Value.Application (Value.Builtin Syntax.Some) (encode a)
    encode Nothing = Value.Scalar Syntax.Null

{-| This is the underlying class that powers the `ToGrace` class's support for
    automatically deriving a `Generic` implementation
-}
class GenericToGrace f where
    genericEncode :: State Int (f a -> Value)

instance GenericToGrace V1 where
    genericEncode = do
        -- EmptyCase does not work here and produces a non-exhaustive pattern
        -- match warning
        let encode_ _ = error "Grace.Encode.genericEncode: V1 inhabited"

        return encode_

instance GenericToGrace U1 where
    genericEncode = do
        let encode_ U1 = Value.Record mempty

        return encode_

instance GenericToGrace f => GenericToGrace (M1 D d f) where
    genericEncode = fmap (. unM1) genericEncode

instance GenericToGrace f => GenericToGrace (M1 C c f) where
    genericEncode = fmap (. unM1) genericEncode

instance (Selector s, ToGrace a) => GenericToGrace (M1 S s (K1 i a)) where
    genericEncode = do
        let m1 :: M1 S s (K1 i a) r
            m1 = undefined

        name <- Marshal.selector m1

        let encode_ (M1 (K1 a))
                | Generics.selName m1 == "" =
                    encode a
                | otherwise = Value.Record (HashMap.singleton name (encode a))

        return encode_

instance (Selector s₀, Selector s₁, ToGrace a₀, ToGrace a₁) => GenericToGrace (M1 S s₀ (K1 i₀ a₀) :*: M1 S s₁ (K1 i₁ a₁)) where
    genericEncode = do
        name₀ <- Marshal.selector (undefined :: M1 S s₀ (K1 i₀ a₀) r)
        name₁ <- Marshal.selector (undefined :: M1 S s₁ (K1 i₁ a₁) r)

        let encode_ (M1 (K1 a₀) :*: M1 (K1 a₁)) =
                Value.Record
                    (HashMap.fromList
                        [ (name₀, encode a₀), (name₁, encode a₁) ]
                    )

        return encode_

instance (Selector s, GenericToGrace (f₀ :*: f₁), ToGrace a) => GenericToGrace ((f₀ :*: f₁) :*: M1 S s (K1 i a)) where
    genericEncode = do
        encode₀ <- genericEncode

        name <- Marshal.selector (undefined :: M1 S s (K1 i a) r)

        let encode_ (f :*: M1 (K1 a)) = Value.Record
                ( HashMap.insert name (encode a)
                    (unsafeExpectRecordLiteral (encode₀ f))
                )

        return encode_

instance (Selector s, ToGrace a, GenericToGrace (f₀ :*: f₁)) => GenericToGrace (M1 S s (K1 i a) :*: (f₀ :*: f₁)) where
    genericEncode = do
        name <- Marshal.selector (undefined :: M1 S s (K1 i a) r)

        encode₁ <- genericEncode

        let encode_ (M1 (K1 a) :*: f) = Value.Record
                (HashMap.insert name (encode a)
                  (unsafeExpectRecordLiteral (encode₁ f))
                )

        return encode_

instance (GenericToGrace (f₀ :*: f₁), GenericToGrace (f₂ :*: f₃)) => GenericToGrace ((f₀ :*: f₁) :*: (f₂ :*: f₃)) where
    genericEncode = do
        encode₀ <- genericEncode
        encode₁ <- genericEncode

        let encode_ (f₀ :*: f₁) = Value.Record
                (   unsafeExpectRecordLiteral (encode₀ f₀)
                <>  unsafeExpectRecordLiteral (encode₁ f₁)
                )

        return encode_

instance (Constructor c₀, Constructor c₁, GenericToGrace f₀, GenericToGrace f₁) => GenericToGrace (M1 C c₀ f₀ :+: M1 C c₁ f₁) where
    genericEncode = do
        let name₀ = Text.pack (Generics.conName (undefined :: M1 i c₀ f₀ r))
        let name₁ = Text.pack (Generics.conName (undefined :: M1 i c₁ f₁ r))

        let encode₀ = State.evalState genericEncode 0
        let encode₁ = State.evalState genericEncode 0

        let encode_ (L1 (M1 f)) =
                Value.Application (Value.Alternative name₀) (encode₀ f)
            encode_ (R1 (M1 f)) =
                Value.Application (Value.Alternative name₁) (encode₁ f)

        return encode_

instance (Constructor c, GenericToGrace f₀, GenericToGrace (f₁ :+: f₂)) => GenericToGrace (M1 C c f₀ :+: (f₁ :+: f₂)) where
    genericEncode = do
        let name = Text.pack (Generics.conName (undefined :: M1 C c f₀ r))

        let encode₀ = State.evalState genericEncode 0
        let encode₁ = State.evalState genericEncode 0

        let encode_ (L1 (M1 f)) =
                Value.Application (Value.Alternative name) (encode₀ f)
            encode_ (R1 f) =
                encode₁ f

        return encode_

instance (Constructor c, GenericToGrace (f₀ :+: f₁), GenericToGrace f₂) => GenericToGrace ((f₀ :+: f₁) :+: M1 C c f₂) where
    genericEncode = do
        let name = Text.pack (Generics.conName (undefined :: M1 C c f₂ r))

        let encode₀ = State.evalState genericEncode 0
        let encode₁ = State.evalState genericEncode 0

        let encode_ (L1 f) =
                encode₀ f
            encode_ (R1 (M1 f)) =
                Value.Application (Value.Alternative name) (encode₁ f)

        return encode_

instance (GenericToGrace (f₀ :+: f₁), GenericToGrace (f₂ :+: f₃)) => GenericToGrace ((f₀ :+: f₁) :+: (f₂ :+: f₃)) where
    genericEncode = do
        let encode₀ = State.evalState genericEncode 0
        let encode₁ = State.evalState genericEncode 0

        let encode_ (L1 f) = encode₀ f
            encode_ (R1 f) = encode₁ f

        return encode_

unsafeExpectRecordLiteral :: Value -> InsOrdHashMap Text Value
unsafeExpectRecordLiteral (Value.Record fieldValues) =
    fieldValues
unsafeExpectRecordLiteral _ =
    error "Grace.Encode.unsafeExpectRecordLiteral: not a record"
