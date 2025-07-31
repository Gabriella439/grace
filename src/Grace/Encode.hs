{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

{-| Use this module to encode Haskell expressions as Grace expressions

    Example usage:

    >>> encode encoder True
    Scalar{ location = (), scalar = Bool True }
-}
module Grace.Encode
    ( ToGrace(..)
    , Encoder(..)
    , Key(..)
    ) where

import Control.Monad.State (State)
import Data.Functor.Contravariant (Contravariant(..))
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Void (Void)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.String (IsString)
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)
import Grace.Syntax (Syntax(..))
import Grace.Type (Type(..))
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
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Vector as Vector
import qualified GHC.Generics as Generics
import qualified Grace.Monotype as Monotype
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type

-- | Convert a Haskell expression to a Grace expression
class ToGrace a where
    encoder :: Encoder a

    default encoder :: (Generic a, GenericToGrace (Rep a)) => Encoder a
    encoder = contramap from (State.evalState genericEncoder 0)

instance ToGrace Void where
instance ToGrace ()
instance (ToGrace a, ToGrace b) => ToGrace (a, b)
instance (ToGrace a, ToGrace b) => ToGrace (Either a b)

instance ToGrace Bool where
    encoder = Encoder{ encode, expected }
      where
        encode bool = Syntax.Scalar{ location = (), scalar = Syntax.Bool bool }

        expected = Type.Scalar{ location = (), scalar = Monotype.Bool }

instance ToGrace Natural where
    encoder = Encoder{ encode, expected }
      where
        encode natural =
            Syntax.Scalar{ location = (), scalar = Syntax.Natural natural }

        expected = Type.Scalar{ location = (), scalar = Monotype.Natural }

instance ToGrace Word where
    encoder = contramap fromIntegral (encoder @Natural)

instance ToGrace Word8 where
    encoder = contramap fromIntegral (encoder @Natural)

instance ToGrace Word16 where
    encoder = contramap fromIntegral (encoder @Natural)

instance ToGrace Word32 where
    encoder = contramap fromIntegral (encoder @Natural)

instance ToGrace Word64 where
    encoder = contramap fromIntegral (encoder @Natural)

instance ToGrace Integer where
    encoder = Encoder{ encode, expected }
      where
        encode integer =
            Syntax.Scalar{ location = (), scalar = Syntax.Integer integer }

        expected = Type.Scalar{ location = (), scalar = Monotype.Integer }

instance ToGrace Int where
    encoder = contramap fromIntegral (encoder @Integer)

instance ToGrace Int8 where
    encoder = contramap fromIntegral (encoder @Integer)

instance ToGrace Int16 where
    encoder = contramap fromIntegral (encoder @Integer)

instance ToGrace Int32 where
    encoder = contramap fromIntegral (encoder @Integer)

instance ToGrace Int64 where
    encoder = contramap fromIntegral (encoder @Integer)

instance ToGrace Scientific where
    encoder = Encoder{ encode, expected }
      where
        encode scientific =
            Syntax.Scalar{ location = (), scalar = Syntax.Real scientific }

        expected = Type.Scalar{ location = (), scalar = Monotype.Real }

instance ToGrace Float where
    encoder = contramap Scientific.fromFloatDigits (encoder @Scientific)

instance ToGrace Double where
    encoder = contramap Scientific.fromFloatDigits (encoder @Scientific)

instance ToGrace Text where
    encoder = Encoder{ encode, expected }
      where
        encode text =
            Syntax.Text{ location = (), chunks = Syntax.Chunks text [] }

        expected = Type.Scalar{ location = (), scalar = Monotype.Text }

instance ToGrace Text.Lazy.Text where
    encoder = contramap Text.Lazy.toStrict encoder

instance {-# OVERLAPPING #-} ToGrace [Char] where
    encoder = contramap Text.pack encoder

instance ToGrace a => ToGrace (Seq a) where
    encoder = Encoder{ encode, expected }
      where
        Encoder{ encode = encode₀, expected = expected₀ } = encoder

        encode list =
            Syntax.List{ location = (), elements = fmap encode₀ list }

        expected = Type.List{ location = (), type_ = expected₀ }

instance ToGrace a => ToGrace [a] where
    encoder = contramap Seq.fromList encoder

instance ToGrace a => ToGrace (Vector a) where
    encoder = contramap Vector.toList encoder

instance ToGrace a => ToGrace (Maybe a) where
    encoder = Encoder{ encode, expected }
      where
        Encoder{ encode = encode₀, expected = expected₀ } = encoder

        encode (Just a) =
            Syntax.Application
                { location = ()
                , function = Syntax.Builtin
                    { location = ()
                    , builtin = Syntax.Some
                    }
                , argument = encode₀ a
                }
        encode Nothing =
            Syntax.Scalar{ location = (), scalar = Syntax.Null }

        expected = Type.Optional{ location = (), type_ = expected₀ }

{-| An `Encoder` represents both the encoding logic for a value alongside its
    type

    The reason why we do this:

@
class `ToGrace` a where
    `encoder` :: `Encoder` s a
@

    … and not this:

@
class `ToGrace` a where
    encode :: a -> `Syntax` s `Void`

    expected :: Type s
@

    … is because the latter requires @AllowAmbiguousTypes@ in order to use such
    an @expected@ method (since @a@ appears nowhere in the type of @expected@).

    Instead, if you do this:

@
`encode` foo
  where
    `Encoder`{ `encode`, `expected` } = `encoder`
@

    … then the compiler can infer which `expected` to use based on which type
    `encode` is instantiated to.
-}
data Encoder a = Encoder
    { encode :: a -> Syntax () Void
    , expected :: Type ()
    }

instance Contravariant Encoder where
    contramap f Encoder{ encode, expected } =
        Encoder{ encode = encode . f, expected }

-- | A protected `Text` value
newtype Key = Key{ text :: Text }
    deriving newtype (IsString)

instance ToGrace Key where
    encoder = Encoder{ encode, expected }
      where
        encode Key{ text } =
            Syntax.Scalar{ location = (), scalar = Syntax.Key text }

        expected = Type.Scalar{ location = (), scalar = Monotype.Key }

{-| This is the underlying class that powers the `ToGrace` class's support for
    automatically deriving a `Generic` implementation
-}
class GenericToGrace f where
    genericEncoder :: State Int (Encoder (f a))

instance GenericToGrace V1 where
    genericEncoder = do
        -- EmptyCase does not work here and produces a non-exhaustive pattern
        -- match warning
        let encode _ = error "Grace.Encode.genericEncoder: V1 inhabited"

        let expected = Type.Union
                { location = ()
                , alternatives = Type.Alternatives [] Monotype.EmptyAlternatives
                }

        return Encoder{ encode, expected }

instance GenericToGrace U1 where
    genericEncoder = do
        return Encoder{ encode, expected }
      where
        encode U1 = Syntax.Record{ location = (), fieldValues = [] }

        expected = Type.Record
            { location = ()
            , fields = Type.Fields [] Monotype.EmptyFields
            }

instance GenericToGrace f => GenericToGrace (M1 D d f) where
    genericEncoder = fmap (contramap unM1) genericEncoder

instance GenericToGrace f => GenericToGrace (M1 C c f) where
    genericEncoder = fmap (contramap unM1) genericEncoder

instance (Selector s, ToGrace a) => GenericToGrace (M1 S s (K1 i a)) where
    genericEncoder = do
        let m1 :: M1 S s (K1 i a) r
            m1 = undefined

        name <- selector m1

        let Encoder{ encode = encode₀, expected = expected₀ } = encoder

        let encode (M1 (K1 a))
                | Generics.selName m1 == "" =
                    encode₀ a
                | otherwise =
                    Syntax.Record
                        { location = ()
                        , fieldValues = [ (name, encode₀ a) ]
                        }

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

        return Encoder{ encode, expected }

instance (Selector s₀, Selector s₁, ToGrace a₀, ToGrace a₁) => GenericToGrace (M1 S s₀ (K1 i₀ a₀) :*: M1 S s₁ (K1 i₁ a₁)) where
    genericEncoder = do
        name₀ <- selector (undefined :: M1 S s₀ (K1 i₀ a₀) r)
        name₁ <- selector (undefined :: M1 S s₁ (K1 i₁ a₁) r)

        let Encoder{ encode = encode₀, expected = expected₀ } = encoder
        let Encoder{ encode = encode₁, expected = expected₁ } = encoder

        let encode (M1 (K1 a₀) :*: M1 (K1 a₁)) = Syntax.Record
                { location = ()
                , fieldValues = [ (name₀, encode₀ a₀), (name₁, encode₁ a₁) ]
                }

        let expected = Type.Record
                { location = ()
                , fields = Type.Fields
                    [ (name₀, expected₀), (name₁, expected₁) ]
                    Monotype.EmptyFields
                }

        return Encoder{ encode, expected }

instance (Selector s, GenericToGrace (f₀ :*: f₁), ToGrace a) => GenericToGrace ((f₀ :*: f₁) :*: M1 S s (K1 i a)) where
    genericEncoder = do
        Encoder{ encode = encode₀, expected = expected₀ } <- genericEncoder

        name <- selector (undefined :: M1 S s (K1 i a) r)

        let Encoder{ encode = encode₁, expected = expected₁ } = encoder

        let encode (f :*: M1 (K1 a)) = Syntax.Record
                { location = ()
                , fieldValues =
                        unsafeExpectRecordLiteral (encode₀ f)
                    <>  [ (name, encode₁ a) ]
                }

        let expected = Type.Record
                { location = ()
                , fields = Type.Fields
                    (unsafeExpectRecordType expected₀ <> [ (name, expected₁) ])
                    Monotype.EmptyFields
                }

        return Encoder{ encode, expected }

instance (Selector s, ToGrace a, GenericToGrace (f₀ :*: f₁)) => GenericToGrace (M1 S s (K1 i a) :*: (f₀ :*: f₁)) where
    genericEncoder = do
        name <- selector (undefined :: M1 S s (K1 i a) r)

        let Encoder{ encode = encode₀, expected = expected₀ } = encoder

        Encoder{ encode = encode₁, expected = expected₁ } <- genericEncoder

        let encode (M1 (K1 a) :*: f) = Syntax.Record
                { location = ()
                , fieldValues =
                    (name, encode₀ a) : unsafeExpectRecordLiteral (encode₁ f)
                }

        let expected = Type.Record
                { location = ()
                , fields = Type.Fields
                    ((name, expected₀) : unsafeExpectRecordType expected₁)
                    Monotype.EmptyFields
                }

        return Encoder{ encode, expected }

instance (GenericToGrace (f₀ :*: f₁), GenericToGrace (f₂ :*: f₃)) => GenericToGrace ((f₀ :*: f₁) :*: (f₂ :*: f₃)) where
    genericEncoder = do
        Encoder{ encode = encode₀, expected = expected₀ } <- genericEncoder
        Encoder{ encode = encode₁, expected = expected₁ } <- genericEncoder

        let encode (f₀ :*: f₁) = Syntax.Record
                { location = ()
                , fieldValues =
                        unsafeExpectRecordLiteral (encode₀ f₀)
                    <>  unsafeExpectRecordLiteral (encode₁ f₁)
                }

        let expected = Type.Record
                { location = ()
                , fields = Type.Fields
                    (   unsafeExpectRecordType expected₀
                    <>  unsafeExpectRecordType expected₁
                    )
                    Monotype.EmptyFields
                }

        return Encoder{ encode, expected }

instance (Constructor c₀, Constructor c₁, GenericToGrace f₀, GenericToGrace f₁) => GenericToGrace (M1 C c₀ f₀ :+: M1 C c₁ f₁) where
    genericEncoder = do
        let name₀ = Text.pack (Generics.conName (undefined :: M1 i c₀ f₀ r))
        let name₁ = Text.pack (Generics.conName (undefined :: M1 i c₁ f₁ r))

        let Encoder{ encode = encode₀, expected = expected₀ } =
                State.evalState genericEncoder 0
        let Encoder{ encode = encode₁, expected = expected₁ } =
                State.evalState genericEncoder 0

        let encode (L1 (M1 f)) = Syntax.Application
                { location = ()
                , function = Syntax.Alternative{ location = (), name = name₀ }
                , argument = encode₀ f
                }
            encode (R1 (M1 f)) = Syntax.Application
                { location = ()
                , function = Syntax.Alternative{ location = (), name = name₁ }
                , argument = encode₁ f
                }

        let expected = Type.Union
                { location = ()
                , alternatives = Type.Alternatives
                    [ (name₀, expected₀), (name₁, expected₁) ]
                    Monotype.EmptyAlternatives
                }

        return Encoder{ encode, expected }

instance (Constructor c, GenericToGrace f₀, GenericToGrace (f₁ :+: f₂)) => GenericToGrace (M1 C c f₀ :+: (f₁ :+: f₂)) where
    genericEncoder = do
        let name = Text.pack (Generics.conName (undefined :: M1 C c f₀ r))

        let Encoder{ encode = encode₀, expected = expected₀ } =
                State.evalState genericEncoder 0
        let Encoder{ encode = encode₁, expected = expected₁ } =
                State.evalState genericEncoder 0

        let encode (L1 (M1 f)) =
                Syntax.Application
                    { location = ()
                    , function = Syntax.Alternative{ location = (), name }
                    , argument = encode₀ f
                    }
            encode (R1 f) =
                encode₁ f

        let expected = Type.Union
                { location = ()
                , alternatives = Type.Alternatives
                    (   unsafeExpectUnion expected₀
                    <>  unsafeExpectUnion expected₁
                    )
                    Monotype.EmptyAlternatives
                }

        return Encoder{ encode, expected }

instance (Constructor c, GenericToGrace (f₀ :+: f₁), GenericToGrace f₂) => GenericToGrace ((f₀ :+: f₁) :+: M1 C c f₂) where
    genericEncoder = do
        let name = Text.pack (Generics.conName (undefined :: M1 C c f₂ r))

        let Encoder{ encode = encode₀, expected = expected₀ } =
                State.evalState genericEncoder 0
        let Encoder{ encode = encode₁, expected = expected₁ } =
                State.evalState genericEncoder 0

        let encode (L1 f) =
                encode₀ f
            encode (R1 (M1 f)) =
                Syntax.Application
                    { location = ()
                    , function = Syntax.Alternative{ location = (), name }
                    , argument = encode₁ f
                    }

        let expected = Type.Union
                { location = ()
                , alternatives = Type.Alternatives
                    (   unsafeExpectUnion expected₀
                    <>  unsafeExpectUnion expected₁
                    )
                    Monotype.EmptyAlternatives
                }

        return Encoder{ encode, expected }

instance (GenericToGrace (f₀ :+: f₁), GenericToGrace (f₂ :+: f₃)) => GenericToGrace ((f₀ :+: f₁) :+: (f₂ :+: f₃)) where
    genericEncoder = do
        let Encoder{ encode = encode₀, expected = expected₀ } =
                State.evalState genericEncoder 0
        let Encoder{ encode = encode₁, expected = expected₁ } =
                State.evalState genericEncoder 0

        let encode (L1 f) = encode₀ f
            encode (R1 f) = encode₁ f

        let expected = Type.Union
                { location = ()
                , alternatives = Type.Alternatives
                    (   unsafeExpectUnion expected₀
                    <>  unsafeExpectUnion expected₁
                    )
                    Monotype.EmptyAlternatives
                }

        return Encoder{ encode, expected }

selector :: Selector s => M1 S s f r -> State Int Text
selector m1 = do
    let name₀ = Generics.selName m1

    if name₀ == ""
        then do
            n <- State.get

            State.put $! n + 1

            return (Text.pack (show n))
        else do
            return (Text.pack name₀)

unsafeExpectRecordLiteral :: Syntax s a -> [(Text, Syntax s a)]
unsafeExpectRecordLiteral Syntax.Record{ fieldValues } =
    fieldValues
unsafeExpectRecordLiteral _ =
    error "Grace.Encode.unsafeExpectRecordLiteral: not a record"

unsafeExpectRecordType :: Type s -> [(Text, Type s)]
unsafeExpectRecordType Type.Record{ fields = Type.Fields fieldTypes _ } =
    fieldTypes
unsafeExpectRecordType _ =
    error "Grace.Encode.unsafeExpectRecordType: not a record"

unsafeExpectUnion :: Type s -> [(Text, Type s)]
unsafeExpectUnion Type.Union{ alternatives = Type.Alternatives alternativeTypes _ } =
    alternativeTypes
unsafeExpectUnion _ =
    error "Grace.Encode.unsafeExpectUnion: not a union"
