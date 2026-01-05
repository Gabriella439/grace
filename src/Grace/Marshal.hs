{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Internal module shared between "Grace.Decode" and "Grace.Encode"
module Grace.Marshal
    ( Key(..)
    , ToGraceType(..)
    , GenericToGraceType(..)
    , selector
    ) where

import Control.Monad.State (State)
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.String (IsString)
import Data.Text (Text)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Vector (Vector)
import Data.Void (Void)
import Grace.Type (Type)
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
import qualified Data.Aeson as Aeson
import qualified Data.Kind as Kind
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified GHC.Generics as Generics
import qualified Grace.Monotype as Monotype
import qualified Grace.Type as Type

-- | A protected `Text` value
newtype Key = Key{ text :: Text }
    deriving newtype (Eq, IsString, Show, FromJSON, ToJSON)

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

-- | Convert a Haskell type to a Grace type
class ToGraceType a where
    expected :: Type ()
    default expected :: (Generic a, GenericToGraceType (Rep a)) => Type ()
    expected = State.evalState (genericExpected @(Rep a)) 0

instance ToGraceType Void
instance ToGraceType ()
instance (ToGraceType a, ToGraceType b) => ToGraceType (a, b)
instance (ToGraceType a, ToGraceType b) => ToGraceType (Either a b)

instance ToGraceType Bool where
    expected = Type.Scalar{ location = (), scalar = Monotype.Bool }

instance ToGraceType Natural where
    expected = Type.Scalar{ location = (), scalar = Monotype.Natural }

instance ToGraceType Word where
    expected = expected @Natural

instance ToGraceType Word8 where
    expected = expected @Natural

instance ToGraceType Word16 where
    expected = expected @Natural

instance ToGraceType Word32 where
    expected = expected @Natural

instance ToGraceType Word64 where
    expected = expected @Natural

instance ToGraceType Integer where
    expected = Type.Scalar{ location = (), scalar = Monotype.Integer }

instance ToGraceType Int where
    expected = expected @Integer

instance ToGraceType Int8 where
    expected = expected @Integer

instance ToGraceType Int16 where
    expected = expected @Integer

instance ToGraceType Int32 where
    expected = expected @Integer

instance ToGraceType Int64 where
    expected = expected @Integer

instance ToGraceType Scientific where
    expected = Type.Scalar{ location = (), scalar = Monotype.Real }

instance ToGraceType Double where
    expected = expected @Scientific

instance ToGraceType Float where
    expected = expected @Scientific

instance ToGraceType Text where
    expected = Type.Scalar{ location = (), scalar = Monotype.Text }

instance ToGraceType Text.Lazy.Text where
    expected = expected @Text

instance {-# OVERLAPPING #-} ToGraceType [Char] where
    expected = expected @Text

instance ToGraceType Key where
    expected = Type.Scalar{ location = (), scalar = Monotype.Key }

instance ToGraceType Aeson.Value where
    expected = Type.Scalar{ location = (), scalar = Monotype.JSON }

instance ToGraceType a => ToGraceType (Seq a) where
    expected = Type.List{ location = (), type_ = expected @a }

instance ToGraceType a => ToGraceType [a] where
    expected = expected @(Seq a)

instance ToGraceType a => ToGraceType (Vector a) where
    expected = expected @[a]

instance ToGraceType a => ToGraceType (Maybe a) where
    expected = Type.Optional{ location = (), type_ = expected @a }

{-| This is the underlying class that powers the `ToGraceType` class's support
    for automatically deriving a `Generic` implementation
-}
class GenericToGraceType (f :: Kind.Type -> Kind.Type) where
    genericExpected :: State Int (Type ())

instance GenericToGraceType V1 where
    genericExpected = do
        return Type.Union
            { location = ()
            , alternatives = Type.Alternatives [] Monotype.EmptyAlternatives
            }

instance GenericToGraceType U1 where
    genericExpected = do
        return Type.Record
            { location = ()
            , fields = Type.Fields [] Monotype.EmptyFields
            }

instance GenericToGraceType f => GenericToGraceType (M1 D d f) where
    genericExpected = genericExpected @f

instance GenericToGraceType f => GenericToGraceType (M1 C d f) where
    genericExpected = genericExpected @f

instance (Selector s, ToGraceType a) => GenericToGraceType (M1 S s (K1 i a)) where
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

instance (Selector s₀, Selector s₁, ToGraceType a₀, ToGraceType a₁) => GenericToGraceType (M1 S s₀ (K1 i₀ a₀) :*: M1 S s₁ (K1 i₁ a₁)) where
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

instance (Selector s, GenericToGraceType (f₀ :*: f₁), ToGraceType a) => GenericToGraceType ((f₀ :*: f₁) :*: M1 S s (K1 i a)) where
    genericExpected = do
        expected₀ <- genericExpected @(f₀ :*: f₁)

        name <- selector (undefined :: M1 S s (K1 i a) r)

        return Type.Record
            { location = ()
            , fields = Type.Fields
                ((name, expected @a) : unsafeExpectRecordType expected₀)
                Monotype.EmptyFields
            }

instance (Selector s, ToGraceType a, GenericToGraceType (f₀ :*: f₁)) => GenericToGraceType (M1 S s (K1 i a) :*: (f₀ :*: f₁)) where
    genericExpected = do
        name <- selector (undefined :: M1 S s (K1 i a) r)

        expected₁ <- genericExpected @(f₀ :*: f₁)

        return Type.Record
            { location = ()
            , fields = Type.Fields
                ((name, expected @a) : unsafeExpectRecordType expected₁)
                Monotype.EmptyFields
            }

instance (GenericToGraceType (f₀ :*: f₁), GenericToGraceType (f₂ :*: f₃)) => GenericToGraceType ((f₀ :*: f₁) :*: (f₂ :*: f₃)) where
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

instance (Constructor c₀, Constructor c₁, GenericToGraceType f₀, GenericToGraceType f₁) => GenericToGraceType (M1 C c₀ f₀ :+: M1 C c₁ f₁) where
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

instance (Constructor c, GenericToGraceType f₀, GenericToGraceType (f₁ :+: f₂)) => GenericToGraceType (M1 C c f₀ :+: (f₁ :+: f₂)) where
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

instance (Constructor c, GenericToGraceType (f₀ :+: f₁), GenericToGraceType f₂) => GenericToGraceType ((f₀ :+: f₁) :+: M1 C c f₂) where
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

instance (GenericToGraceType (f₀ :+: f₁), GenericToGraceType (f₂ :+: f₃)) => GenericToGraceType ((f₀ :+: f₁) :+: (f₂ :+: f₃)) where
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
