{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

{-| This module stores the `Monotype` type representing monomorphic types and
    utilites for operating on `Monotype`s
-}
module Grace.Monotype
    ( -- * Types
      VariableId(..)
    , Monotype(..)
    , Scalar(..)
    , Record(..)
    , RemainingFields(..)
    , Union(..)
    , RemainingAlternatives(..)
      -- * Utilities
    , variableId
    ) where

import Data.List (elemIndex)
import Data.String (IsString(..))
import Data.Text (Text, pack, unsnoc)
import GHC.Generics (Generic)
import Grace.Existential (Existential)
import Grace.Pretty (Pretty(..), builtin, label)
import Language.Haskell.TH.Syntax (Lift)

data VariableId = VariableId Text Int
    deriving (Eq, Show, Lift)

{-| Construct non-unique VariableId out of raw Text. -}
variableId :: Text -> VariableId
variableId = \text ->
  let (name, ind) = takeIntFromEnd 0 1 text
  in VariableId name ind
  where
    takeIntFromEnd :: Int -> Int -> Text -> (Text, Int)
    takeIntFromEnd curr mult text
      | Just (xs, x) <- unsnoc text
      , Just ind <- x `elemIndex` "0123456789"
        = takeIntFromEnd (curr + ind*mult) (10*mult) xs
    takeIntFromEnd curr _ xs = (xs, curr)

instance IsString VariableId where
  fromString = variableId . pack

instance Pretty VariableId where
  pretty (VariableId text ind) = label $ pretty (text <> if ind == 0 then "" else fromString $ show (ind - 1))

{-| A monomorphic type

    This is same type as `Grace.Type.Type`, except without the
    `Grace.Type.Forall` and `Grace.Type.Exists` constructors
-}
data Monotype
    = VariableType VariableId
    | UnsolvedType (Existential Monotype)
    | Function Monotype Monotype
    | Optional Monotype
    | List Monotype
    | Record Record
    | Union Union
    | Scalar Scalar
    deriving stock (Eq, Generic, Show)

instance IsString Monotype where
    fromString = VariableType . fromString

-- | A scalar type
data Scalar
    = Bool
    -- ^ Boolean type
    --
    -- >>> pretty Bool
    -- Bool
    | Real
    -- ^ Real number type
    --
    -- >>> pretty Real
    -- Real
    | Integer
    -- ^ Integer number type
    --
    -- >>> pretty Integer
    -- Integer
    | JSON
    -- ^ JSON type
    --
    -- >>> pretty JSON
    -- JSON
    | Natural
    -- ^ Natural number type
    --
    -- >>> pretty Natural
    -- Natural
    | Text
    -- ^ Text type
    --
    -- >>> pretty Text
    -- Text
    deriving stock (Eq, Generic, Lift, Show)

instance Pretty Scalar where
    pretty Bool    = builtin "Bool"
    pretty Real    = builtin "Real"
    pretty JSON    = builtin "JSON"
    pretty Natural = builtin "Natural"
    pretty Integer = builtin "Integer"
    pretty Text    = builtin "Text"

-- | A monomorphic record type
data Record = Fields [(Text, Monotype)] RemainingFields
    deriving stock (Eq, Generic, Show)

-- | This represents whether or not the record type is open or closed
data RemainingFields
    = EmptyFields
    -- ^ The record type is closed, meaning that all fields are known
    | UnsolvedFields (Existential Record)
    -- ^ The record type is open, meaning that some fields are known and there
    --   is an unsolved fields variable that is a placeholder for other fields
    --   that may or may not be present
    | VariableFields VariableId
    -- ^ Same as `UnsolvedFields`, except that the user has given the fields
    --   variable an explicit name in the source code
    deriving stock (Eq, Generic, Lift, Show)

-- | A monomorphic union type
data Union = Alternatives [(Text, Monotype)] RemainingAlternatives
    deriving stock (Eq, Generic, Show)

-- | This represents whether or not the union type is open or closed
data RemainingAlternatives
    = EmptyAlternatives
    -- ^ The union type is closed, meaning that all alternatives are known
    | UnsolvedAlternatives (Existential Union)
    -- ^ The union type is open, meaning that some alternatives are known and
    --   there is an unsolved alternatives variable that is a placeholder for
    --   other alternatives that may or may not be present
    | VariableAlternatives VariableId
    -- ^ Same as `UnsolvedAlternatives`, except that the user has given the
    --   alternatives variable an explicit name in the source code
    deriving stock (Eq, Generic, Lift, Show)
