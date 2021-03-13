{-| This module provides newtype wrappers for the different types of
    existential variables used in the type-checking algorithm.

    We don't use naked `Int`s since (empirically), conflating the various
    existentials is a common source of type-checking errors.
-}
module Grace.Existential
    ( -- * Types
      ExistentialType(..)
    , ExistentialRow(..)
    ) where

-- | An unsolved type variable
newtype ExistentialType = ExistentialType Int
    deriving newtype (Eq, Ord, Num, Show)

-- | An unsolved row variable
newtype ExistentialRow = ExistentialRow Int
    deriving newtype (Eq, Ord, Num, Show)
