{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

{-| This module provides a newtype wrapper for existential variables used in the
    type-checking algorithm.

    We don't use naked `Int`s since (empirically) conflating the various
    existentials is a common source of type-checking errors.
-}
module Grace.Existential
    ( -- * Types
      Existential
      -- * Utilities
    , internalName
    ) where

import Data.Text (Text)
import Grace.Pretty (Pretty(..), label)
import Language.Haskell.TH.Syntax (Lift)

import qualified Data.Char as Char
import qualified Data.Text as Text

{-| An existential variable

    The type variable is used to track what type of existential variable we're
    using, which will be one of these three types:

    * @`Existential` "Grace.Monotype".Monotype@ - An existential type
    * @`Existential` "Grace.Monotype".Record@ - An existential fields variable
    * @`Existential` "Grace.Monotype".Union@ - An existential alternatives
      variable
-}
newtype Existential a = UnsafeExistential Int
    deriving stock Lift
    deriving newtype (Eq, Num, Show)

instance Pretty (Existential a) where
    pretty (UnsafeExistential n) = label $ pretty $ prefix <> suffix
      where
        (prefix, q) = internalName n
        suffix = if q == 0 then "" else Text.pack (show (q - 1))

internalName :: Int -> (Text, Int)
internalName n = (prefix, q)
  where
    (q, r) = n `quotRem` 26

    prefix = Text.singleton $ Char.chr (Char.ord 'a' + r)
