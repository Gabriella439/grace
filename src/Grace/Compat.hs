{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | An internal utility module for backwards compatibility across GHC releases
-- and different libary versions.
module Grace.Compat where

import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Text (Text)

import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.HashMap.Strict as HashMap.Strict
import qualified Data.List as List

#if MIN_VERSION_aeson(2, 0, 0)
import Data.Aeson.KeyMap (KeyMap)

import qualified Data.Aeson.KeyMap as Aeson
#else
import Data.HashMap.Strict (HashMap)
#endif

#if !MIN_VERSION_containers(0, 6, 6)
import Data.Sequence.Internal (Digit(..), Elem(..), FingerTree(..), Node(..), Seq(..))
import Language.Haskell.TH.Syntax (Lift(..))
#endif

#if MIN_VERSION_aeson(2, 0, 0)
fromAesonMap :: KeyMap v -> InsOrdHashMap Text v
fromAesonMap = HashMap.fromHashMap . Aeson.toHashMapText

toAesonMap :: InsOrdHashMap Text v -> KeyMap v
toAesonMap = Aeson.fromHashMapText . HashMap.toHashMap

sorted :: KeyMap value -> [(Text, value)]
sorted = List.sortOn fst . HashMap.Strict.toList . Aeson.toHashMapText
#else
fromAesonMap :: HashMap Text v -> InsOrdHashMap Text v
fromAesonMap = HashMap.fromHashMap

toAesonMap :: InsOrdHashMap Text v -> HashMap Text v
toAesonMap = HashMap.toHashMap

sorted :: HashMap Text v -> [(Text, v)]
sorted = List.sortOn fst . HashMap.Strict.toList
#endif

#if !MIN_VERSION_containers(0, 6, 6)
deriving stock instance Lift a => Lift (Seq a)
deriving stock instance Lift a => Lift (Digit a)
deriving stock instance Lift a => Lift (Elem a)
deriving stock instance Lift a => Lift (FingerTree a)
deriving stock instance Lift a => Lift (Node a)
#endif
