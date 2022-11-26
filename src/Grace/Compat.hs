{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | An internal utility module for backwards compatibility across GHC releases
-- and different libary versions.
module Grace.Compat where

import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Text (Text)

import qualified Data.HashMap.Strict.InsOrd as HashMap

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
#else
fromAesonMap :: HashMap Text v -> InsOrdHashMap Text v
fromAesonMap = HashMap.fromHashMap
#endif

#if !MIN_VERSION_containers(0, 6, 6)
deriving instance Lift a => Lift (Seq a)
deriving instance Lift a => Lift (Digit a)
deriving instance Lift a => Lift (Elem a)
deriving instance Lift a => Lift (FingerTree a)
deriving instance Lift a => Lift (Node a)
#endif
