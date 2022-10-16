{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Grace.Utils where

#if !MIN_VERSION_containers(0, 6, 6)
import Data.Sequence
import Data.Sequence.Internal
import Language.Haskell.TH.Syntax (Lift(..))

deriving instance Lift a => Lift (Seq a)
deriving instance Lift a => Lift (Digit a)
deriving instance Lift a => Lift (Elem a)
deriving instance Lift a => Lift (FingerTree a)
deriving instance Lift a => Lift (Node a)
#endif
