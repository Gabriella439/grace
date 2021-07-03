{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

{-| This module exists primarily to avoid a name clash with constructors of the
    same name in the "Grace.Type" module
-}
module Grace.Domain where

import GHC.Generics (Generic)

-- | The domain over which a @forall@ is quantified
data Domain
    = Type
    -- ^ @forall (a : Type) . …@
    | Fields
    -- ^ @forall (a : Fields) . …@
    | Alternatives
    -- ^ @forall (a : Alternatives) . …@
    deriving stock (Eq, Generic, Show)
