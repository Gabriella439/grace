{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

{-| This module exists primarily to avoid a name clash with constructors of the
    same name in the "Grace.Type" module
-}
module Grace.Domain where

import GHC.Generics (Generic)

import Prettyprinter (Pretty(..))

-- | The domain over which a @forall@ is quantified
data Domain
    = Type
    -- ^ @forall (a : Type) . …@
    | Fields
    -- ^ @forall (a : Fields) . …@
    | Alternatives
    -- ^ @forall (a : Alternatives) . …@
    deriving stock (Eq, Generic, Show)

instance Pretty Domain where
    pretty Type         = "Type"
    pretty Fields       = "Fields"
    pretty Alternatives = "Alternatives"
