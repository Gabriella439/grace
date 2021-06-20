{-| This module exists primarily to avoid a name clash with constructors of the
    same name in the `Grace.Type` module
-}
module Grace.Domain where

-- | The domain over which a @forall@ is quantified
data Domain = Type | Fields | Alternatives
    deriving stock (Eq, Ord, Show)
