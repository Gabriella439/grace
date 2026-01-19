{-# LANGUAGE FlexibleContexts #-}

module Grace.Interpret where

import Data.Text (Text)
import Grace.Location (Location)
import Grace.Monad (Grace)
import Grace.Type (Type)
import Grace.Value (Value)

-- | Like `interpret`, but accepts a custom list of bindings
interpretWith
    :: [(Text, Type Location, Value Location )]
    -- ^ @(name, type, value)@ for each custom binding
    -> Maybe (Type Location)
    -- ^ Optional expected type for the input
    -> Grace (Type Location, Value Location)
