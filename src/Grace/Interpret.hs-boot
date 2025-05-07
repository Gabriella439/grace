{-# LANGUAGE FlexibleContexts #-}

module Grace.Interpret where

import Control.Exception.Safe (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Grace.HTTP (Manager, Methods)
import Grace.Input (Input(..))
import Grace.Location (Location(..))
import Grace.Type (Type)
import Grace.Value (Value)

interpretWith
    :: (MonadCatch m, MonadIO m)
    => Maybe Methods
    -- ^ OpenAI methods
    -> [(Text, Type Location, Value)]
    -- ^ @(name, type, value)@ for each custom binding
    -> Maybe (Type Location)
    -- ^ Optional expected type for the input
    -> Manager
    -> Input
    -> m (Type Location, Value)
