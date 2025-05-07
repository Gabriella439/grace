{-# LANGUAGE FlexibleContexts #-}

module Grace.Interpret where

import Control.Exception (Exception)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Grace.HTTP (Manager)
import Grace.Input (Input(..))
import Grace.Location (Location(..))
import Grace.Type (Type)
import Grace.Value (Value)
import OpenAI.V1 (Methods)

interpretWith
    :: (MonadError InterpretError m, MonadIO m)
    => Maybe Methods
    -- ^ OpenAI methods
    -> [(Text, Type Location, Value)]
    -- ^ @(name, type, value)@ for each custom binding
    -> Maybe (Type Location)
    -- ^ Optional expected type for the input
    -> Manager
    -> Input
    -> m (Type Location, Value)

data InterpretError

instance Exception InterpretError
instance Show InterpretError
