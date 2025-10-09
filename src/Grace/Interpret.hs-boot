{-# LANGUAGE FlexibleContexts #-}

module Grace.Interpret where

import Control.Exception.Safe (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState)
import Data.Text (Text)
import Grace.HTTP (Methods)
import Grace.Input (Input(..))
import Grace.Location (Location(..))
import Grace.Type (Type)
import Grace.Value (Value)

import {-# SOURCE #-} Grace.Infer (Status)

-- | Like `interpret`, but accepts a custom list of bindings
interpretWith
    :: (MonadCatch m, MonadState Status m, MonadIO m)
    => (Text -> Methods)
    -- ^ OpenAI methods
    -> [(Text, Type Location, Value)]
    -- ^ @(name, type, value)@ for each custom binding
    -> Maybe (Type Location)
    -- ^ Optional expected type for the input
    -> Input
    -> m (Type Location, Value)

