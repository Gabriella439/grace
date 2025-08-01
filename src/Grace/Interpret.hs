{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

-- | This module implements the main interpretation function
module Grace.Interpret
    ( -- * Interpret
      Input(..)
    , interpret
    , interpretWith
    , load
    ) where

import Control.Exception.Safe (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Grace.Decode (Decoder(..))
import Grace.HTTP (Manager, Methods)
import Grace.Input (Input(..))
import Grace.Location (Location(..))
import Grace.Type (Type)
import Grace.Value (Value)

import qualified Control.Exception.Safe as Exception
import qualified Grace.Context as Context
import qualified Grace.HTTP as HTTP
import qualified Grace.Import as Import
import qualified Grace.Infer as Infer
import qualified Grace.Normalize as Normalize
import qualified Grace.Syntax as Syntax

{-| Interpret Grace source code, return the inferred type and the evaluated
    result

    This is the top-level function for the Grace interpreter
-}
interpret
    :: (MonadCatch m, MonadIO m)
    => (Text -> Methods) -> Input -> m (Type Location, Value)
interpret keyToMethods input = do
    manager <- liftIO HTTP.newManager

    interpretWith keyToMethods [] Nothing manager input

-- | Like `interpret`, but accepts a custom list of bindings
interpretWith
    :: (MonadCatch m, MonadIO m)
    => (Text -> Methods)
    -- ^ OpenAI methods
    -> [(Text, Type Location, Value)]
    -- ^ @(name, type, value)@ for each custom binding
    -> Maybe (Type Location)
    -- ^ Optional expected type for the input
    -> Manager
    -> Input
    -> m (Type Location, Value)
interpretWith keyToMethods bindings maybeAnnotation manager input = do
    expression <- liftIO (Import.resolve manager input)

    let annotatedExpression = case maybeAnnotation of
            Just annotation ->
                Syntax.Annotation
                    { annotated = expression
                    , annotation
                    , location = Syntax.location expression
                    }
            Nothing ->
                expression

    let typeContext = do
            (variable, type_, _) <- bindings

            return (Context.Annotation variable type_)

    (inferred, elaboratedExpression) <- Infer.typeWith input manager typeContext annotatedExpression

    let evaluationContext = do
            (variable, _, value) <- bindings

            return (variable, value)

    value <- liftIO (Normalize.evaluate keyToMethods evaluationContext elaboratedExpression)

    return (inferred, value)

-- | Load a Grace expression
load :: (MonadCatch m, MonadIO m) => Decoder a -> Input -> m a
load Decoder{ decode, expected } input = do
    keyToMethods <- liftIO HTTP.getMethods

    manager <- liftIO HTTP.newManager

    let type_ = fmap (\_ -> Unknown) expected

    (_, value) <- interpretWith keyToMethods [] (Just type_) manager input

    case decode value of
        Left exception -> Exception.throwM exception
        Right a -> return a
