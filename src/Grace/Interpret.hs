{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

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
import Grace.Decode (FromGrace(..))
import Grace.HTTP (Methods)
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
    interpretWith keyToMethods [] Nothing input

-- | Like `interpret`, but accepts a custom list of bindings
interpretWith
    :: (MonadCatch m, MonadIO m)
    => (Text -> Methods)
    -- ^ OpenAI methods
    -> [(Text, Type Location, Value)]
    -- ^ @(name, type, value)@ for each custom binding
    -> Maybe (Type Location)
    -- ^ Optional expected type for the input
    -> Input
    -> m (Type Location, Value)
interpretWith keyToMethods bindings maybeAnnotation input = do
    expression <- liftIO (Import.resolve input)

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

    (inferred, elaboratedExpression) <- Infer.typeWith input typeContext annotatedExpression

    let evaluationContext = do
            (variable, _, value) <- bindings

            return (variable, value)

    value <- liftIO (Normalize.evaluate keyToMethods evaluationContext elaboratedExpression)

    return (inferred, value)

-- | Load a Grace expression
load :: forall m a . (FromGrace a, MonadCatch m, MonadIO m) => Input -> m a
load input = do
    keyToMethods <- liftIO HTTP.getMethods

    let type_ = fmap (\_ -> Unknown) (expected @a)

    (_, value) <- interpretWith keyToMethods [] (Just type_) input

    case decode value of
        Left exception -> Exception.throwM exception
        Right a -> return a
