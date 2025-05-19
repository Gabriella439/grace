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
    , stripSome
    ) where

import Control.Exception.Safe (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Grace.HTTP (Manager, Methods)
import Grace.Input (Input(..))
import Grace.Location (Location(..))
import Grace.Type (Type)
import Grace.Value (Value)

import qualified Control.Lens as Lens
import qualified Grace.Context as Context
import qualified Grace.HTTP as HTTP
import qualified Grace.Import as Import
import qualified Grace.Infer as Infer
import qualified Grace.Normalize as Normalize
import qualified Grace.Syntax as Syntax
import qualified Grace.Value as Value

{-| Interpret Grace source code, return the inferred type and the evaluated
    result

    This is the top-level function for the Grace interpreter
-}
interpret
    :: (MonadCatch m, MonadIO m)
    => Maybe Methods -> Input -> m (Type Location, Value)
interpret maybeMethods input = do
    manager <- liftIO HTTP.newManager

    interpretWith maybeMethods [] Nothing manager input

-- | Like `interpret`, but accepts a custom list of bindings
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
interpretWith maybeMethods bindings maybeAnnotation manager input = do
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

    value <- liftIO (Normalize.evaluate maybeMethods evaluationContext elaboratedExpression)

    return (inferred, stripSome value)

-- | Strip all @some@s from the final result.  They are only used internally for
-- the purpose of evaluation but do not need to be user-visible.
stripSome :: Value -> Value
stripSome = Lens.transform transformValue
  where
    transformValue (Value.Application (Value.Builtin Syntax.Some) e) = e
    transformValue e = e
