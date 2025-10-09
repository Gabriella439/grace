{-# LANGUAGE FlexibleContexts #-}

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
import Grace.Infer (Status(..))
import Grace.Input (Input(..))
import Grace.Location (Location(..))
import Grace.Type (Type)
import Grace.Value (Value)

import qualified Control.Exception.Safe as Exception
import qualified Control.Lens as Lens
import qualified Control.Monad.State as State
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
    => (Text -> Methods) -> Input -> m (Type Location, Value)
interpret keyToMethods input = do
    interpretWith keyToMethods initialStatus [] Nothing input
  where
    initialStatus = Status{ count = 0, input, context = [] }

-- | Like `interpret`, but accepts a custom list of bindings
interpretWith
    :: (MonadCatch m, MonadIO m)
    => (Text -> Methods)
    -- ^ OpenAI methods
    -> Status
    -- ^ Type-checking status
    -> [(Text, Type Location, Value)]
    -- ^ @(name, type, value)@ for each custom binding
    -> Maybe (Type Location)
    -- ^ Optional expected type for the input
    -> Input
    -> m (Type Location, Value)
interpretWith keyToMethods status₀ bindings maybeAnnotation input = do
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

    let status₁ = status₀{ context = typeContext <> context status₀ }

    ((inferred, elaboratedExpression), status₂@Status{ context }) <- State.runStateT (Infer.infer annotatedExpression) status₁

    let evaluationContext = do
            (variable, _, value) <- bindings

            return (variable, value)

    value <- liftIO (Normalize.evaluate keyToMethods status₂ evaluationContext elaboratedExpression)

    return (Context.complete context inferred, Lens.over Value.types (Context.complete context) value)

-- | Load a Grace expression
load :: forall m a . (FromGrace a, MonadCatch m, MonadIO m) => Input -> m a
load input = do
    keyToMethods <- liftIO HTTP.getMethods

    let type_ = fmap (\_ -> Unknown) (expected @a)

    let initialStatus = Status{ count = 0, input, context = [] }

    (_, value) <- interpretWith keyToMethods initialStatus [] (Just type_) input

    case decode value of
        Left exception -> Exception.throwM exception
        Right a -> return a
