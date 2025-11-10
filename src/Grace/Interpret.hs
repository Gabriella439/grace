{-# LANGUAGE FlexibleContexts #-}

-- | This module implements the main interpretation function
module Grace.Interpret
    ( -- * Interpret
      Input(..)
    , interpret
    , interpretWith
    , load
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Grace.Decode (FromGrace(..))
import Grace.HTTP (Methods)
import Grace.Input (Input(..), Mode(..))
import Grace.Location (Location(..))
import Grace.Monad (Grace, Status(..))
import Grace.Type (Type)
import Grace.Value (Value)

import qualified Control.Exception.Safe as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Grace.Context as Context
import qualified Grace.HTTP as HTTP
import qualified Grace.Import as Import
import qualified Grace.Infer as Infer
import qualified Grace.Monad as Grace
import qualified Grace.Normalize as Normalize
import qualified Grace.Syntax as Syntax
import qualified Grace.Value as Value

{-| Interpret Grace source code, return the inferred type and the evaluated
    result

    This is the top-level function for the Grace interpreter
-}
interpret
    :: MonadIO io => (Text -> Methods) -> Input -> io (Type Location, Value)
interpret keyToMethods input = do
    let initialStatus = Status{ count = 0, context = [] }

    ((inferred, value), Status{ context }) <- do
        Grace.runGrace input initialStatus (interpretWith keyToMethods [] Nothing)

    return (Context.complete context inferred, Value.complete context value)

-- | Like `interpret`, but accepts a custom list of bindings
interpretWith
    :: (Text -> Methods)
    -- ^ OpenAI methods
    -> [(Text, Type Location, Value)]
    -- ^ @(name, type, value)@ for each custom binding
    -> Maybe (Type Location)
    -- ^ Optional expected type for the input
    -> Grace (Type Location, Value)
interpretWith keyToMethods bindings maybeAnnotation = do
    input <- Reader.ask

    expression <- liftIO (Import.resolve AsCode input)

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

    State.modify (\status -> status{ context = typeContext <> context status })

    (inferred, elaboratedExpression) <- Infer.infer annotatedExpression

    let evaluationContext = do
            (variable, _, value) <- bindings

            return (variable, value)

    value <- Normalize.evaluate keyToMethods evaluationContext elaboratedExpression

    return (inferred, value)

-- | Load a Grace expression
load :: forall m a . (FromGrace a, MonadIO m) => Input -> m a
load input = do
    keyToMethods <- liftIO HTTP.getMethods

    let type_ = fmap (\_ -> Unknown) (expected @a)

    let initialStatus = Status{ count = 0, context = [] }

    (_, value) <- Grace.evalGrace input initialStatus (interpretWith keyToMethods [] (Just type_) )

    case decode value of
        Left exception -> liftIO (Exception.throwIO exception)
        Right a -> return a
