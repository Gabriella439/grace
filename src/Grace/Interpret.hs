{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import Grace.Encode (ToGrace(..), ToGraceType(..))
import Grace.Input (Input(..), Mode(..))
import Grace.Location (Location(..))
import Grace.Monad (Grace, Status(..))
import Grace.Type (Type)
import Grace.Value (Value)

import qualified Control.Exception.Safe as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Grace.Context as Context
import qualified Grace.Import as Import
import qualified Grace.Infer as Infer
import qualified Grace.Monad as Grace
import qualified Grace.Normalize as Normalize
import qualified Grace.Pretty as Pretty
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type
import qualified Grace.Value as Value

{-| Interpret Grace source code, return the inferred type and the evaluated
    result

    This is the top-level function for the Grace interpreter
-}
interpret :: MonadIO io => Input -> io (Type Location, Value)
interpret input = do
    let initialStatus = Status{ count = 0, context = [] }

    ((inferred, value), Status{ context }) <- do
        Grace.runGrace input initialStatus (interpretWith [] Nothing)

    return (Context.complete context inferred, Value.complete context value)

-- | Like `interpret`, but accepts a custom list of bindings
interpretWith
    :: [(Text, Type Location, Value)]
    -- ^ @(name, type, value)@ for each custom binding
    -> Maybe (Type Location)
    -- ^ Optional expected type for the input
    -> Grace (Type Location, Value)
interpretWith bindings maybeAnnotation = do
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

    value <- Normalize.evaluate evaluationContext elaboratedExpression

    return (inferred, value)

-- | Load a Grace expression
load :: forall m a . (FromGrace a, MonadIO m) => Input -> m a
load input = do
    let type_ = fmap (\_ -> Unknown) (expected @a)

    let initialStatus = Status{ count = 0, context = [] }

    (_, value) <- Grace.evalGrace input initialStatus (interpretWith [] (Just type_) )

    case decode value of
        Left exception -> liftIO (Exception.throwIO exception)
        Right a -> return a

instance (ToGrace a, FromGrace b) => FromGrace (a -> IO b) where
    decode function = do
        return \a -> do
            let inputValue = encode a

            let initialStatus = Status{ count = 0, context = [] }

            let code = Pretty.toText (Normalize.quote inputValue)

            let input = Code "(decode)" code

            outputValue <- Grace.evalGrace input initialStatus (Normalize.apply function inputValue)

            case decode outputValue of
                Left  e -> Exception.throwIO e
                Right b -> return b

instance (ToGraceType a, ToGraceType b) => ToGraceType (a -> IO b) where
    expected = Type.Function
        { location = ()
        , input = expected @a
        , output = expected @b
        }
