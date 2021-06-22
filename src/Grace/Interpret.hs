{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

-- | This module implements support for file-based imports
module Grace.Interpret
    ( -- * Interpret
      Input(..)
    , interpret
    ) where

import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Text (Text)
import Grace.Location (Location(..))
import Grace.Type (Type)
import Grace.Value (Value)
import System.FilePath ((</>))

import qualified Control.Monad.Except as Except
import qualified Data.Text.IO         as Text.IO
import qualified Grace.Infer          as Infer
import qualified Grace.Normalize      as Normalize
import qualified Grace.Parser         as Parser
import qualified System.FilePath      as FilePath

{-| Input to the `interpret` function

    You should prefer to use `Path` if possible (for better error messages and
    correctly handling transitive imports).  The `Code` constructor is intended
    for cases like interpreting code read from standard input.
-}
data Input
    = Path FilePath
    -- ^ The path to the code
    | Code Text
    -- ^ Source code

{-| Interpret Grace source code, return the inferred type and the evaluated
    result

    This is the top-level function for the Grace interpreter
-}
interpret :: (MonadError Text m, MonadIO m) => Input -> m (Type Location, Value)
interpret input = do
    code <- case input of
        Path file -> liftIO (Text.IO.readFile file)
        Code text -> return text

    let name = case input of
            Path file -> file
            Code _    -> "(input)"

    expression <- case Parser.parse name code of
        Left message -> do
            Except.throwError message

        Right expression -> do
            let locate offset = Location{..}

            return (first locate expression)

    let resolve file = interpret (Path path)
          where
            path = case input of
                Path parent -> FilePath.takeDirectory parent </> file
                Code _      -> file

    resolvedExpression <- traverse resolve expression

    case Infer.typeOf resolvedExpression of
        Left message -> do
            Except.throwError message

        Right inferred -> do
            return (inferred, Normalize.evaluate [] resolvedExpression)
