-- | This module contains the implementation of the @grace repl@ subcommand
{-# LANGUAGE OverloadedStrings #-}

module Grace.Repl
    ( -- * REPL
      repl
    ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text (pack)
import Grace.Interpret (Input(..))

import qualified Control.Monad.Except         as Except
import qualified Data.Text.IO                 as Text.IO
import qualified Grace.Interpret              as Interpret
import qualified Grace.Normalize              as Normalize
import qualified Grace.Pretty                 as Pretty
import qualified System.Console.Repline       as Repline
import qualified System.IO                    as IO

repl :: IO ()
repl =
  Repline.evalRepl
    (pure (pure ">>> "))
    interpret
    []
    (Just ':')
    Nothing
    Repline.File
    (return ())
    (return Repline.Exit)

interpret :: MonadIO m => String -> Repline.HaskelineT m ()
interpret string = liftIO $ do
    let code = pack string
        input = Code code

    eitherResult <- do
        Except.runExceptT (Interpret.interpret Nothing input)

    case eitherResult of
       Left text -> Text.IO.hPutStrLn IO.stderr text
       Right (_inferred, value) -> do
        let syntax = Normalize.quote [] value

        Pretty.renderIO True 80 IO.stdout (Pretty.pretty syntax <> "\n")
