-- | This module contains the implementation of the @grace repl@ subcommand
{-# LANGUAGE OverloadedStrings #-}

module Grace.Repl
    ( -- * REPL
      repl
    ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text (pack)
import Grace.Interpret (Input(..))
import System.Console.Repline (ReplOpts(..))

import qualified Control.Monad.Except         as Except
import qualified Data.Text.IO                 as Text.IO
import qualified Grace.Interpret              as Interpret
import qualified Grace.Normalize              as Normalize
import qualified Grace.Pretty                 as Grace.Pretty
import qualified System.Console.Repline       as Repline
import qualified System.IO                    as IO

repl :: IO ()
repl =
  Repline.evalReplOpts ReplOpts
      { banner = pure (pure ">>> ")
      , command = interpret
      , options = []
      , prefix = Just ':'
      , multilineCommand = Nothing
      , tabComplete = Repline.File
      , initialiser = return ()
      , finaliser = return Repline.Exit
      }

interpret :: MonadIO m => String -> Repline.HaskelineT m ()
interpret string = liftIO $ do
    let input = Code (pack string)

    eitherResult <- do
        Except.runExceptT (Interpret.interpret Nothing input)

    case eitherResult of
        Left text -> Text.IO.hPutStrLn IO.stderr text
        Right (_inferred, value) -> do
            let syntax = Normalize.quote [] value

            width <- Grace.Pretty.getWidth
            Grace.Pretty.renderIO True width IO.stdout (Grace.Pretty.pretty syntax <> "\n")
