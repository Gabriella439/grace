-- | This module contains the implementation of the @grace repl@ subcommand
{-# LANGUAGE OverloadedStrings #-}

module Grace.Repl
    ( -- * REPL
      repl
    ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State (evalStateT, get, modify, StateT)
import Data.Text (pack, Text)
import Grace.Interpret (Input(..))
import Grace.Location (Location)
import Grace.Type (Type)
import Grace.Value (Value)
import System.Console.Repline (ReplOpts(..))

import qualified Control.Monad.Except         as Except
import qualified Data.Text.IO                 as Text.IO
import qualified Grace.Interpret              as Interpret
import qualified Grace.Normalize              as Normalize
import qualified Grace.Pretty                 as Grace.Pretty
import qualified System.Console.Repline       as Repline
import qualified System.IO                    as IO

repl :: IO ()
repl = evalStateT action []
  where
    action =
      Repline.evalReplOpts ReplOpts
        { banner = pure (pure ">>> ")
        , command = interpret
        , options = [("let", assignment)]
        , prefix = Just ':'
        , multilineCommand = Nothing
        , tabComplete = Repline.File
        , initialiser = return ()
        , finaliser = return Repline.Exit
        }


type Status = [(Text, Type Location, Value)]

interpret :: MonadIO m => String -> Repline.HaskelineT (StateT Status m) ()
interpret string = do
    let input = Code (pack string)

    context <- get
    eitherResult <- Except.runExceptT (Interpret.interpretWith context Nothing input)

    case eitherResult of
        Left text -> liftIO $ Text.IO.hPutStrLn IO.stderr text
        Right (_inferred, value) -> do
            let syntax = Normalize.quote [] value

            width <- liftIO $ Grace.Pretty.getWidth
            liftIO $ Grace.Pretty.renderIO True width IO.stdout (Grace.Pretty.pretty syntax <> "\n")

assignment :: MonadIO m => String -> Repline.HaskelineT (StateT Status m) ()
assignment input
    | (var, '=' : expr) <- break (== '=') input
    = do
      let exprc = Code (pack expr)
          variable = pack var

      context <- get
      eitherResult <- Except.runExceptT (Interpret.interpretWith context Nothing exprc)

      case eitherResult of
          Left text -> liftIO (Text.IO.hPutStrLn IO.stderr text)
          Right (type', value) -> modify ((variable, type', value) :)

    | otherwise
    = liftIO (putStrLn "usage: let = {expression}")
