-- | This module contains the implementation of the @grace repl@ subcommand
{-# LANGUAGE OverloadedStrings #-}

module Grace.Repl
    ( -- * REPL
      repl
    ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State (evalStateT, modify, StateT)
import Data.Text (pack, Text)
import Grace.Interpret (Input(..))
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


type Status = [(Text, Value)]

interpret :: MonadIO m => String -> Repline.HaskelineT (StateT Status m) ()
interpret string = liftIO $ do
    let input = Code (pack string)

    eitherResult <- Except.runExceptT (Interpret.interpret Nothing input)

    case eitherResult of
        Left text -> Text.IO.hPutStrLn IO.stderr text
        Right (_inferred, value) -> do
            let syntax = Normalize.quote [] value

            width <- Grace.Pretty.getWidth
            Grace.Pretty.renderIO True width IO.stdout (Grace.Pretty.pretty syntax <> "\n")

assignment :: MonadIO m => String -> Repline.HaskelineT (StateT Status m) ()
assignment input
    | (var, '=' : expr) <- break (== '=') input
    = do
      let exprc = Code (pack expr)
          variable = pack var

      eitherResult <- Except.runExceptT (Interpret.interpret Nothing exprc)

      case eitherResult of
          Left text -> liftIO (Text.IO.hPutStrLn IO.stderr text)
          Right (_, value) -> modify ((variable, value) :)

    | otherwise
    = liftIO (putStrLn "usage: let = {expression}")
