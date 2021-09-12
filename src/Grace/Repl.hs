{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- | This module contains the implementation of the @grace repl@ subcommand

module Grace.Repl
    ( -- * REPL
      repl
    ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State (MonadState(..), StateT)
import Data.Foldable (toList)
import Data.Text (pack, strip, unpack, Text)
import Grace.Interpret (Input(..))
import Grace.Lexer (reserved)
import Grace.Location (Location)
import Grace.Type (Type)
import Grace.Value (Value)

import System.Console.Repline
    ( Cmd
    , CompleterStyle(..)
    , MultiLine(..)
    , Options
    , ReplOpts(..)
    )

import qualified Control.Monad.Except   as Except
import qualified Control.Monad.State    as State
import qualified Data.Text.IO           as Text.IO
import qualified Grace.Interpret        as Interpret
import qualified Grace.Normalize        as Normalize
import qualified Grace.Pretty           as Grace.Pretty
import qualified System.Console.Repline as Repline
import qualified System.IO              as IO

-- | Entrypoint for the @grace repl@ subcommand
repl :: IO ()
repl = State.evalStateT action []
  where
    action =
      Repline.evalReplOpts ReplOpts
        { banner = prompt
        , command = interpret
        , options = commands
        , prefix = Just ':'
        , multilineCommand = Just "paste"
        , tabComplete = complete
        , initialiser = return ()
        , finaliser = return Repline.Exit
        }

prompt :: Monad m => MultiLine -> m String
prompt MultiLine  = return "... "
prompt SingleLine = return ">>> "

type Status = [(Text, Type Location, Value)]

commands :: (MonadState Status m, MonadIO m) => Options m
commands = [ ("let", assignment), ("type", infer) ]

interpret :: (MonadState Status m, MonadIO m) => Cmd m
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

assignment :: (MonadState Status m, MonadIO m) => Cmd m
assignment string
    | (var, '=' : expr) <- break (== '=') string
    = do
      let input = Code (pack expr)

      let variable = strip (pack var)

      context <- get

      eitherResult <- do
          Except.runExceptT (Interpret.interpretWith context Nothing input)

      case eitherResult of
          Left text -> liftIO (Text.IO.hPutStrLn IO.stderr text)
          Right (type_, value) -> State.modify ((variable, type_, value) :)

    | otherwise
    = liftIO (putStrLn "usage: let = {expression}")

infer :: (MonadState Status m, MonadIO m) => Cmd m
infer expr = do
    let input = Code (pack expr)

    context <- get

    eitherResult <- do
        Except.runExceptT (Interpret.interpretWith context Nothing input)

    case eitherResult of
        Left text -> do
            liftIO (Text.IO.hPutStrLn IO.stderr text)

        Right (type_, _) -> do
            width <- liftIO $ Grace.Pretty.getWidth

            liftIO (Grace.Pretty.renderIO True width IO.stdout (Grace.Pretty.pretty type_ <> "\n"))

complete :: Monad m => CompleterStyle m
complete =
    Combine File
      (Custom (Repline.runMatcher [ (":", completeCommands) ] completeIdentifiers))
  where
    completeCommands =
        Repline.listCompleter (fmap adapt (commands @(StateT Status IO)))
      where
        adapt (c, _) = ":" <> c

    completeIdentifiers = Repline.listCompleter (fmap unpack (toList reserved))
