{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- | This module contains the implementation of the @grace repl@ subcommand

module Grace.Repl
    ( -- * REPL
      repl
    , replWith
    ) where

import Control.Exception.Safe (MonadThrow, displayException)
import Control.Monad.IO.Class (MonadIO, liftIO)
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
repl = replWith Interpret.defaultInterpretSettings

-- | Entrypoint for the @grace repl@ subcommand
replWith :: Interpret.InterpretSettings -> IO ()
replWith settings = State.evalStateT action []
  where
    action =
      Repline.evalReplOpts ReplOpts
        { banner = prompt
        , command = interpret settings
        , options = commands settings
        , prefix = Just ':'
        , multilineCommand = Just "paste"
        , tabComplete = complete settings
        , initialiser = return ()
        , finaliser = return Repline.Exit
        }

prompt :: Monad m => MultiLine -> m String
prompt MultiLine  = return "... "
prompt SingleLine = return ">>> "

type Status = [(Text, Type Location, Value)]

commands :: (MonadIO m, MonadState Status m, MonadThrow m) => Interpret.InterpretSettings -> Options m
commands settings =
    [ ("let", assignment settings)
    , ("type", infer settings)
    ]

interpret :: (MonadIO m, MonadState Status m, MonadThrow m) => Interpret.InterpretSettings -> Cmd m
interpret settings string = do
    let input = Code "(input)" (pack string)

    context <- get
    eitherResult <- Except.runExceptT (Interpret.interpretWith settings context Nothing input)

    case eitherResult of
        Left e -> liftIO (Text.IO.hPutStrLn IO.stderr (pack (displayException e)))
        Right (_inferred, value) -> do
            let syntax = Normalize.quote [] value

            width <- liftIO Grace.Pretty.getWidth
            liftIO (Grace.Pretty.renderIO True width IO.stdout (Grace.Pretty.pretty syntax <> "\n"))

assignment :: (MonadIO m, MonadState Status m, MonadThrow m) => Interpret.InterpretSettings -> Cmd m
assignment settings string
    | (var, '=' : expr) <- break (== '=') string
    = do
      let input = Code "(input)" (pack expr)

      let variable = strip (pack var)

      context <- get

      eitherResult <- do
          Except.runExceptT (Interpret.interpretWith settings context Nothing input)

      case eitherResult of
          Left e -> liftIO (Text.IO.hPutStrLn IO.stderr (pack (displayException e)))
          Right (type_, value) -> State.modify ((variable, type_, value) :)

    | otherwise
    = liftIO (putStrLn "usage: let = {expression}")

infer :: (MonadIO m, MonadState Status m, MonadThrow m) => Interpret.InterpretSettings -> Cmd m
infer settings expr = do
    let input = Code "(input)" (pack expr)

    context <- get

    eitherResult <- do
        Except.runExceptT (Interpret.interpretWith settings context Nothing input)

    case eitherResult of
        Left e -> do
            liftIO (Text.IO.hPutStrLn IO.stderr (pack (displayException e)))

        Right (type_, _) -> do
            width <- liftIO Grace.Pretty.getWidth

            liftIO (Grace.Pretty.renderIO True width IO.stdout (Grace.Pretty.pretty type_ <> "\n"))

complete :: Monad m => Interpret.InterpretSettings -> CompleterStyle m
complete settings =
    Custom (Repline.runMatcher [ (":", completeCommands) ] completeIdentifiers)
  where
    completeCommands =
        Repline.listCompleter (fmap adapt (commands @(StateT Status IO) settings))
      where
        adapt (c, _) = ":" <> c

    completeIdentifiers = Repline.listCompleter (fmap unpack (toList reserved))
