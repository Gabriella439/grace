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
import Grace.Import (ImportCallback)
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
import qualified Grace.Import           as Import
import qualified Grace.Interpret        as Interpret
import qualified Grace.Normalize        as Normalize
import qualified Grace.Pretty           as Grace.Pretty
import qualified Grace.Resolver.Builtin as Resolver
import qualified System.Console.Repline as Repline
import qualified System.IO              as IO

-- | Entrypoint for the @grace repl@ subcommand
repl :: IO ()
repl = replWith (Import.resolverToCallback Resolver.defaultResolver)

-- | Entrypoint for the @grace repl@ subcommand
replWith :: ImportCallback -> IO ()
replWith importCb = State.evalStateT action []
  where
    action =
      Repline.evalReplOpts ReplOpts
        { banner = prompt
        , command = interpret importCb
        , options = commands importCb
        , prefix = Just ':'
        , multilineCommand = Just "paste"
        , tabComplete = complete importCb
        , initialiser = return ()
        , finaliser = return Repline.Exit
        }

prompt :: Monad m => MultiLine -> m String
prompt MultiLine  = return "... "
prompt SingleLine = return ">>> "

type Status = [(Text, Type Location, Value)]

commands :: (MonadIO m, MonadState Status m, MonadThrow m) => ImportCallback -> Options m
commands importCb =
    [ ("let", assignment importCb)
    , ("type", infer importCb)
    ]

interpret :: (MonadIO m, MonadState Status m, MonadThrow m) => ImportCallback -> Cmd m
interpret importCb string = do
    let input = Code "(input)" (pack string)

    context <- get
    eitherResult <- Except.runExceptT (Interpret.interpretWith importCb context Nothing input)

    case eitherResult of
        Left e -> liftIO (Text.IO.hPutStrLn IO.stderr (pack (displayException e)))
        Right (_inferred, value) -> do
            let syntax = Normalize.quote [] value

            width <- liftIO Grace.Pretty.getWidth
            liftIO (Grace.Pretty.renderIO True width IO.stdout (Grace.Pretty.pretty syntax <> "\n"))

assignment :: (MonadIO m, MonadState Status m, MonadThrow m) => ImportCallback -> Cmd m
assignment importCb string
    | (var, '=' : expr) <- break (== '=') string
    = do
      let input = Code "(input)" (pack expr)

      let variable = strip (pack var)

      context <- get

      eitherResult <- do
          Except.runExceptT (Interpret.interpretWith importCb context Nothing input)

      case eitherResult of
          Left e -> liftIO (Text.IO.hPutStrLn IO.stderr (pack (displayException e)))
          Right (type_, value) -> State.modify ((variable, type_, value) :)

    | otherwise
    = liftIO (putStrLn "usage: let = {expression}")

infer :: (MonadIO m, MonadState Status m, MonadThrow m) => ImportCallback -> Cmd m
infer importCb expr = do
    let input = Code "(input)" (pack expr)

    context <- get

    eitherResult <- do
        Except.runExceptT (Interpret.interpretWith importCb context Nothing input)

    case eitherResult of
        Left e -> do
            liftIO (Text.IO.hPutStrLn IO.stderr (pack (displayException e)))

        Right (type_, _) -> do
            width <- liftIO Grace.Pretty.getWidth

            liftIO (Grace.Pretty.renderIO True width IO.stdout (Grace.Pretty.pretty type_ <> "\n"))

complete :: Monad m => ImportCallback -> CompleterStyle m
complete importCb =
    Custom (Repline.runMatcher [ (":", completeCommands) ] completeIdentifiers)
  where
    completeCommands =
        Repline.listCompleter (fmap adapt (commands @(StateT Status IO) importCb))
      where
        adapt (c, _) = ":" <> c

    completeIdentifiers = Repline.listCompleter (fmap unpack (toList reserved))
