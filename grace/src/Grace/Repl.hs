{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module contains the implementation of the @grace repl@ subcommand

module Grace.Repl
    ( -- * REPL
      repl
    ) where

import Control.Exception.Safe (displayException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState(..))
import Data.Foldable (toList)
import Data.Text (pack, strip, unpack)
import Grace.Interpret (Input(..))
import Grace.Lexer (reserved)

import System.Console.Repline
    ( CompleterStyle(..)
    , MultiLine(..)
    , ReplOpts(..)
    )

import qualified Control.Monad.Except as Except
import qualified Control.Monad.State as State
import qualified Data.Text.IO as Text.IO
import qualified Grace.Interpret as Interpret
import qualified Grace.Normalize as Normalize
import qualified Grace.Pretty as Pretty
import qualified Network.HTTP.Client.TLS as TLS
import qualified System.Console.Repline as Repline
import qualified System.IO as IO

-- | Entrypoint for the @grace repl@ subcommand
repl :: IO ()
repl = do
    manager <- TLS.newTlsManager

    let interpret input = do
            context <- get

            Except.runExceptT (Interpret.interpretWith context Nothing manager input)

    let err e =
            liftIO (Text.IO.hPutStrLn IO.stderr (pack (displayException e)))

    let command string = do
            let input = Code "(input)" (pack string)

            eitherResult <- interpret input

            case eitherResult of
                Left e -> do
                    err e

                Right (_inferred, value) -> do
                    let syntax = Normalize.quote [] value

                    width <- liftIO Pretty.getWidth

                    liftIO (Pretty.renderIO True width IO.stdout (Pretty.pretty syntax <> "\n"))

    let assignment string
            | (var, '=' : expr) <- break (== '=') string = do
                let input = Code "(input)" (pack expr)

                let variable = strip (pack var)

                eitherResult <- interpret input

                case eitherResult of
                    Left e -> do
                        err e

                    Right (type_, value) -> do
                        State.modify ((variable, type_, value) :)

            | otherwise = do
                liftIO (putStrLn "usage: let = {expression}")

    let infer expr = do
            let input = Code "(input)" (pack expr)

            eitherResult <- interpret input

            case eitherResult of
                Left e -> do
                    err e

                Right (type_, _) -> do
                    width <- liftIO Pretty.getWidth

                    liftIO (Pretty.renderIO True width IO.stdout (Pretty.pretty type_ <> "\n"))

    let options =
            [ ("let", Repline.dontCrash . assignment)
            , ("type", Repline.dontCrash . infer)
            ]

    let tabComplete =
            Custom (Repline.runMatcher [ (":", completeCommands) ] completeIdentifiers)
          where
            completeCommands =
                Repline.listCompleter (fmap adapt options)
              where
                adapt (c, _) = ":" <> c

            completeIdentifiers args = do
                context <- get

                let completions =
                            toList reserved
                        <>  fmap (\(name, _, _) -> name) context

                Repline.listCompleter (fmap unpack completions) args

    let banner MultiLine  = return "... "
        banner SingleLine = return ">>> "

    let prefix = Just ':'

    let multilineCommand = Just "paste"

    let initialiser = return ()

    let finaliser = return Repline.Exit

    let action = Repline.evalReplOpts ReplOpts{..}

    State.evalStateT action []
