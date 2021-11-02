{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module contains the implementation of the @grace repl@ subcommand

module Grace.Repl
    ( -- * REPL
      repl
    ) where

import Control.Applicative (empty)
import Control.Exception.Safe (displayException, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState(..))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.String.Interpolate (__i)
import Grace.Interpret (Input(..))
import Grace.Lexer (reserved)
import System.Console.Haskeline (Interrupt(..))
import System.Console.Repline (CompleterStyle(..), MultiLine(..), ReplOpts(..))

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State as State
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Grace.Interpret as Interpret
import qualified Grace.Normalize as Normalize
import qualified Grace.Pretty as Pretty
import qualified Grace.Type as Type
import qualified Network.HTTP.Client.TLS as TLS
import qualified System.Console.Haskeline.Completion as Completion
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
            liftIO (Text.IO.hPutStrLn IO.stderr (Text.pack (displayException e)))

    let command string = do
            let input = Code "(input)" (Text.pack string)

            eitherResult <- interpret input

            case eitherResult of
                Left e -> do
                    err e

                Right (_inferred, value) -> do
                    let syntax = Normalize.quote [] value

                    width <- liftIO Pretty.getWidth

                    liftIO (Pretty.renderIO True width IO.stdout (Pretty.pretty syntax <> "\n"))

    let help _string = do
            liftIO (putStrLn [__i|
                Type any expression to normalize it or use one of the following commands:
                :help
                    Print help text and describe options
                :paste
                    Start a multi-line input. Submit with <Ctrl-D>
                :type EXPRESSION
                    Infer the type of an expression
                :let IDENTIFIER = EXPRESSION
                    Assign an expression to a variable
                :quit
                    Exit the REPL
            |])

    let assignment string
            | (var, '=' : expr) <- break (== '=') string = do
                let input = Code "(input)" (Text.pack expr)

                let variable = Text.strip (Text.pack var)

                eitherResult <- interpret input

                case eitherResult of
                    Left e -> do
                        err e

                    Right (type_, value) -> do
                        State.modify ((variable, type_, value) :)

            | otherwise = do
                liftIO (putStrLn "usage: let = {expression}")

    let infer expr = do
            let input = Code "(input)" (Text.pack expr)

            eitherResult <- interpret input

            case eitherResult of
                Left e -> do
                    err e

                Right (type_, _) -> do
                    width <- liftIO Pretty.getWidth

                    liftIO (Pretty.renderIO True width IO.stdout (Pretty.pretty type_ <> "\n"))

    let quit _ =
            liftIO (throwIO Interrupt)

    let options =
            [ ("help", Repline.dontCrash . help)
            , ("let", Repline.dontCrash . assignment)
            -- `paste` is included here for auto-completion purposes only.
            -- `repline`'s `multilineCommand` logic overrides this no-op.
            , ("paste", Repline.dontCrash . \_ -> return ())
            , ("quit", quit)
            , ("type", Repline.dontCrash . infer)
            ]

    let tabComplete =
            Custom (Repline.runMatcher [ (":", completeCommands) ] complete)
          where
            completeCommands =
                Repline.listCompleter (fmap adapt options)
              where
                adapt (c, _) = ":" <> c

            complete =
                foldr Repline.fallbackCompletion Completion.noCompletion
                    [ completeReserved
                    , completeIdentifiers
                    , completeFields
                    ]

            completeReserved =
                Repline.listCompleter (fmap Text.unpack (toList reserved))

            completeIdentifiers args = do
                context <- get

                let completions =
                        fmap (\(name, _, _) -> name) context

                Repline.listCompleter (fmap Text.unpack completions) args

            completeFields =
                Repline.wordCompleter \prefix -> do
                    let toNonEmpty (x : xs) =  x :| xs
                        toNonEmpty      []  = "" :| []

                    let loop (c0 :| c1 : cs) context = do
                            let newContext = do
                                    (name, type_) <- context

                                    Monad.guard (c0 == name)

                                    case Type.node type_ of
                                        Type.Record (Type.Fields keyTypes _) -> do
                                            keyTypes
                                        _ -> do
                                            empty

                            results <- loop (c1 :| cs) newContext

                            let prepend result = c0 <> "." <> result

                            return (fmap prepend results)
                        loop (c0 :| []) context = return do
                            (name, _) <- context

                            Monad.guard (Text.isPrefixOf c0 name)

                            return name

                    let startingComponents =
                            toNonEmpty (Text.splitOn "." (Text.pack prefix))

                    context <- get

                    let startingContext = do
                            (name, type_, _) <- context

                            return (name, type_)

                    results <- loop startingComponents startingContext

                    return (fmap Text.unpack results)

    let banner MultiLine  = return "... "
        banner SingleLine = return ">>> "

    let prefix = Just ':'

    let multilineCommand = Just "paste"

    let initialiser = return ()

    let finaliser = return Repline.Exit

    let action = Repline.evalReplOpts ReplOpts{..}

    State.evalStateT action []
