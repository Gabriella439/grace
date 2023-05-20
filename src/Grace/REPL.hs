{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module contains the implementation of the @grace repl@ subcommand

module Grace.REPL
    ( -- * REPL
      repl
    ) where

import Control.Applicative (empty)
import Control.Exception.Safe (displayException, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState(..))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Grace.Interpret (Input(..))
import Grace.Lexer (reserved)
import System.Console.Haskeline (Interrupt(..))
import System.Console.Repline (CompleterStyle(..), MultiLine(..), ReplOpts(..))

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State as State
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Grace.HTTP as HTTP
import qualified Grace.Interpret as Interpret
import qualified Grace.Normalize as Normalize
import qualified Grace.Pretty as Pretty
import qualified Grace.Type as Type
import qualified Grace.Width as Width
import qualified System.Console.Haskeline.Completion as Completion
import qualified System.Console.Repline as Repline
import qualified System.IO as IO

-- | Entrypoint for the @grace repl@ subcommand
repl :: IO ()
repl = do
    manager <- HTTP.newManager

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

                    width <- liftIO Width.getWidth

                    liftIO (Pretty.renderIO True width IO.stdout (Pretty.pretty syntax <> "\n"))

    let help _string = do
            liftIO (putStrLn
                "Type any expression to normalize it or use one of the following commands:\n\
                \:help\n\
                \    Print help text and describe options\n\
                \:paste\n\
                \    Start a multi-line input. Submit with <Ctrl-D>\n\
                \:type EXPRESSION\n\
                \    Infer the type of an expression\n\
                \:let IDENTIFIER = EXPRESSION\n\
                \    Assign an expression to a variable\n\
                \:quit\n\
                \    Exit the REPL")

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
                    width <- liftIO Width.getWidth

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

    let tabComplete = Prefix complete [ (":", completeCommands) ]
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

                                    case type_ of
                                        Type.Record{ fields = Type.Fields keyTypes _ } -> do
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
