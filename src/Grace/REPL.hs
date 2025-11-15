{-# LANGUAGE FlexibleContexts #-}

-- | This module contains the implementation of the @grace repl@ subcommand

module Grace.REPL
    ( -- * REPL
      repl
    ) where

import Control.Applicative (empty)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (MonadState(..))
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Grace.HTTP (Methods)
import Grace.Interpret (Input(..))
import Grace.Location (Location(..))
import Grace.Monad (Status(..))
import Grace.Parser (REPLCommand(..))
import System.Console.Haskeline (Interrupt(..))
import System.Console.Repline (CompleterStyle(..), MultiLine(..), ReplOpts(..))

import Control.Exception.Safe
    (Exception, SomeException, displayException, throwIO)

import qualified Control.Exception.Safe as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.State as State
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Grace.Context as Context
import qualified Grace.Infer as Infer
import qualified Grace.Label as Label
import qualified Grace.Monad as Grace
import qualified Grace.Normalize as Normalize
import qualified Grace.Parser as Parser
import qualified Grace.Pretty as Pretty
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type
import qualified Grace.Value as Value
import qualified Grace.Width as Width
import qualified System.Console.Haskeline.Completion as Completion
import qualified System.Console.Repline as Repline
import qualified System.IO as IO

-- | Entrypoint for the @grace repl@ subcommand
repl :: (Text -> Methods) -> IO ()
repl keyToMethods = do
    let err :: (Exception e, MonadIO io) => e -> io ()
        err e =
            liftIO (Text.IO.hPutStrLn IO.stderr (Text.pack (displayException e)))

    let command infer string = do
            let text = Text.pack string

            let locate offset = Location
                    { name = "(input)"
                    , code = text
                    , offset
                    }

            let interpret syntax₀ = do
                    assignments₀ <- State.get

                    let syntax₁ = case reverse assignments₀ of
                            [] -> first locate syntax₀

                            assignment₁ : assignments₁ -> Syntax.Let
                              { location = Unknown
                              , assignments = assignment₁ :| assignments₁
                              , body = first locate syntax₀
                              }

                    let input = Code "(input)" text

                    let status = Status
                            { count = 0
                            , context = []
                            }

                    let action = do
                            (inferred, elaborated) <- Infer.infer syntax₁

                            value <- Normalize.evaluate keyToMethods [] elaborated

                            return (inferred, value)

                    result <- liftIO (Exception.try (Grace.runGrace input status action))

                    case result of
                        Left (e :: SomeException) -> do
                            return (Left e)

                        Right ((inferred, value), Status{ context }) -> do
                            let annotation = Context.complete context inferred

                            let annotated =
                                    Normalize.quote (Value.complete context value)

                            return (Right (annotation, annotated))

            case Parser.parseREPLCommand "(input)" text of
                Left parseError -> do
                    err parseError

                Right (Evaluate syntax₀) -> do
                    result <- interpret syntax₀

                    case result of
                        Left (e :: SomeException) -> do
                            err e

                        Right (annotation, annotated) -> do
                            width <- liftIO Width.getWidth

                            let document =
                                    if infer
                                    then Pretty.pretty annotation
                                    else Pretty.pretty annotated

                            liftIO (Pretty.renderIO True width IO.stdout (document <> "\n"))

                Right (Assign assignment) -> do
                    assignments <- State.get

                    State.put (first locate assignment : assignments)

                    result <- interpret Syntax.Record{ location = 0, fieldValues = []  }

                    case result of
                        Left e -> do
                            State.put assignments

                            err e

                        Right _ -> do
                            return ()

    let help _string = do
            liftIO (putStrLn
                "Type any expression to normalize it or use one of the following commands:\n\
                \:help\n\
                \    Print help text and describe options\n\
                \:paste\n\
                \    Start a multi-line input. Submit with <Ctrl-D>\n\
                \let IDENTIFIER = EXPRESSION\n\
                \    Assign an expression to a variable\n\
                \:quit\n\
                \    Exit the REPL")

    let quit _ =
            liftIO (throwIO Interrupt)

    let options =
            [ ("help", Repline.dontCrash . help)
            -- `paste` is included here for auto-completion purposes only.
            -- `repline`'s `multilineCommand` logic overrides this no-op.
            , ("paste", Repline.dontCrash . \_ -> return ())
            , ("quit", quit)
            , ("type", Repline.dontCrash . command True)
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
                Repline.listCompleter (fmap Text.unpack (toList Label.reservedLabels))

            completeIdentifiers args = do
                assignments <- get

                let completions = do
                        Syntax.Define{ definition = Syntax.Definition{ name } } <- assignments

                        return name

                Repline.listCompleter (fmap Text.unpack completions) args

            completeFields =
                Repline.wordCompleter \prefix -> do
                    let toNonEmpty (x : xs) =  x :| xs
                        toNonEmpty      []  = "" :| []

                    let loop (c0 :| c1 : cs) context = do
                            let newContext = do
                                    (name, annotation) <- context

                                    Monad.guard (c0 == name)

                                    case annotation of
                                        Just Type.Record{ fields = Type.Fields keyTypes _ } -> do
                                            (key, type_) <- keyTypes

                                            return (key, Just type_)
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

                    assignments <- get

                    let startingContext = do
                            Syntax.Define{ definition = Syntax.Definition{ name, annotation } } <- assignments

                            return (name, annotation)

                    results <- loop startingComponents startingContext

                    return (fmap Text.unpack results)

    let banner MultiLine  = return "... "
        banner SingleLine = return ">>> "

    let initialiser = liftIO (putStrLn "Type :help for more information.")

    let action = Repline.evalReplOpts ReplOpts
            { banner
            , command = command False
            , options
            , prefix = Just ':'
            , multilineCommand = Just "paste"
            , tabComplete
            , initialiser
            , finaliser = return Repline.Exit
            }

    State.evalStateT action []
