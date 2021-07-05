{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-| This module contains the top-level `main` function that parses, type-checks
    and evaluates an expression
-}
module Grace
    ( -- * Main
      main
    ) where

import Control.Applicative (many, (<|>))
import Data.Foldable (traverse_)
import Data.Void (Void)
import Grace.Interpret (Input(..))
import Grace.Location (Location(..))
import Grace.Syntax (Builtin(..), Node(..), Syntax(..))
import Options.Applicative (Parser, ParserInfo)
import System.Console.Terminal.Size (Window(..))

import qualified Control.Monad.Except         as Except
import qualified Data.Text.IO                 as Text.IO
import qualified Prettyprinter                as Pretty
import qualified Grace.Infer                  as Infer
import qualified Grace.Interpret              as Interpret
import qualified Grace.Normalize              as Normalize
import qualified Grace.Parser                 as Parser
import qualified Grace.Pretty
import qualified Options.Applicative          as Options
import qualified System.Console.ANSI          as ANSI
import qualified System.Console.Terminal.Size as Size
import qualified System.Exit                  as Exit
import qualified System.IO                    as IO

data Highlight = Color | Plain | Auto

data Options
    = Interpret { annotate :: Bool, highlight :: Highlight, file :: FilePath }
    | Format { highlight :: Highlight, files :: [FilePath] }
    | Builtins { highlight :: Highlight }

parserInfo :: ParserInfo Options
parserInfo =
    Options.info (Options.helper <*> parser)
        (Options.progDesc "Command-line utility for the Grace language")

parser :: Parser Options
parser = do
    let interpret = do
            annotate <- Options.switch 
                (   Options.long "annotate"
                <>  Options.help "Add a type annotation for the inferred type"
                )

            file <- Options.strArgument
                (   Options.help "File to interpret"
                <>  Options.metavar "FILE"
                )

            highlight <- parseHighlight

            return Interpret{..}

    let format = do
            let parseFile =
                    Options.strArgument
                        (   Options.help "File to format"
                        <>  Options.metavar "FILE"
                        )

            highlight <- parseHighlight

            files <- many parseFile

            return Format{..}

    let builtins = do
            highlight <- parseHighlight

            return Builtins{..}

    Options.hsubparser
        (   Options.command "format"
                (Options.info format
                    (Options.progDesc "Format Grace code")
                )

        <>  Options.command "interpret"
                (Options.info interpret
                    (Options.progDesc "Interpret a Grace file")
                )

        <>  Options.command "builtins"
                (Options.info builtins
                    (Options.progDesc "List all built-in functions and their types")
                )
        )
  where
    parseHighlight =
            Options.flag' Color
                (    Options.long "color"
                <>   Options.help "Enable syntax highlighting"
                )
        <|> Options.flag' Plain
                (    Options.long "plain"
                <>   Options.help "Disable syntax highlighting"
                )
        <|> pure Auto


detectColor :: Highlight -> IO Bool
detectColor Color = do
    return True
detectColor Plain = do
    return False
detectColor Auto = do
    ANSI.hSupportsANSI IO.stdout

-- | Command-line entrypoint
main :: IO ()
main = do
    options <- Options.execParser parserInfo

    case options of
        Interpret{..} -> do
            input <- case file of
                "-" -> do
                    text <- Text.IO.getContents

                    return (Code text)
                _ -> do
                    return (Path file)

            eitherResult <- do
                Except.runExceptT (Interpret.interpret Nothing input)

            (inferred, value) <- case eitherResult of
                Left message -> do
                    Text.IO.hPutStrLn IO.stderr message

                    Exit.exitFailure
                Right result -> do
                    return result

            let syntax = Normalize.quote [] value

            let annotatedExpression
                    | annotate =
                        Syntax
                            { node =
                                Annotation syntax
                                    (fmap (\_ -> ()) inferred)
                            , location = ()
                            }
                    | otherwise =
                        syntax

            maybeWindow <- Size.size

            let renderWidth =
                    case maybeWindow of
                        Nothing         -> Grace.Pretty.defaultColumns
                        Just Window{..} -> width

            color <- detectColor highlight

            Grace.Pretty.renderIO color renderWidth IO.stdout
                (Grace.Pretty.pretty annotatedExpression <> Pretty.hardline)

        Format{..} -> do
            case files of
                [ "-" ] -> do
                    text <- Text.IO.getContents

                    syntax <- case Parser.parse "(input)" text of
                        Left message -> do
                            Text.IO.hPutStrLn IO.stderr message

                            Exit.exitFailure
                        Right syntax -> do
                            return syntax

                    maybeWindow <- Size.size

                    let renderWidth =
                            case maybeWindow of
                                Nothing         -> Grace.Pretty.defaultColumns
                                Just Window{..} -> width

                    color <- detectColor highlight

                    Grace.Pretty.renderIO color renderWidth IO.stdout
                        (Grace.Pretty.pretty syntax <> Pretty.hardline)
                _ -> do
                    let formatFile file = do
                            text <- Text.IO.readFile file

                            syntax <- case Parser.parse file text of
                                Left message -> do
                                    Text.IO.hPutStrLn IO.stderr message

                                    Exit.exitFailure
                                Right syntax -> do
                                    return syntax

                            IO.withFile file IO.WriteMode \handle -> do
                                Grace.Pretty.renderIO
                                    False
                                    Grace.Pretty.defaultColumns
                                    handle
                                    (Grace.Pretty.pretty syntax <> Pretty.hardline)

                    traverse_ formatFile files

        Builtins{..} -> do
            let displayBuiltin :: Builtin -> IO ()
                displayBuiltin builtin = do
                    let expression =
                            Syntax
                                { location =
                                    Location
                                        { name = "(input)"
                                        , code =
                                            Grace.Pretty.renderStrict
                                                False
                                                Grace.Pretty.defaultColumns
                                                (Grace.Pretty.pretty builtin)
                                        , offset = 0
                                        }
                                , node = Builtin builtin
                                }

                    type_ <- case Infer.typeOf expression of
                        Left message -> do
                            Text.IO.hPutStrLn IO.stderr message

                            Exit.exitFailure
                        Right type_ -> do
                            return type_

                    let annotated :: Node Location Void
                        annotated = Annotation expression type_

                    maybeWindow <- Size.size

                    let renderWidth =
                            case maybeWindow of
                                Nothing         -> Grace.Pretty.defaultColumns
                                Just Window{..} -> width

                    color <- detectColor highlight

                    Grace.Pretty.renderIO
                        color
                        renderWidth
                        IO.stdout
                        (Grace.Pretty.pretty annotated <> Pretty.hardline)

            let builtins = [ minBound .. maxBound ]

            case builtins of
                [] -> do
                    return ()

                b0 : bs -> do
                    displayBuiltin b0

                    traverse_ (\b -> Text.IO.putStrLn "" >> displayBuiltin b) bs
