{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE RecordWildCards #-}

{-| This module contains the top-level `main` function that parses, type-checks
    and evaluates an expression
-}
module Grace
    ( -- * Main
      main
    ) where

import Control.Applicative (many, (<|>))
import Data.Foldable (traverse_)
import Grace.Interpret (Input(..))
import Grace.Syntax (Syntax(..))
import Options.Applicative (Parser, ParserInfo)
import System.Console.Terminal.Size (Window(..))

import qualified Control.Monad.Except         as Except
import qualified Data.Text.IO                 as Text.IO
import qualified Prettyprinter                as Pretty
import qualified Grace.Interpret              as Interpret
import qualified Grace.Normalize              as Normalize
import qualified Grace.Parser                 as Parser
import qualified Grace.Pretty
import qualified Grace.Syntax                 as Syntax
import qualified Options.Applicative          as Options
import qualified System.Console.ANSI          as ANSI
import qualified System.Console.Terminal.Size as Size
import qualified System.Exit                  as Exit
import qualified System.IO                    as IO

data Highlight = Color | Plain | Auto

data Options
    = Interpret { annotate :: Bool, highlight :: Highlight, file :: FilePath }
    | Format { highlight :: Highlight, files :: [FilePath] }

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

    Options.hsubparser
        (   Options.command "format"
                (Options.info format
                    (Options.progDesc "Format Grace code")
                )

        <>  Options.command "interpret"
                (Options.info interpret
                    (Options.progDesc "Interpret a Grace file")
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
                                Syntax.Annotation syntax
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
            color <- detectColor highlight

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
                                    color
                                    Grace.Pretty.defaultColumns
                                    handle
                                    (Grace.Pretty.pretty syntax <> Pretty.hardline)

                    traverse_ formatFile files
