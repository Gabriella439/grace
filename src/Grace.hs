{-| This module contains the top-level `main` function that implements the
    command-line API
-}
module Grace
    ( -- * Main
      main
    ) where

import Control.Applicative (many, (<|>))
import Control.Exception.Safe (Exception(..), SomeException)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Void (Void)
import Grace.Input (Input(..), Mode(..))
import Grace.Location (Location(..))
import Grace.Monad (Status(..))
import Grace.Syntax (Builtin(..), Syntax(..))
import Grace.Type (Type(..))
import Options.Applicative (Parser, ParserInfo)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)

import qualified Control.Exception.Safe as Exception
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified GHC.IO.Encoding
import qualified Grace.Infer as Infer
import qualified Grace.Interpret as Interpret
import qualified Grace.Monad as Grace
import qualified Grace.Monotype as Monotype
import qualified Grace.Normalize as Normalize
import qualified Grace.Parser as Parser
import qualified Grace.Pretty
import qualified Grace.REPL as REPL
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type
import qualified Grace.Value as Value
import qualified Grace.Width as Width
import qualified Options.Applicative as Options
import qualified Prettyprinter as Pretty
import qualified System.Console.ANSI as ANSI
import qualified System.Exit as Exit
import qualified System.IO as IO

data Highlight
    = Color
    -- ^ Force the use of ANSI color escape sequences to highlight source code
    | Plain
    -- ^ Don't highlight source code
    | Auto
    -- ^ Auto-detect whether to highlight source code based on whether or not
    --   @stdout@ is a terminal

data Options
    = Interpret
        { annotate :: Bool
        , highlight :: Highlight
        , file :: FilePath
        }
    | Text { file :: FilePath }
    | Format { highlight :: Highlight, files :: [FilePath] }
    | Builtins { highlight :: Highlight }
    | REPL

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

            return Interpret{ annotate, file, highlight }

    let text = do
            file <- Options.strArgument
                (   Options.help "File to interpret"
                <>  Options.metavar "FILE"
                )

            return Grace.Text{ file }

    let format = do
            let parseFile =
                    Options.strArgument
                        (   Options.help "File to format"
                        <>  Options.metavar "FILE"
                        )

            highlight <- parseHighlight

            files <- many parseFile

            return Format{ highlight, files }

    let builtins = do
            highlight <- parseHighlight

            return Builtins{ highlight }

    let repl = do
            pure REPL{ }

    Options.hsubparser
        (   Options.command "interpret"
                (Options.info interpret
                    (Options.progDesc "Interpret a Grace file")
                )

        <>  Options.command "text"
                (Options.info text
                    (Options.progDesc "Render a Grace text literal")
                )

        <>  Options.command "format"
                (Options.info format
                    (Options.progDesc "Format Grace code")
                )

        <>  Options.command "builtins"
                (Options.info builtins
                    (Options.progDesc "List all built-in functions and their types")
                )
        <> Options.command "repl"
                (Options.info repl
                    (Options.progDesc "Enter a REPL for Grace")
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
detectColor Color = do return True
detectColor Plain = do return False
detectColor Auto  = do ANSI.hSupportsANSI IO.stdout

getRender :: Highlight -> IO (Doc AnsiStyle -> IO ())
getRender highlight = do
    color <- detectColor highlight

    width <- Width.getWidth

    return (Grace.Pretty.renderIO color width IO.stdout)

throws :: Exception e => Either e a -> IO a
throws (Left e) = do
    Text.IO.hPutStrLn IO.stderr (Text.pack (displayException e))
    Exit.exitFailure
throws (Right result) = do
    return result

-- | Command-line entrypoint
main :: IO ()
main = Exception.handle handler do
    GHC.IO.Encoding.setLocaleEncoding IO.utf8

    options <- Options.execParser parserInfo

    case options of
        Interpret{ annotate, highlight, file } -> do
            input <- case file of
                "-" -> do
                    Code "(input)" <$> Text.IO.getContents
                _ -> do
                    return (Path file AsCode)

            (inferred, value) <- Interpret.interpret input

            let syntax = Normalize.strip (Value.quote value)

            let annotatedExpression
                    | annotate = Annotation
                        { annotated = syntax
                        , annotation = void inferred
                        , location = ()
                        }
                    | otherwise =
                        syntax

            render <- getRender highlight

            render (Grace.Pretty.pretty annotatedExpression <> Pretty.hardline)

        Grace.Text{ file } -> do
            input <- case file of
                "-" -> do
                    Code "(input)" <$> Text.IO.getContents
                _ -> do
                    return (Path file AsCode)

            let location = Location
                    { name = "(input)"
                    , code = "… : Text"
                    , offset = 4
                    }

            let expected = Type.Scalar{ scalar = Monotype.Text, location }

            let initialStatus = Status{ count = 0, context = [] }

            (_, value) <- Grace.evalGrace input initialStatus (Interpret.interpretWith [] (Just expected))

            case value of
                Value.Text text -> Text.IO.putStr text
                _ -> do
                    Text.IO.hPutStrLn IO.stderr
                        "Internal error: Not a plain Text literal\n\
                        \\n\
                        \The input expression did not evaluate to a Text literal, even though it had the\n\
                        \correct type"
                    Exit.exitFailure

        Format{ highlight, files } -> do
            case files of
                [ "-" ] -> do
                    text <- Text.IO.getContents

                    syntax <- throws (Parser.parse "(input)" text)

                    render <- getRender highlight

                    render (Grace.Pretty.pretty syntax <> Pretty.hardline)
                _ -> do
                    let formatFile file = do
                            text <- Text.IO.readFile file

                            syntax <- throws (Parser.parse file text)

                            IO.withFile file IO.WriteMode \handle -> do
                                Grace.Pretty.renderIO
                                    False
                                    Width.defaultWidth
                                    handle
                                    (Grace.Pretty.pretty syntax <> Pretty.hardline)

                    traverse_ formatFile files

        Builtins{ highlight } -> do
            let displayBuiltin :: Builtin -> IO ()
                displayBuiltin builtin = do
                    let code =
                            Grace.Pretty.renderStrict
                                False
                                Width.defaultWidth
                                (Grace.Pretty.pretty builtin)

                    let expression = Syntax.Builtin
                            { location = Location
                                { name = "(input)", code, offset = 0 }
                            , builtin
                            }

                    let input = Code "(input)" code

                    (type_, _) <- Infer.typeOf input expression

                    let annotated :: Syntax Location Void
                        annotated = Annotation
                            { annotated = expression
                            , annotation = type_
                            , location = Syntax.location expression
                            }

                    render <- getRender highlight

                    render (Grace.Pretty.pretty annotated <> Pretty.hardline)

            let builtins = [ minBound .. maxBound ]

            case builtins of
                [] -> do
                    return ()

                b0 : bs -> do
                    displayBuiltin b0

                    traverse_ (\b -> Text.IO.putStrLn "" >> displayBuiltin b) bs

        REPL{ } -> do
            REPL.repl
  where
    handler :: SomeException -> IO a
    handler e = do
        Text.IO.hPutStrLn IO.stderr (Text.pack (displayException e))
        Exit.exitFailure
