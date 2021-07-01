{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}

{-| This module contains the top-level `main` function that parses, type-checks
    and evaluates an expression
-}
module Grace
    ( -- * Main
      main
    ) where

import Prettyprinter (Pretty(..))
import Grace.Interpret (Input(..))
import Grace.Syntax (Syntax(..))
import Options.Applicative (Parser, ParserInfo)

import qualified Control.Monad.Except      as Except
import qualified Data.Text.IO              as Text.IO
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty.Text
import qualified Grace.Interpret           as Interpret
import qualified Grace.Normalize           as Normalize
import qualified Grace.Syntax              as Syntax
import qualified Options.Applicative       as Options
import qualified System.Exit               as Exit
import qualified System.IO                 as IO

data Options = Options
    { annotate :: Bool
    }

parserInfo :: ParserInfo Options
parserInfo =
    Options.info (Options.helper <*> parser)
        (Options.progDesc "Interpreter for the Grace language")

parser :: Parser Options
parser = do
    annotate <- Options.switch 
        (   Options.long "annotate"
        <>  Options.help "Add a type annotation for the inferred type"
        )

    return Options{..}

pretty_ :: Pretty a => a -> IO ()
pretty_ x = Pretty.Text.putDoc (Pretty.pretty x <> Pretty.hardline)

-- | Command-line entrypoint
main :: IO ()
main = do
    Options{..} <- Options.execParser parserInfo

    text <- Text.IO.getContents

    eitherResult <- Except.runExceptT (Interpret.interpret (Code text))

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
                    { node = Syntax.Annotation syntax (fmap (\_ -> ()) inferred)
                    , location = ()
                    }
            | otherwise =
                syntax

    pretty_ annotatedExpression
