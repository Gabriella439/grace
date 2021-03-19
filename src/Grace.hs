{-| This module contains the top-level `main` function that parses, type-checks
    and evaluates an expression
-}
module Grace
    ( -- * Main
      main
    ) where

import Data.Text.Prettyprint.Doc (Pretty(..))

import qualified Data.ByteString.Lazy                  as ByteString.Lazy
import qualified Data.Text.IO                          as Text.IO
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty.Text
import qualified Grace.Import                          as Import
import qualified Grace.Infer                           as Infer
import qualified Grace.Lexer                           as Lexer
import qualified Grace.Normalize                       as Normalize
import qualified Grace.Parser                          as Parser
import qualified System.Exit                           as Exit

pretty_ :: Pretty a => a -> IO ()
pretty_ x = Pretty.Text.putDoc (Pretty.pretty x <> Pretty.hardline)

-- | Command-line entrypoint
main :: IO ()
main = do
    bytes <- ByteString.Lazy.getContents

    expression <- case Lexer.runAlex bytes Parser.parseExpression of
        Left string -> do
            putStrLn string

            Exit.exitFailure
        Right expression -> do
            return expression

    resolvedExpression <- Import.resolve "." expression

    case Infer.typeOf resolvedExpression of
        Left text -> do
            Text.IO.putStrLn text
        Right inferred -> do
            pretty_ inferred

            pretty_ (Normalize.normalize resolvedExpression)
