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
import qualified Grace.Infer
import qualified Grace.Lexer
import qualified Grace.Normalize
import qualified Grace.Parser
import qualified System.Exit                           as Exit

pretty_ :: Pretty a => a -> IO ()
pretty_ x = Pretty.Text.putDoc (Pretty.pretty x <> Pretty.hardline)

-- | Command-line entrypoint
main :: IO ()
main = do
    bytes <- ByteString.Lazy.getContents

    expression <- case Grace.Lexer.runAlex bytes Grace.Parser.parseExpression of
        Left string -> do
            putStrLn string

            Exit.exitFailure
        Right expression -> do
            return expression

    case Grace.Infer.typeOf expression of
        Left text -> do
            Text.IO.putStrLn text
        Right inferred -> do
            pretty_ inferred

            pretty_ (Grace.Normalize.normalize expression)
