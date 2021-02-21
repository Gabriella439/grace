{-| This module contains the top-level `main` function that parses, type-checks
    and evaluates an expression
-}
module Grace
    ( -- * Main
      main
    ) where

import Grace.Syntax (Syntax)

import qualified Data.ByteString.Lazy                  as ByteString.Lazy
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty.Text
import qualified Grace.Lexer
import qualified Grace.Normalize
import qualified Grace.Parser
import qualified Grace.Pretty
import qualified System.Exit                           as Exit

pretty :: Syntax -> IO ()
pretty syntax = Pretty.Text.putDoc doc
  where
    doc =   Pretty.group (Grace.Pretty.prettyExpression syntax)
        <>  Pretty.hardline

-- | Command-line entrypoint
main :: IO ()
main = do
    bytes <- ByteString.Lazy.getContents

    expression <- case Grace.Lexer.runAlex bytes Grace.Parser.parseExpression of
        Left string -> do
            putStr string

            Exit.exitFailure
        Right expression -> do
            return expression

    pretty (Grace.Normalize.normalize expression)
