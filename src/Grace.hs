module Grace where

import Grace.Syntax (Syntax)

import qualified Data.Text                             as Text
import qualified Data.Text.IO                          as Text.IO
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty.Text
import qualified Grace.Lexer
import qualified Grace.Normalize
import qualified Grace.Parser
import qualified Grace.Pretty
import qualified Grace.Type
import qualified System.Exit                           as Exit

pretty :: Syntax -> IO ()
pretty syntax = Pretty.Text.putDoc doc
  where
    doc =   Pretty.group (Grace.Pretty.expression syntax)
        <>  Pretty.hardline

main :: IO ()
main = do
    text <- Text.IO.getContents

    let tokens = Grace.Lexer.alexScanTokens (Text.unpack text)

    let expression = Grace.Parser.parseExpression tokens

    case Grace.Type.typeOf expression of
        Left string -> do
            putStrLn string

            Exit.exitFailure
        Right inferredType -> do
            pretty inferredType

    pretty (Grace.Normalize.normalize expression)
