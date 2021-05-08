-- | This module implements support for file-based imports
module Grace.Interpret
    ( -- * Interpret
      Input(..)
    , interpret
    ) where

import Data.Text (Text)
import Grace.Type (Type)
import Grace.Value (Value)
import System.FilePath ((</>))

import qualified Data.Text.IO    as Text.IO
import qualified Grace.Infer     as Infer
import qualified Grace.Normalize as Normalize
import qualified Grace.Parser    as Parser
import qualified System.Exit     as Exit
import qualified System.FilePath as FilePath
import qualified System.IO       as IO

{-| Input to the `interpret` function

    You should prefer to use `Path` if possible (for better error messages and
    correctly handling transitive imports).  The `Code` constructor is intended
    for cases like interpreting code read from standard input.
-}
data Input
    = Path FilePath
    -- ^ The path to the code
    | Code Text
    -- ^ Source code

{-| Interpret Grace source code, return the inferred type and the evaluated
    result

    This is the top-level function for the Grace interpreter
-}
interpret :: Input -> IO (Type, Value)
interpret  input = do
    text <- case input of
        Path file -> Text.IO.readFile file
        Code text -> return text

    let name = case input of
            Path file -> file
            Code _    -> "(input)"

    expression <- case Parser.parse name text of
        Left message -> do
            Text.IO.hPutStrLn IO.stderr message
            Exit.exitFailure
        Right expression -> do
            return expression

    let here = case input of
            Path file -> file
            Code _    -> "./."

    let resolve file = interpret (Path (FilePath.takeDirectory here </> file))

    resolvedExpression <- traverse resolve expression

    case Infer.typeOf resolvedExpression of
        Left message -> do
            Text.IO.hPutStrLn IO.stderr message
            Exit.exitFailure
        Right inferred -> do
            return (inferred, Normalize.evaluate [] resolvedExpression)
