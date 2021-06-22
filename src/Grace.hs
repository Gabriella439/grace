{-| This module contains the top-level `main` function that parses, type-checks
    and evaluates an expression
-}
module Grace
    ( -- * Main
      main
    ) where

import Prettyprinter (Pretty(..))
import Grace.Interpret (Input(..))

import qualified Control.Monad.Except      as Except
import qualified Data.Text.IO              as Text.IO
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty.Text
import qualified Grace.Interpret           as Interpret
import qualified Grace.Normalize           as Normalize
import qualified System.Exit               as Exit
import qualified System.IO                 as IO

pretty_ :: Pretty a => a -> IO ()
pretty_ x = Pretty.Text.putDoc (Pretty.pretty x <> Pretty.hardline)

-- | Command-line entrypoint
main :: IO ()
main = do
    text <- Text.IO.getContents

    eitherResult <- Except.runExceptT (Interpret.interpret (Code text))

    (inferred, value) <- case eitherResult of
        Left message -> do
            Text.IO.hPutStrLn IO.stderr message

            Exit.exitFailure
        Right result -> do
            return result

    pretty_ inferred

    pretty_ (Normalize.quote [] value)
