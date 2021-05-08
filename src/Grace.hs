{-| This module contains the top-level `main` function that parses, type-checks
    and evaluates an expression
-}
module Grace
    ( -- * Main
      main
    ) where

import Data.Text.Prettyprint.Doc (Pretty(..))
import Grace.Interpret (Input(..))

import qualified Data.Text.IO                          as Text.IO
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty.Text
import qualified Grace.Interpret                       as Interpret
import qualified Grace.Normalize                       as Normalize

pretty_ :: Pretty a => a -> IO ()
pretty_ x = Pretty.Text.putDoc (Pretty.pretty x <> Pretty.hardline)

-- | Command-line entrypoint
main :: IO ()
main = do
    text <- Text.IO.getContents

    (inferred, value) <- Interpret.interpret (Code text)

    pretty_ inferred

    pretty_ (Normalize.quote [] value)
