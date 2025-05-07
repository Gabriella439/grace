{-| This module provides a uniform interface for accessing data files using
    both GHC and GHCJS
-}
module Grace.DataFile
    ( readDataFile
    ) where

import Data.Text (Text)

import qualified Data.Text as Text
import qualified Grace.HTTP as HTTP

readDataFile :: FilePath -> IO Text
readDataFile relativePath = do
    HTTP.fetch () (Text.pack relativePath)
