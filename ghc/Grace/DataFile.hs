{-| This module provides a uniform interface for accessing data files using
    both GHC and GHCJS
-}
module Grace.DataFile
    ( readDataFile
    ) where

import Data.Text (Text)

import qualified Paths_grace as Paths
import qualified Data.Text.IO as Text.IO

-- | Read a data file by its relative path
readDataFile :: FilePath -> IO Text
readDataFile relativePath = do
    absolutePath <- Paths.getDataFileName relativePath

    Text.IO.readFile absolutePath
