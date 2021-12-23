{-# LANGUAGE RecordWildCards #-}

{-| This module provides a uniform interface for getting the console width using
    both GHC and GHCJS
-}
module Grace.Width
    ( -- * Width
      getWidth
    , defaultWidth
    ) where

import System.Console.Terminal.Size (Window(..))

import qualified System.Console.Terminal.Size as Size

-- | Get the width of the terminal (in columns)
getWidth :: IO Int
getWidth = do
    maybeWindow <- Size.size

    let renderWidth =
            case maybeWindow of
                Nothing         -> defaultWidth
                Just Window{..} -> width

    return renderWidth

-- | The default width to use
defaultWidth :: Int
defaultWidth = 80
