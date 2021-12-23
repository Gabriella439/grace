{-| This module provides a uniform interface for getting the console width using
    both GHC and GHCJS
-}
module Grace.Width
    ( -- * Width
      getWidth
    , defaultWidth
    ) where

-- | Get the width of the terminal (in columns)
getWidth :: IO Int
getWidth = pure defaultWidth

-- | The default column size to use
defaultWidth :: Int
defaultWidth = 80
