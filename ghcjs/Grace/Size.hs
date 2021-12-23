module Grace.Width
    ( -- * Width
      getWidth
    ) where

-- | Get the width of the terminal (in columns)
getWidth :: IO Int
getWidth = pure 80
