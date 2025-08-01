{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Internal module shared between "Grace.Decode" and "Grace.Encode"
module Grace.Marshal
    ( Key(..)
    , selector
    ) where

import Control.Monad.State (State)
import Data.Text (Text)
import Data.String (IsString)
import GHC.Generics (M1, S, Selector)

import qualified Control.Monad.State as State
import qualified Data.Text as Text
import qualified GHC.Generics as Generics

-- | A protected `Text` value
newtype Key = Key{ text :: Text }
    deriving newtype (IsString, Show)

selector :: Selector s => M1 S s f r -> State Int Text
selector m1 = do
    let name₀ = Generics.selName m1

    if name₀ == ""
        then do
            n <- State.get

            State.put $! n + 1

            return (Text.pack (show n))
        else do
            return (Text.pack name₀)
