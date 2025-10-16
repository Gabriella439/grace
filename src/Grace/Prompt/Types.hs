-- | Types associated with prompting LLMs
module Grace.Prompt.Types
    ( -- * Types
      Prompt(..)
    , Effort(..)
    ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Grace.Decode (FromGrace, Key(..))

-- | Arguments to the @prompt@ keyword
data Prompt = Prompt
    { key :: Grace.Decode.Key
    , text :: Maybe Text
    , model :: Maybe Text
    , search :: Maybe Bool
    , effort :: Maybe Effort
    } deriving stock (Generic)
      deriving anyclass (FromGrace)

-- | The amount of effort a reasoning model puts into reasoning
data Effort = Low | Medium | High
    deriving stock (Generic)
    deriving anyclass (FromGrace)
