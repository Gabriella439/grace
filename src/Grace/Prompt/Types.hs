-- | Types associated with prompting LLMs
module Grace.Prompt.Types
    ( -- * Types
      Prompt(..)
    , Message(..)
    , Effort(..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Grace.Decode (FromGrace, Key(..), ToGraceType)
import Grace.Encode (ToGrace)

-- | Arguments to the @prompt@ keyword
data Prompt = Prompt
    { key :: Grace.Decode.Key
    , text :: Maybe Text
    , history :: Maybe [Message]
    , model :: Maybe Text
    , search :: Maybe Bool
    , effort :: Maybe Effort
    } deriving stock (Generic)
      deriving anyclass (FromGrace, FromJSON, ToGrace, ToGraceType, ToJSON)

-- | A message added to the conversation history
data Message
    = System{ name :: Maybe Text, text :: Text }
    | User{ name :: Maybe Text, text :: Text }
    | Assistant{ name :: Maybe Text, text :: Text }
    deriving stock (Generic)
    deriving anyclass (FromGrace, FromJSON, ToGrace, ToGraceType, ToJSON)

-- | The amount of effort a reasoning model puts into reasoning
data Effort = Minimal | Low | Medium | High
    deriving stock (Generic)
    deriving anyclass (FromGrace, FromJSON, ToGrace, ToGraceType, ToJSON)
