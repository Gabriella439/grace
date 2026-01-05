-- | Types associated with prompting LLMs
module Grace.Prompt.Types
    ( -- * Types
      Prompt(..)
    , Message(..)
    , Effort(..)
    ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Grace.Decode (FromGrace, Key(..), ToGraceType)

-- | Arguments to the @prompt@ keyword
data Prompt = Prompt
    { key :: Grace.Decode.Key
    , text :: Maybe Text
    , history :: Maybe [Message]
    , model :: Maybe Text
    , search :: Maybe Bool
    , effort :: Maybe Effort
    } deriving stock (Generic)
      deriving anyclass (FromGrace, ToGraceType)

-- | A message added to the conversation history
data Message
    = System{ name :: Maybe Text, text :: Text }
    | User{ name :: Maybe Text, text :: Text }
    | Assistant{ name :: Maybe Text, text :: Text }
    deriving stock (Generic)
    deriving anyclass (FromGrace, ToGraceType)

-- | The amount of effort a reasoning model puts into reasoning
data Effort = Low | Medium | High
    deriving stock (Generic)
    deriving anyclass (FromGrace, ToGraceType)
