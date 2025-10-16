-- | Utilities for working with the @aeson@ package
module Grace.Aeson where

import Control.Exception.Safe (Exception(..))
import Data.Aeson (FromJSON)
import Data.Text (Text)

import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

-- | JSON decoding failed
data JSONDecodingFailed = JSONDecodingFailed
    { message :: String
    , text :: Text
    } deriving stock (Show)

instance Exception JSONDecodingFailed where
    displayException JSONDecodingFailed{ message, text } =
        "Failed to decode output as JSON\n\
        \\n\
        \The following text:\n\
        \\n\
        \" <> Text.unpack text <> "\n\
        \\n\
        \… to decode as JSON.\n\
        \\n\
        \Decoding error message:\n\
        \\n\
        \" <> message

-- | Decode a structured value from JSON-encoded `Text`
decode :: FromJSON a => Text -> IO a
decode text = do
    let bytes = ByteString.Lazy.fromStrict (Encoding.encodeUtf8 text)

    case Aeson.eitherDecode bytes of
        Left message ->
            Exception.throwIO JSONDecodingFailed{ message, text }
        Right a ->
            return a
