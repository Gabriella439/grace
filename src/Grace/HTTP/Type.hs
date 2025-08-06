-- | This module contains types shared between the GHC and GHCJS implementations
module Grace.HTTP.Type where

import Data.Aeson (Value)
import Data.Text (Text)
import GHC.Generics (Generic)
import Grace.Decode (FromGrace)

import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Text.Encoding as Encoding
import qualified Network.HTTP.Types as HTTP.Types

-- | An HTTP header
data Header = Header{ header :: Text, value :: Text }
    deriving stock (Generic)
    deriving anyclass (FromGrace)

-- | A query parameter
data Parameter = Parameter{ parameter :: Text, value :: Maybe Text }
    deriving stock (Generic)
    deriving anyclass (FromGrace)

-- | An HTTP request
data HTTP
    = GET
        { url :: Text
        , headers :: Maybe [Header]
        , parameters :: Maybe [Parameter]
        }
    | POST
        { url :: Text
        , headers :: Maybe [Header]
        , request :: Maybe Value
        }
    deriving stock (Generic)
    deriving anyclass (FromGrace)

completeHeaders :: Maybe [Header] -> [HTTP.Types.Header]
completeHeaders headers = do
    Header{ header, value } <- requiredHeaders <> defaultedHeaders

    let headerBytes = CaseInsensitive.mk (Encoding.encodeUtf8 header)

    let valueBytes = Encoding.encodeUtf8 value

    return (headerBytes, valueBytes)
  where
    requiredHeaders =
        [ Header{ header = "Content-Type", value = "application/json" }
        , Header{ header = "Accept"      , value = "application/json" }
        ]

    defaultedHeaders = case headers of
        Nothing -> []
        Just h -> h
