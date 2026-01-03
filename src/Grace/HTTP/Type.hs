-- | This module contains types shared between the GHC and GHCJS implementations
module Grace.HTTP.Type where

import Data.Aeson (Value)
import Data.Text (Text)
import GHC.Generics (Generic)
import Grace.Decode (FromGrace, ToGraceType)

import qualified Control.Exception.Safe as Exception
import qualified Data.Binary.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Time.Clock.POSIX as Time
import qualified Network.HTTP.Types as HTTP.Types

-- | An HTTP header
data Header = Header{ header :: Text, value :: Text }
    deriving stock (Generic)
    deriving anyclass (FromGrace, ToGraceType)

-- | A query parameter
data Parameter = Parameter{ parameter :: Text, value :: Maybe Text }
    deriving stock (Generic)
    deriving anyclass (FromGrace, ToGraceType)

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
    deriving anyclass (FromGrace, ToGraceType)

completeHeaders :: Bool -> Bool -> Maybe [Header] -> [HTTP.Types.Header]
completeHeaders import_ body headers = do
    Header{ header, value } <- requiredHeaders <> defaultedHeaders

    let headerBytes = CaseInsensitive.mk (Encoding.encodeUtf8 header)

    let valueBytes = Encoding.encodeUtf8 (Text.strip value)

    return (headerBytes, valueBytes)
  where
    requiredHeaders
        | import_ =
            [ ]
        | otherwise =
            (   [ Header{ header = "Accept"      , value = "application/json" }
                ]
            <>  contentType
            )
      where
        contentType
            | body =
                [ Header{ header = "Content-Type", value = "application/json" } ]
            | otherwise =
                [ ]

    defaultedHeaders = case headers of
        Nothing -> []
        Just h -> h

organization :: Maybe Text
organization = Nothing

renderQueryText :: Text -> Maybe [Parameter] -> IO Text
renderQueryText url parameters = do
    let (intermediateURL, queryBytes) = Text.break (== '?') url

    let oldQueryText =
            HTTP.Types.parseQueryText (Encoding.encodeUtf8 queryBytes)

    let oldParameters = do
            (parameter, value) <- oldQueryText

            return Parameter{ parameter, value }

    currentTime <- Time.getPOSIXTime

    let cacheBust =
            [ Parameter
                { parameter = "cachebust"
                , value = Just (Text.pack (show currentTime))
                }
            ]

    let finalParameters = case parameters of
            Nothing -> oldParameters <> cacheBust
            Just newParameters -> oldParameters <> newParameters <> cacheBust

    let queryText = do
            Parameter{ parameter, value } <- finalParameters

            return (parameter, value)

    let builder = HTTP.Types.renderQueryText True queryText

    let bytes = ByteString.Lazy.toStrict (Builder.toLazyByteString builder)

    case Encoding.decodeUtf8' bytes of
        Left exception -> Exception.throwIO exception
        Right text -> return (intermediateURL <> text)
