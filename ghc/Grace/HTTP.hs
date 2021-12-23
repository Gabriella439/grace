{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}

{-| This module provides a uniform interface for making HTTP requests using both
    GHC and GHCJS
-}
module Grace.HTTP
    ( HttpException
    , Manager
    , newManager
    , fetch
    , renderError
    ) where

import Control.Exception (Exception(..))
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Network.HTTP.Client (HttpExceptionContent(..), Manager)

import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Lazy.Encoding
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types as HTTP.Types

-- | Exception type thrown by `fetch` in the event of any failure
data HttpException
    = HttpException HTTP.HttpException
    | NotUTF8 UnicodeException
    deriving stock (Show)

instance Exception HttpException where
    displayException = Text.unpack . renderError

-- | Acquire a new `Manager`
newManager :: IO Manager
newManager = TLS.newTlsManager

-- | Fetch a URL (using the @http-client@ package)
fetch
    :: Manager
    -> Text
    -- ^ URL
    -> IO Text
    -- ^ Response body
fetch manager url = do
    request <- HTTP.parseUrlThrow (Text.unpack url)

    let handler :: HTTP.HttpException -> IO a
        handler httpException = Exception.throwIO (HttpException httpException)

    response <- Exception.handle handler (HTTP.httpLbs request manager)

    let lazyBytes = HTTP.responseBody response

    case Lazy.Encoding.decodeUtf8' lazyBytes of
        Left exception -> Exception.throwIO (NotUTF8 exception)
        Right lazyText -> return (Text.Lazy.toStrict lazyText)

-- | Render an `HttpException` as `Text`
renderError :: HttpException -> Text
renderError (HttpException httpException) = case httpException of
    HTTP.InvalidUrlException _ _ ->
        "Invalid URL"

    HTTP.HttpExceptionRequest _ e -> case e of
        ConnectionFailure _ ->
            "Remote host not found"
        InvalidDestinationHost _ ->
            "Invalid remote host name"
        ResponseTimeout ->
            "The remote host took too long to respond"
        ConnectionTimeout ->
            "Connection establishment took too long"
        StatusCodeException response body -> prefix <> suffix
          where
            statusCode =
                HTTP.Types.statusCode (HTTP.responseStatus response)

            prefix =
                case statusCode of
                    401 -> "Access unauthorized"
                    403 -> "Access forbidden"
                    404 -> "Remote file not found"
                    500 -> "Server-side failure"
                    502 -> "Upstream failure"
                    503 -> "Server temporarily unavailable"
                    504 -> "Upstream timeout"
                    _   -> "HTTP request failure"

            suffix =
                    "\n\n"
                <>  [__i|
                    HTTP status code: #{statusCode}#{responseBody}
                    |]

            responseBody :: Text
            responseBody =
                case Encoding.decodeUtf8' body of
                    Left _ ->
                            "\n\n"
                        <>  [__i|
                            Response body (non-UTF8 bytes):

                            #{body}
                            |]
                    Right "" ->
                        ""
                    Right bodyText ->
                            "\n\n"
                        <>  [__i|
                            Response body:

                            #{prefixedText}
                            |]
                      where
                        prefixedLines =
                                zipWith combine prefixes
                                    (Text.lines bodyText)
                            <>  [ "…│ …" ]
                          where
                            prefixes = [(1 :: Int)..7]

                            combine n line =
                                Text.pack (show n) <> "│ " <> line

                        prefixedText = Text.unlines prefixedLines
        _ ->
           [__i|
           HTTP request failure

           #{displayException httpException}
           |]
renderError (NotUTF8 unicodeException) =
    [__i|
    Not UTF8

    #{Exception.displayException unicodeException}
    |]
