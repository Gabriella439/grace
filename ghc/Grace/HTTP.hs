{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NamedFieldPuns        #-}

{-| This module provides a uniform interface for making HTTP requests using both
    GHC and GHCJS
-}
module Grace.HTTP
    ( HttpException
    , Manager
    , newManager
    , fetch
    , post
    , renderError
    , Methods
    , getMethods
    , Grace.HTTP.createChatCompletion
    ) where

import Control.Exception (Exception(..))
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import OpenAI.V1 (Methods(..))
import OpenAI.V1.Chat.Completions (ChatCompletionObject, CreateChatCompletion)

import Network.HTTP.Client
    ( HttpExceptionContent(..)
    , Manager
    , Request(..)
    , RequestBody(..)
    , method
    )

import qualified Control.Exception as Exception
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Lazy.Encoding
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types as HTTP.Types
import qualified OpenAI.V1 as OpenAI

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

-- | Make a POST request
post
    :: Manager
    -> Text
    -- ^ URL
    -> [(Text, Text)]
    -- ^ Headers
    -> Maybe ByteString
    -- ^ Request body
    -> IO ByteString
    -- ^ Response body
post manager url headers maybeRequestBody = do
    request₀ <- HTTP.parseUrlThrow (Text.unpack url)

    let requiredHeaders =
            [ ("Content-Type", "application/json")
            , ("Accept"      , "application/json")
            ]

    let requestHeaders = do
            (headerName, headerValue) <- requiredHeaders <> headers

            let newHeaderName =
                    CaseInsensitive.mk (Encoding.encodeUtf8 headerName)

            let newHeaderValue = Encoding.encodeUtf8 headerValue

            return (newHeaderName, newHeaderValue)

    let request₁ = request₀
            { method = HTTP.Types.methodPost
            , requestHeaders
            }

    let request₂ = case maybeRequestBody of
            Nothing ->
                request₁
            Just requestBody ->
                request₁{ requestBody = RequestBodyLBS requestBody }

    let handler :: HTTP.HttpException -> IO a
        handler httpException = Exception.throwIO (HttpException httpException)

    response <- Exception.handle handler (HTTP.httpLbs request₂ manager)

    return (HTTP.responseBody response)

-- | Render an `HttpException` as `Data.Text.Text`
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
                    "\n\
                    \\n\
                    \HTTP status code: " <> Text.pack (show statusCode) <> responseBody

            responseBody :: Text
            responseBody =
                case Encoding.decodeUtf8' body of
                    Left _ ->
                            "\n\
                            \\n\
                            \Response body (non-UTF8 bytes):\n\
                            \\n\
                            \" <> Text.pack (show body)
                    Right "" ->
                        ""
                    Right bodyText ->
                            "\n\n"
                        <>  "Response body:\n\
                            \\n\
                            \" <> prefixedText
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
           "HTTP request failure\n\
           \\n\
           \" <> Text.pack (displayException httpException)
renderError (NotUTF8 unicodeException) =
    "Not UTF8\n\
    \\n\
    \" <> Text.pack (displayException unicodeException)

-- | Initialize API for prompting
getMethods :: IO (Text -> Methods)
getMethods = do
    clientEnv <- OpenAI.getClientEnv "https://api.openai.com"

    return (OpenAI.makeMethods clientEnv)

-- | This powers the @prompt@ keyword
createChatCompletion
    :: Methods
    -> CreateChatCompletion
    -> IO ChatCompletionObject
createChatCompletion Methods{ createChatCompletion = c } = c
