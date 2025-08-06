{-| This module provides a uniform interface for making HTTP requests using both
    GHC and GHCJS
-}
module Grace.HTTP
    ( HttpException
    , fetch
    , HTTP(..)
    , Header(..)
    , Parameter(..)
    , http
    , renderError
    , Methods
    , getMethods
    , Grace.HTTP.createChatCompletion
    ) where

import Control.Concurrent.MVar (MVar)
import Control.Exception (Exception(..))
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Grace.HTTP.Type (Header(..), HTTP(..), Parameter(..), completeHeaders)
import OpenAI.V1 (Methods(..))
import OpenAI.V1.Chat.Completions (ChatCompletionObject, CreateChatCompletion)

import Network.HTTP.Client
    ( HttpExceptionContent(..)
    , Manager
    , ManagerSettings(..)
    , Request(..)
    , RequestBody(..)
    , method
    )

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Lazy.Encoding
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types as HTTP.Types
import qualified OpenAI.V1 as OpenAI
import qualified System.IO.Unsafe as Unsafe

-- | Exception type thrown by `fetch` in the event of any failure
data HttpException
    = HttpException HTTP.HttpException
    | NotUTF8 UnicodeException
    deriving stock (Show)

instance Exception HttpException where
    displayException = Text.unpack . renderError

managerMVar :: MVar (Maybe Manager)
managerMVar = Unsafe.unsafePerformIO (MVar.newMVar Nothing)
{-# NOINLINE managerMVar #-}

-- | Acquire a new `Manager`
--
-- This is safe to call multiple times.  The `Manager` returned by the first
-- call is cached and reused by subsequent calls.
newManager :: IO Manager
newManager = MVar.modifyMVar managerMVar \maybeManager -> do
    manager <- case maybeManager of
        Nothing -> do
            TLS.newTlsManagerWith TLS.tlsManagerSettings
                { managerResponseTimeout = HTTP.responseTimeoutNone
                }

        Just manager -> do
            return manager

    return (Just manager, manager)

-- | Fetch a URL (using the @http-client@ package)
fetch
    :: Text
    -- ^ URL
    -> IO Text
    -- ^ Response body
fetch url = do
    manager <- newManager

    request <- HTTP.parseUrlThrow (Text.unpack url)

    let handler :: HTTP.HttpException -> IO a
        handler httpException = Exception.throwIO (HttpException httpException)

    response <- Exception.handle handler (HTTP.httpLbs request manager)

    let lazyBytes = HTTP.responseBody response

    case Lazy.Encoding.decodeUtf8' lazyBytes of
        Left exception -> Exception.throwIO (NotUTF8 exception)
        Right lazyText -> return (Text.Lazy.toStrict lazyText)

-- | Make a POST request
http :: HTTP -> IO Text
http GET{ url, headers, parameters } = do
    manager <- newManager

    request₀ <- HTTP.parseUrlThrow (Text.unpack url)

    let request₁ = request₀
            { method = HTTP.Types.methodGet
            , requestHeaders = completeHeaders headers
            }

    let request₂ = case parameters of
            Nothing ->
                request₁
            Just ps ->
                let convertedParameters = do
                        Parameter{ parameter, value } <- ps
                        return (Encoding.encodeUtf8 parameter, fmap Encoding.encodeUtf8 value)

                in  HTTP.setQueryString convertedParameters request₁

    let handler :: HTTP.HttpException -> IO a
        handler httpException = Exception.throwIO (HttpException httpException)

    response <- Exception.handle handler (HTTP.httpLbs request₂ manager)

    case Lazy.Encoding.decodeUtf8' (HTTP.responseBody response) of
        Left exception -> Exception.throwIO (NotUTF8 exception)
        Right lazyText -> return (Text.Lazy.toStrict lazyText)

http POST{ url, headers, request } = do
    manager <- newManager

    request₀ <- HTTP.parseUrlThrow (Text.unpack url)

    let request₁ = request₀
            { method = HTTP.Types.methodPost
            , requestHeaders = completeHeaders headers
            }

    let request₂ = case request of
            Nothing ->
                request₁
            Just requestBody ->
                request₁{ requestBody = RequestBodyLBS (Aeson.encode requestBody) }

    let handler :: HTTP.HttpException -> IO a
        handler httpException = Exception.throwIO (HttpException httpException)

    response <- Exception.handle handler (HTTP.httpLbs request₂ manager)

    case Lazy.Encoding.decodeUtf8' (HTTP.responseBody response) of
        Left exception -> Exception.throwIO (NotUTF8 exception)
        Right lazyText -> return (Text.Lazy.toStrict lazyText)

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
