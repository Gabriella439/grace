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
    , createChatCompletion
    ) where

import Control.Exception.Safe (Exception(..))
import Data.Text (Text)
import GHCJS.Fetch.Types (JSResponse)
import OpenAI.V1.Chat.Completions (ChatCompletionObject, CreateChatCompletion)

import Grace.HTTP.Type
    ( Header(..)
    , HTTP(..)
    , Parameter(..)
    , completeHeaders
    , organization
    )
import GHCJS.Fetch
    ( Request(..)
    , RequestCredentials(..)
    , RequestOptions(..)
    , JSPromiseException
    )

import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Binary.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.JSString as JSString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified GHCJS.Fetch as Fetch
import qualified GHCJS.Prim as Prim
import qualified Network.HTTP.Types as HTTP.Types

-- | An `HttpException` is just a type synonym for a `JSPromiseException`
type HttpException = JSPromiseException

-- | Fetch a URL (using @XMLHttpRequest@)
fetch
    :: Text
    -- ^ URL
    -> IO Text
    -- ^ Response body
fetch url = do
    let request = Request
            { reqUrl = JSString.pack (Text.unpack url)
            , reqOptions = Fetch.defaultRequestOptions
                { reqOptMethod = HTTP.Types.methodGet
                }
            }

    response <- Fetch.fetch request

    jsString <- Fetch.responseText response

    return (Text.pack (JSString.unpack jsString))

responseToText :: JSResponse -> IO Text
responseToText response = do
    jsString <- Fetch.responseText response

    return (Text.pack (JSString.unpack jsString))

-- | Make an HTTP request
http :: HTTP -> IO Text
http GET{ url, headers, parameters } = do
    reqUrl <- case parameters of
        Nothing -> do
            return (JSString.pack (Text.unpack url))
        Just ps -> do
          let queryText = do
                  Parameter{ parameter, value } <- ps

                  return (parameter, value)

          let builder = HTTP.Types.renderQueryText True queryText

          let bytes =
                  ByteString.Lazy.toStrict (Builder.toLazyByteString builder)

          query <- case Encoding.decodeUtf8' bytes of
              Left exception -> Exception.throwIO exception
              Right text -> return text

          return (JSString.pack (Text.unpack (url <> query)))

    let reqOptions = Fetch.defaultRequestOptions
            { reqOptHeaders = completeHeaders headers
            , reqOptMethod = HTTP.Types.methodGet
            , reqOptCredentials = CredInclude
            }

    let request = Request{ reqUrl, reqOptions }

    response <- Fetch.fetch request

    responseToText response

http POST{ url, headers, request } = do
    let reqUrl = JSString.pack (Text.unpack url)

    let reqOptions₀ = Fetch.defaultRequestOptions
            { reqOptHeaders = completeHeaders headers
            , reqOptMethod = HTTP.Types.methodPost
            , reqOptCredentials = CredInclude
            }

    reqOptions <- case request of
            Nothing -> do
                return reqOptions₀

            Just requestBody -> do
                requestText <- case Encoding.decodeUtf8' (ByteString.Lazy.toStrict (Aeson.encode requestBody)) of
                    Left exception -> Exception.throwIO exception
                    Right text -> return text

                let reqOptBody =
                        Just (Prim.toJSString (Text.unpack requestText))

                return reqOptions₀{ reqOptBody }

    response <- Fetch.fetch Request{ reqUrl, reqOptions }

    responseToText response

-- | Render an `HttpException` as `Data.Text.Text`
renderError :: HttpException -> Text
renderError = Text.pack . displayException

-- | The GHCJS implementation of OpenAI bindings just stores the API key
type Methods = Text

-- | Initialize API for prompting
getMethods :: IO (Text -> Methods)
getMethods = return id

-- | This powers the @prompt@ keyword
createChatCompletion
    :: Methods
    -> CreateChatCompletion
    -> IO ChatCompletionObject
createChatCompletion key createChatCompletion_ = do
    let keyBytes = Encoding.encodeUtf8 key

    body <- case Encoding.decodeUtf8' (ByteString.Lazy.toStrict (Aeson.encode createChatCompletion_)) of
        Left exception -> Exception.throwIO exception
        Right text -> return (Text.unpack text)

    let organizationHeader = case organization of
            Nothing -> []
            Just o  -> [("OpenAI-Organization", Encoding.encodeUtf8 o)]

    let request = Request
            { reqUrl = "https://api.openai.com/v1/chat/completions"
            , reqOptions = Fetch.defaultRequestOptions
                { reqOptMethod = "POST"
                , reqOptHeaders =
                    [ ("Content-Type", "application/json")
                    , ("Authorization", "Bearer " <> keyBytes)
                    ] <> organizationHeader
                , reqOptBody = Just (Prim.toJSString body)
                }
            }

    response <- Fetch.fetch request

    jsString <- Fetch.responseText response

    let strictBytes = Encoding.encodeUtf8 (Text.pack (JSString.unpack jsString))

    let lazyBytes = ByteString.Lazy.fromStrict strictBytes

    case Aeson.eitherDecode lazyBytes of
        Left string ->
            fail string
        Right chatCompletionObject ->
            return chatCompletionObject
