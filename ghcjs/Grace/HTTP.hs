{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}

{-| This module provides a uniform interface for making HTTP requests using both
    GHC and GHCJS
-}
module Grace.HTTP
    ( HttpException
    , Manager
    , newManager
    , fetch
    , HTTP(..)
    , post
    , renderError
    , Methods
    , getMethods
    , createChatCompletion
    ) where

import Control.Exception (Exception(..))
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHCJS.Fetch (Request(..), RequestOptions(..), JSPromiseException)
import Grace.Decode (FromGrace)
import OpenAI.V1.Chat.Completions (ChatCompletionObject, CreateChatCompletion)

import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.JSString as JSString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified GHCJS.Fetch as Fetch
import qualified GHCJS.Prim as Prim
import qualified Network.HTTP.Types as HTTP.Types

{-| The GHCJS implementation of HTTP requests doesn't require a real `Manager`
    so this supplies an empty placeholder
-}
type Manager = ()

-- | An `HttpException` is just a type synonym for a `JSPromiseException`
type HttpException = JSPromiseException

{-| Acquire a new `Manager`

    This does nothing since the GHCJS implementation doesn't use a `Manager`
-}
newManager :: IO Manager
newManager = mempty

-- | Fetch a URL (using @XMLHttpRequest@)
fetch
    :: Manager
    -> Text
    -- ^ URL
    -> IO Text
    -- ^ Response body
fetch _manager url = do
    let request = Request
            { reqUrl = JSString.pack (Text.unpack url)
            , reqOptions = Fetch.defaultRequestOptions
            }

    response <- Fetch.fetch request

    jsString <- Fetch.responseText response

    return (Text.pack (JSString.unpack jsString))

data Header = Header{ header :: Text, value :: Text }
    deriving stock (Generic)
    deriving anyclass (FromGrace)

data HTTP
    = POST
        { url :: Text
        , headers :: Maybe [Header]
        , request :: Maybe Aeson.Value
        }
    deriving stock (Generic)
    deriving anyclass (FromGrace)

-- | Make a POST request
post :: Manager -> HTTP -> IO ByteString
post manager Post{ url, headers, request } = do
    let defaultedHeaders = case headers of
            Nothing -> []
            Just h -> h

    let requiredHeaders =
            [ Header{ header = "Content-Type", value = "application/json" }
            , Header{ header = "Accept"      , value = "application/json" }
            ]

    let reqOptHeaders = do
            Header{ header, value } <- requiredHeaders <> defaultedHeaders

            return (CaseInsensitive.mk (Encoding.encodeUtf8 header), Encoding.encodeUtf8 value)

    let reqOptions₀ = Fetch.defaultRequestOptions
            { reqOptHeaders
            , reqOptMethod = HTTP.Types.methodPost
            }

    reqOptions₁ <- case request of
            Nothing -> do
                return reqOptions₀

            Just requestBody -> do
                requestText <- case Encoding.decodeUtf8' (ByteString.Lazy.toStrict (Aeson.encode requestBody)) of
                    Left exception -> Exception.throwIO exception
                    Right text -> return text

                let reqOptBody =
                        Just (Prim.toJSString (Text.unpack requestText))

                return reqOptions₀{ reqOptBody }

    let request = Request
            { reqUrl = JSString.pack (Text.unpack url)
            , reqOptions = reqOptions₁
            }

    response <- Fetch.fetch request

    jsString <- Fetch.responseText response

    return (ByteString.Lazy.fromStrict (Encoding.encodeUtf8 (Text.pack (JSString.unpack jsString))))

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

    let request = Request
            { reqUrl = "https://api.openai.com/v1/chat/completions"
            , reqOptions = Fetch.defaultRequestOptions
                { reqOptMethod = "POST"
                , reqOptHeaders =
                    [ ("Content-Type", "application/json")
                    , ("Authorization", "Bearer " <> keyBytes)
                    ]
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
