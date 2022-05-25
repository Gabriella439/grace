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
import Data.Text (Text)
import GHCJS.Fetch (Request(..), JSPromiseException)

import qualified Data.JSString as JSString
import qualified Data.Text as Text
import qualified GHCJS.Fetch as Fetch

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

-- | Render an `HttpException` as `Text`
renderError :: HttpException -> Text
renderError = Text.pack . displayException
