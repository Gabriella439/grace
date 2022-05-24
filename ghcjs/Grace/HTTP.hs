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

import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified JavaScript.XHR as XHR

{-| The GHCJS implementation of HTTP requests doesn't require a real `Manager`
    so this supplies an empty placeholder
-}
type Manager = ()

-- | Exception type thrown by `fetch` in the event of any failure
data HttpException = UnexpectedStatusCode Int
    deriving stock (Show)

instance Exception HttpException where
    displayException = Text.unpack . renderError

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
    (statusCode, body) <- XHR.get url

    case statusCode of
        200 -> return body
        _   -> Exception.throwIO (UnexpectedStatusCode statusCode)

-- | Render an `HttpException` as `Text`
renderError :: HttpException -> Text
renderError (UnexpectedStatusCode code) =
    "Non-200 status code: " <> Text.pack (show code)
