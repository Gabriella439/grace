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
import Data.HashMap.Strict (HashMap)
import Data.IORef (IORef)
import Data.Text (Text)

import qualified Control.Exception as Exception
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified JavaScript.XHR as XHR
import qualified System.IO.Unsafe as Unsafe

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

cache :: IORef (HashMap Text Text)
cache = Unsafe.unsafePerformIO (IORef.newIORef HashMap.empty)
{-# NOINLINE cache #-}

-- | Fetch a URL (using @XMLHttpRequest@)
fetch
    :: Manager
    -> Text
    -- ^ URL
    -> IO Text
    -- ^ Response body
fetch _manager url = do
    m <- IORef.readIORef cache

    case HashMap.lookup url m of
        Nothing -> do
            (statusCode, body) <- XHR.get url

            case statusCode of
                200 -> do
                    IORef.writeIORef cache $! HashMap.insert url body m
                    return body
                _   -> Exception.throwIO (UnexpectedStatusCode statusCode)
        Just body -> do
            return body

-- | Render an `HttpException` as `Text`
renderError :: HttpException -> Text
renderError (UnexpectedStatusCode code) =
    "Non-200 status code: " <> Text.pack (show code)
