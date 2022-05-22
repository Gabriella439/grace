{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (Exception(..))
import Data.JSString (JSString)
import Data.Text (Text)
import GHCJS.Foreign.Callback (Callback)
import Grace.Input (Input(..))

import qualified Control.Monad.Except as Except
import qualified Data.JSString as JSString
import qualified Data.Text as Text
import qualified GHCJS.Foreign.Callback as Callback
import qualified Grace.Interpret as Interpret
import qualified Grace.Pretty as Pretty
import qualified Grace.Normalize as Normalize

foreign import javascript unsafe "input.addEventListener('input', $1)"
    registerCallback :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "input.value"
    getInputString :: IO JSString

foreign import javascript unsafe "output.innerText = $1"
    setOutputString :: JSString -> IO ()

foreign import javascript unsafe "error.innerText = $1"
    setErrorString :: JSString -> IO ()

foreign import javascript unsafe "output.style.display = $1"
    setOutputDisplay :: JSString -> IO ()

foreign import javascript unsafe "error.style.display = $1"
    setErrorDisplay :: JSString -> IO ()

getInput :: IO Text
getInput = do
    jsString <- getInputString
    return (Text.pack (JSString.unpack jsString))

setError :: Text -> IO ()
setError text = do
    setErrorString (JSString.pack (Text.unpack text))
    setOutputDisplay "none"
    setErrorDisplay "block"

setOutput :: Text -> IO ()
setOutput text = do
    setOutputString (JSString.pack (Text.unpack text))
    setErrorDisplay "none"
    setOutputDisplay "block"

main :: IO ()
main = do
    let interpret = do
            text <- getInput

            let input = Code "(input)" text

            result <- Except.runExceptT (Interpret.interpret input)

            case result of
                Left interpretError -> do
                    setError (Text.pack (displayException interpretError))
                Right (_, value) -> do
                    setOutput (Pretty.toText (Normalize.quote [] value))

    callback <- Callback.asyncCallback interpret

    registerCallback callback
