{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf        #-}

module Main where

import Control.Exception (Exception(..))
import Data.Foldable (toList)
import Data.JSString (JSString)
import Data.Text (Text)
import Data.Traversable (forM)
import GHCJS.Foreign.Callback (Callback)
import GHCJS.Types (JSVal)
import Grace.Input (Input(..))
import Grace.Syntax (Scalar(..))
import Grace.Value (Value(..))
import JavaScript.Array (JSArray)
import Prelude hiding (error)

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.JSString as JSString
import qualified Data.Text as Text
import qualified GHCJS.Foreign.Callback as Callback
import qualified Grace.Interpret as Interpret
import qualified Grace.Normalize as Normalize
import qualified Grace.Pretty as Pretty
import qualified Grace.Normalize as Normalize
import qualified JavaScript.Array as Array

foreign import javascript unsafe "document.getElementById($1)"
    getElementById :: JSString -> IO JSVal

foreign import javascript unsafe "$1.value"
    getValue :: JSVal -> IO JSString

foreign import javascript unsafe "$1.innerText = $2"
    setInnerText :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.textContent= $2"
    setTextContent :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.style.display = $2"
    setDisplay :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.addEventListener($2, $3)"
    addEventListener :: JSVal -> JSString -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "document.createElement($1)"
    createElement :: JSString -> IO JSVal

foreign import javascript unsafe "$1.setAttribute($2,$3)"
    setAttribute :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript unsafe "$1.replaceChildren($2)"
    replaceChild :: JSVal -> JSVal -> IO ()

-- @$1.replaceChildren(...$2)@ does not work because GHCJS fails to parse the
-- spread operator, we work around this by defining the
-- @replaceChildrenWorkaround@ function in JavaScript which takes care of the
-- spread operator for us
foreign import javascript unsafe "replaceChildrenWorkaround($1, $2)"
    replaceChildren :: JSVal -> JSArray -> IO ()

valueToJSString :: Value -> JSString
valueToJSString =
    JSString.pack . Text.unpack . Pretty.renderStrict False 80 . Normalize.quote []

renderValue :: JSVal -> Value -> IO ()
renderValue parent value@Variable{} = do
    var <- createElement "var"
    setTextContent var (valueToJSString value)
renderValue parent (Scalar (Text text))= do
    pre <- createElement "pre"
    setTextContent pre (JSString.pack (Text.unpack text))
    replaceChild parent pre
renderValue parent (Scalar (Bool bool)) = do
    input <- createElement "input"
    setAttribute input "type" "checkbox"
    setAttribute input "class" "form-check-input"
    Monad.when bool (setAttribute input "checked" "")
    setAttribute input "disabled" ""
    replaceChild parent input
renderValue parent (Scalar Null) = do
    span <- createElement "span"
    setTextContent span "∅"
    replaceChild parent span
renderValue parent value@Scalar{} = do
    span <- createElement "span"
    setTextContent span (valueToJSString value)
    replaceChild parent span
renderValue parent (List values) = do
    lis <- forM values \value -> do
        li <- createElement "li"
        renderValue li value
        return li
    ul <- createElement "ul"
    replaceChildren ul (Array.fromList (toList lis))
    replaceChild parent ul
renderValue parent (Record keyValues) = do
    let process key value = do
            dt <- createElement "dt"
            setTextContent dt (JSString.pack (Text.unpack key))
            dd <- createElement "dd"
            renderValue dd value
            return [ dt, dd ]
    dtds <- HashMap.traverseWithKey process keyValues
    dl <- createElement "dl"
    replaceChildren dl (Array.fromList (toList (concat dtds)))
    replaceChild parent dl
renderValue parent (Application (Alternative alternative) value) = do
    renderValue parent (Record (HashMap.singleton alternative value))
renderValue parent value = do
    code <- createElement "code"
    setTextContent code (valueToJSString value)
    pre <- createElement "pre"
    replaceChild pre code
    replaceChild parent pre

main :: IO ()
main = do
    input  <- getElementById "input"
    output <- getElementById "output"
    error  <- getElementById "error"

    let getInput = do
            jsString <- getValue input
            return (Text.pack (JSString.unpack jsString))

    let setError text = do
            setTextContent error (JSString.pack (Text.unpack text))
            setDisplay output "none"
            setDisplay error "block"

    let setOutput value = do
            renderValue output value
            setDisplay error "none"
            setDisplay output "block"

    let interpret = do
            text <- getInput

            if  | Text.null text -> do
                    setError ""
                | otherwise -> do
                    let input = Code "(input)" text

                    setError "…"

                    result <- Except.runExceptT (Interpret.interpret input)

                    case result of
                        Left interpretError -> do
                            setError (Text.pack (displayException interpretError))
                        Right (_, value) -> do
                            setOutput value

    callback <- Callback.asyncCallback interpret

    addEventListener input "input" callback
