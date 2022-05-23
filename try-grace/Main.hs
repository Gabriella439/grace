{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf        #-}

module Main where

import Control.Exception (Exception(..))
import Data.Foldable (toList)
import Data.JSString (JSString)
import Data.Traversable (forM)
import Grace.Type (Type(..))
import GHCJS.Foreign.Callback (Callback)
import GHCJS.Types (JSVal)
import Grace.Input (Input(..))
import Grace.Syntax (Scalar(..))
import Grace.Value (Value(..))
import JavaScript.Array (JSArray)
import Prelude hiding (div, error, span, subtract)

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.IntMap as IntMap
import qualified Data.IORef as IORef
import qualified Data.JSString as JSString
import qualified Data.Scientific as Scientific
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Encoding
import qualified GHCJS.Foreign.Callback as Callback
import qualified Grace.Domain as Domain
import qualified Grace.Interpret as Interpret
import qualified Grace.Monotype as Monotype
import qualified Grace.Normalize as Normalize
import qualified Grace.Pretty as Pretty
import qualified Grace.Type as Type
import qualified Grace.Value as Value
import qualified JavaScript.Array as Array

foreign import javascript unsafe "document.getElementById($1)"
    getElementById :: JSString -> IO JSVal

foreign import javascript unsafe "$1.value"
    getValue :: JSVal -> IO JSString

foreign import javascript unsafe "$1.value"
    getDoubleValue :: JSVal -> IO Double

foreign import javascript unsafe "$1.value"
    getIntValue :: JSVal -> IO Int

foreign import javascript unsafe "$1.checked"
    getChecked :: JSVal -> IO Bool

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

foreign import javascript unsafe "new MutationObserver($1)"
    newObserver :: Callback (IO ()) -> IO JSVal

foreign import javascript unsafe "$1.observe($2, { childList: true, subtree: true })"
    observe :: JSVal -> JSVal -> IO ()

-- @$1.replaceChildren(...$2)@ does not work because GHCJS fails to parse the
-- spread operator, we work around this by defining the
-- @replaceChildrenWorkaround@ function in JavaScript which takes care of the
-- spread operator for us
foreign import javascript unsafe "replaceChildrenWorkaround($1, $2)"
    replaceChildren :: JSVal -> JSArray -> IO ()

foreign import javascript unsafe "$1.before($2)"
    before :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.remove()"
    remove :: JSVal -> IO ()

valueToJSString :: Value -> JSString
valueToJSString =
      JSString.pack
    . Text.unpack
    . Pretty.renderStrict False 80
    . Normalize.quote []

renderValue :: JSVal -> Type s -> Value -> IO ()
renderValue parent Type.Forall{ name, nameLocation, domain = Domain.Type, type_ } value =
    renderValue parent (Type.substituteType name 0 Type.Scalar{ location = nameLocation, scalar = Monotype.JSON } type_) value
renderValue parent Type.Forall{ name, domain = Domain.Fields, type_ } value =
    renderValue parent (Type.substituteFields name 0 (Type.Fields [] Monotype.EmptyFields) type_) value
renderValue parent Type.Forall{ name, domain = Domain.Alternatives, type_ } value =
    renderValue parent (Type.substituteAlternatives name 0 (Type.Alternatives [] Monotype.EmptyAlternatives) type_) value
renderValue parent _ value@Variable{} = do
    var <- createElement "var"
    setTextContent var (valueToJSString value)
    replaceChild parent var
renderValue parent _ (Value.Scalar (Text text))= do
    p <- createElement "p"
    setTextContent p (JSString.pack (Text.unpack text))
    replaceChild parent p
renderValue parent _ (Value.Scalar (Bool bool)) = do
    input <- createElement "input"
    setAttribute input "type" "checkbox"
    setAttribute input "class" "form-check-input"
    Monad.when bool (setAttribute input "checked" "")
    setAttribute input "disabled" ""
    replaceChild parent input
renderValue parent _ (Value.Scalar Null) = do
    span <- createElement "span"
    setTextContent span "∅"
    replaceChild parent span
renderValue parent _ value@Value.Scalar{} = do
    span <- createElement "span"
    setTextContent span (valueToJSString value)
    replaceChild parent span
renderValue parent outer (Value.List values) = do
    inner <- case outer of
            Type.List{ type_ } -> do
                return type_
            Type.Scalar{ scalar = Monotype.JSON } -> do
                return outer
            _ -> do
                fail "renderValue: Missing element type"
    lis <- forM values \value -> do
        li <- createElement "li"
        renderValue li inner value
        return li
    ul <- createElement "ul"
    replaceChildren ul (Array.fromList (toList lis))
    replaceChild parent ul
renderValue parent Type.Record{ fields = Type.Fields keyTypes _ } (Value.Record keyValues) = do
    let process key value = do
            type_ <- case lookup key keyTypes of
                Nothing    -> fail "renderValue: Missing field type"
                Just type_ -> return type_
            dt <- createElement "dt"
            setTextContent dt (JSString.pack (Text.unpack key))
            dd <- createElement "dd"
            renderValue dd type_ value
            return [ dt, dd ]
    dtds <- HashMap.traverseWithKey process keyValues
    dl <- createElement "dl"
    replaceChildren dl (Array.fromList (toList (concat dtds)))
    replaceChild parent dl
renderValue parent outer (Application (Value.Alternative alternative) value) = do
    inner <- case outer of
            Type.Union{ alternatives = Type.Alternatives keyTypes _ } ->
                case lookup alternative keyTypes of
                    Nothing    -> fail "renderValue: Missing alternative type"
                    Just type_ -> return type_
            _ -> do
                fail "renderValue: Missing alternative type"
    renderValue parent Type.Record{ location = location outer, fields = Type.Fields [(alternative, inner)] Monotype.EmptyFields } (Value.Record (HashMap.singleton alternative value))
renderValue parent Type.Function{ input, output } function = do
    (input_, get) <- renderInput input
    div <- createElement "div"
    let invoke = do
            value <- get
            renderValue div output (Normalize.apply function value)
    callback <- Callback.asyncCallback invoke
    addEventListener input_ "input" callback
    observer <- newObserver callback
    observe observer input_
    invoke
    replaceChildren parent (Array.fromList [ input_, div ])
renderValue parent _ value = do
    code <- createElement "code"
    setTextContent code (valueToJSString value)
    replaceChild parent code

renderInput :: Type s -> IO (JSVal, IO Value)
renderInput Type.Scalar{ scalar = Monotype.Bool } = do
    input <- createElement "input"
    setAttribute input "type" "checkbox"
    setAttribute input "class" "form-check-input"
    let get = do
            bool <- getChecked input
            return (Value.Scalar (Bool bool))
    return (input, get)
renderInput Type.Scalar{ scalar = Monotype.Real } = do
    input <- createElement "input"
    setAttribute input "type" "number"
    setAttribute input "step" "any"
    setAttribute input "value" "0"
    let get = do
            double <- getDoubleValue input
            return (Value.Scalar (Real (Scientific.fromFloatDigits double)))
    return (input, get)
renderInput Type.Scalar{ scalar = Monotype.Integer } = do
    input <- createElement "input"
    setAttribute input "type" "number"
    setAttribute input "value" "0"
    let get = do
            integer <- getIntValue input
            return (Value.Scalar (Integer (fromIntegral integer)))
    return (input, get)
renderInput Type.Scalar{ scalar = Monotype.Natural } = do
    input <- createElement "input"
    setAttribute input "type" "number"
    setAttribute input "value" "0"
    setAttribute input "min" "0"
    let get = do
            integer <- getIntValue input
            return (Value.Scalar (Natural (fromIntegral integer)))
    return (input, get)
renderInput Type.Scalar{ scalar = Monotype.JSON } = do
    input <- createElement "input"
    setAttribute input "value" "null"
    let get = do
            string <- getValue input
            case Aeson.eitherDecode (Text.Encoding.encodeUtf8 (Text.Lazy.fromStrict (Text.pack (JSString.unpack string)))) of
                Left _ -> do
                    setAttribute input "class" "form-control is-invalid"
                    return (Value.Scalar Null)
                Right value -> do
                    setAttribute input "class" "form-control is-valid"
                    return value
    return (input, get)
renderInput Type.Scalar{ scalar = Monotype.Text } = do
    textarea <- createElement "textarea"
    let get = do
            string <- getValue textarea
            return (Value.Scalar (Text (Text.pack (JSString.unpack string))))
    return (textarea, get)
renderInput Type.Record{ fields = Type.Fields keyTypes _ } = do
    let process (key, type_) = do
            (jsVal, get) <- renderInput type_
            dt <- createElement "dt"
            setTextContent dt (JSString.pack (Text.unpack key))
            dd <- createElement "dd"
            replaceChild dd jsVal
            return ([ dt, dd ], key, get)
    triples <- traverse process keyTypes
    dl <- createElement "dl"
    let children = do
            (nodes, _, _) <- triples
            nodes
    replaceChildren dl (Array.fromList children)
    let get = do
            let getWithKey (_, key, getInner) = do
                    value <- getInner
                    return (key, value)
            keyValues <- traverse getWithKey triples
            return (Value.Record (HashMap.fromList keyValues))
    return (dl, get)
renderInput Type.List{ type_ } = do
    -- (jsVal, getInner) <- renderInput type_
    ul <- createElement "ul"
    childrenRef <- IORef.newIORef IntMap.empty
    plus <- createElement "button"
    setAttribute plus "type" "button"
    setAttribute plus "class" "btn btn-primary"
    setTextContent plus "+"
    add <- createElement "li"
    replaceChild add plus
    replaceChild ul add
    insert <- Callback.asyncCallback do
        (jsVal, getInner) <- renderInput type_
        minus <- createElement "button"
        setAttribute minus "type" "button"
        setAttribute minus "class" "btn btn-danger"
        setAttribute minus "display" "inline"
        setTextContent minus "-"
        span <- createElement "span"
        setTextContent span " "
        li <- createElement "li"
        let adapt m = (IntMap.insert n getInner m, n) 
              where
                n = case IntMap.lookupMax m of
                    Nothing -> 0
                    Just (i, _)  -> i + 1
        n <- IORef.atomicModifyIORef childrenRef adapt
        delete <- Callback.asyncCallback do
            IORef.atomicModifyIORef childrenRef (\m -> (IntMap.delete n m, ()))
            remove li
        addEventListener minus "click" delete
        replaceChildren li (Array.fromList [ minus, span, jsVal ])
        before plus li
    addEventListener plus "click" insert
    let get = do
            m <- IORef.readIORef childrenRef
            values <- sequence (IntMap.elems m)
            return (Value.List (Seq.fromList values))
    return (ul, get)

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

    let setOutput type_ value = do
            renderValue output type_ value
            setDisplay error "none"
            setDisplay output "block"

    let interpret = do
            text <- getInput

            if  | Text.null text -> do
                    setError ""
                | otherwise -> do
                    let input_ = Code "(input)" text

                    setError "…"

                    result <- Except.runExceptT (Interpret.interpret input_)

                    case result of
                        Left interpretError -> do
                            setError (Text.pack (displayException interpretError))
                        Right (type_, value) -> do
                            setOutput type_ value

    callback <- Callback.asyncCallback interpret

    addEventListener input "input" callback
