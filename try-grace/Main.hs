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
import Grace.Domain (Domain(..))
import Grace.Input (Input(..))
import Grace.Monotype (RemainingAlternatives(..), RemainingFields(..))
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
renderValue parent Type.Forall{ name, nameLocation, domain = Type, type_ } value = do
    -- If an expression has a polymorphic type, specialize the type to JSON
    let json = Type.Scalar{ location = nameLocation, scalar = Monotype.JSON }

    renderValue parent (Type.substituteType name 0 json type_) value

renderValue parent Type.Forall{ name, domain = Fields, type_ } value = do
    let empty = Type.Fields [] EmptyFields

    renderValue parent (Type.substituteFields name 0 empty type_) value

renderValue parent Type.Forall{ name, domain = Alternatives, type_ } value = do
    let empty = Type.Alternatives [] EmptyAlternatives

    renderValue parent (Type.substituteAlternatives name 0 empty type_) value

renderValue parent Type.Optional{ type_ } value =
    renderValue parent type_ value

renderValue parent _ value@Variable{} = do
    var <- createElement "var"

    setTextContent var (valueToJSString value)

    replaceChild parent var

renderValue parent _ (Value.Scalar (Text text))= do
    span <- createElement "span"

    setTextContent span (JSString.pack (Text.unpack text))

    replaceChild parent span

renderValue parent _ (Value.Scalar (Bool bool)) = do
    input <- createElement "input"

    setAttribute input "type"     "checkbox"
    setAttribute input "class"    "form-check-input"
    setAttribute input "disabled" ""
    Monad.when bool (setAttribute input "checked" "")

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

renderValue parent outer (Value.Record keyValues) = do
    let lookupKey = case outer of
            Type.Record{ fields = Type.Fields keyTypes _ } ->
                \key -> lookup key keyTypes

            Type.Scalar{ scalar = Monotype.JSON } ->
                \_ -> Just outer

            _ ->
                \_ -> Nothing

    let process key value = do
            type_ <- case lookupKey key of
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

    -- Render unions the same as a record with one field
    let recordType = Type.Record
            { location = location outer
            , fields = Type.Fields [(alternative, inner)] EmptyFields
            }

    let recordValue = Value.Record (HashMap.singleton alternative value)

    renderValue parent recordType recordValue

renderValue parent Type.Function{ input, output } function = do
    (inputVal, get) <- renderInput input

    outputVal <- createElement "div"

    let invoke = do
            value <- get

            renderValue outputVal output (Normalize.apply function value)

    callback <- Callback.asyncCallback invoke

    observer <- newObserver callback

    observe observer inputVal

    addEventListener inputVal "input" callback

    invoke

    replaceChildren parent (Array.fromList [ inputVal, outputVal ])

renderValue parent _ value = do
    code <- createElement "code"

    setTextContent code (valueToJSString value)

    replaceChild parent code

renderInput :: Type s -> IO (JSVal, IO Value)
renderInput Type.Scalar{ scalar = Monotype.Bool } = do
    input <- createElement "input"

    setAttribute input "type"  "checkbox"
    setAttribute input "class" "form-check-input"

    let get = do
            bool <- getChecked input

            return (Value.Scalar (Bool bool))

    return (input, get)

renderInput Type.Scalar{ scalar = Monotype.Real } = do
    input <- createElement "input"

    setAttribute input "type"  "number"
    setAttribute input "step"  "any"
    setAttribute input "value" "0"

    let get = do
            double <- getDoubleValue input

            return (Value.Scalar (Real (Scientific.fromFloatDigits double)))

    return (input, get)

renderInput Type.Scalar{ scalar = Monotype.Integer } = do
    input <- createElement "input"

    setAttribute input "type"  "number"
    setAttribute input "value" "0"

    let get = do
            -- NOTE: This does not handle unlimited precision integers
            int <- getIntValue input

            return (Value.Scalar (Integer (fromIntegral int)))

    return (input, get)

renderInput Type.Scalar{ scalar = Monotype.Natural } = do
    input <- createElement "input"

    setAttribute input "type"  "number"
    setAttribute input "value" "0"
    setAttribute input "min"   "0"

    let get = do
            -- NOTE: This does not handle unlimited precision integers
            int <- getIntValue input

            return (Value.Scalar (Natural (fromIntegral int)))

    return (input, get)

renderInput Type.Scalar{ scalar = Monotype.JSON } = do
    input <- createElement "input"

    setAttribute input "value" "null"

    let get = do
            string <- getValue input

            let strictText = Text.pack (JSString.unpack string)

            let lazyText = Text.Lazy.fromStrict strictText

            case Aeson.eitherDecode (Text.Encoding.encodeUtf8 lazyText) of
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
            (fieldVal, get) <- renderInput type_

            dt <- createElement "dt"

            setTextContent dt (JSString.pack (Text.unpack key))

            dd <- createElement "dd"

            replaceChild dd fieldVal

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

renderInput Type.Optional{ type_ } = do
    (nestedVal, getInner) <- renderInput type_

    input <- createElement "input"

    setAttribute input "type"  "checkbox"
    setAttribute input "class" "form-check-input"

    span <- createElement "span"

    setTextContent span " "

    div <- createElement "div"

    replaceChildren div (Array.fromList [input, span, nestedVal])

    let get = do
            bool <- getChecked input

            if  | bool      -> getInner
                | otherwise -> return (Value.Scalar Null)

    return (div, get)

renderInput Type.List{ type_ } = do
    plus <- createElement "button"

    setAttribute plus "type"  "button"
    setAttribute plus "class" "btn btn-primary"

    setTextContent plus "+"

    childrenRef <- IORef.newIORef IntMap.empty

    insert <- Callback.asyncCallback do
        (elementVal, getInner) <- renderInput type_
        minus <- createElement "button"

        setAttribute minus "type"    "button"
        setAttribute minus "class"   "btn btn-danger"
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

        replaceChildren li (Array.fromList [ minus, span, elementVal ])

        before plus li

    addEventListener plus "click" insert

    add <- createElement "li"

    replaceChild add plus

    ul <- createElement "ul"

    replaceChild ul add

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
            setDisplay error  "block"

    let setOutput type_ value = do
            renderValue output type_ value

            setDisplay error  "none"
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
