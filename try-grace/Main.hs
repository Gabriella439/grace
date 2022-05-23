{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf        #-}

module Main where

import Control.Applicative (empty)
import Control.Exception (Exception(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Foldable (toList)
import Data.IORef (IORef)
import Data.JSString (JSString)
import Data.Text (Text)
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
import Numeric.Natural (Natural)
import Prelude hiding (div, error, id, span, subtract)

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans.Maybe as Maybe
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
    getElementById_ :: JSString -> IO JSVal

getElementById :: Text -> IO JSVal
getElementById a = getElementById_ (JSString.pack (Text.unpack a))

foreign import javascript unsafe "$1.value"
    getValue_ :: JSVal -> IO JSString

getValue :: MonadIO io => JSVal -> io Text
getValue a = liftIO (fmap (Text.pack . JSString.unpack) (getValue_ a))

getIntegerValue :: MonadIO io => JSVal -> io Integer
getIntegerValue a = liftIO (fmap (read . JSString.unpack) (getValue_ a))

foreign import javascript unsafe "$1.value"
    getDoubleValue_ :: JSVal -> IO Double

getDoubleValue :: MonadIO io => JSVal -> io Double
getDoubleValue a = liftIO (getDoubleValue_ a)

foreign import javascript unsafe "$1.checked"
    getChecked_ :: JSVal -> IO Bool

getChecked :: MonadIO io => JSVal -> io Bool
getChecked a = liftIO (getChecked_ a)

foreign import javascript unsafe "$1.textContent= $2"
    setTextContent_ :: JSVal -> JSString -> IO ()

setTextContent :: MonadIO io => JSVal -> JSString -> io ()
setTextContent a b = liftIO (setTextContent_ a b)

foreign import javascript unsafe "$1.style.display = $2"
    setDisplay_ :: JSVal -> JSString -> IO ()

setDisplay :: MonadIO io => JSVal -> Text -> io ()
setDisplay a b = liftIO (setDisplay_ a (JSString.pack (Text.unpack b)))

foreign import javascript unsafe "$1.addEventListener($2, $3)"
    addEventListener_ :: JSVal -> JSString -> Callback (IO ()) -> IO ()

addEventListener :: MonadIO io => JSVal -> JSString -> Callback (IO ()) -> io ()
addEventListener a b c = liftIO (addEventListener_ a b c)

foreign import javascript unsafe "document.createElement($1)"
    createElement_ :: JSString -> IO JSVal

createElement :: MonadIO io => JSString -> io JSVal
createElement a = liftIO (createElement_ a)

foreign import javascript unsafe "$1.setAttribute($2,$3)"
    setAttribute_ :: JSVal -> JSString -> JSString -> IO ()

setAttribute :: MonadIO io => JSVal -> JSString -> JSString -> io ()
setAttribute a b c = liftIO (setAttribute_ a b c)

foreign import javascript unsafe "$1.replaceChildren($2)"
    replaceChild_ :: JSVal -> JSVal -> IO ()

replaceChild :: MonadIO io => JSVal -> JSVal -> io ()
replaceChild a b = liftIO (replaceChild_ a b)

foreign import javascript unsafe "new MutationObserver($1)"
    newObserver_ :: Callback (IO ()) -> IO JSVal

newObserver :: MonadIO io => Callback (IO ()) -> io JSVal
newObserver a = liftIO (newObserver_ a)

foreign import javascript unsafe "$1.observe($2, { childList: true, subtree: true })"
    observe_ :: JSVal -> JSVal -> IO ()

observe :: MonadIO io => JSVal -> JSVal -> io ()
observe a b = liftIO (observe_ a b)

-- @$1.replaceChildren(...$2)@ does not work because GHCJS fails to parse the
-- spread operator, we work around this by defining the
-- @replaceChildrenWorkaround@ function in JavaScript which takes care of the
-- spread operator for us
foreign import javascript unsafe "replaceChildrenWorkaround($1, $2)"
    replaceChildren_ :: JSVal -> JSArray -> IO ()

replaceChildren :: MonadIO io => JSVal -> JSArray -> io ()
replaceChildren a b = liftIO (replaceChildren_ a b)

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

renderValue :: IORef Natural -> JSVal -> Type s -> Value -> IO ()
renderValue ref parent Type.Forall{ name, nameLocation, domain = Type, type_ } value = do
    -- If an expression has a polymorphic type, specialize the type to JSON
    let json = Type.Scalar{ location = nameLocation, scalar = Monotype.JSON }

    renderValue ref parent (Type.substituteType name 0 json type_) value

renderValue ref parent Type.Forall{ name, domain = Fields, type_ } value = do
    let empty_ = Type.Fields [] EmptyFields

    renderValue ref parent (Type.substituteFields name 0 empty_ type_) value

renderValue ref parent Type.Forall{ name, domain = Alternatives, type_ } value = do
    let empty_ = Type.Alternatives [] EmptyAlternatives

    renderValue ref parent (Type.substituteAlternatives name 0 empty_ type_) value

renderValue ref parent Type.Optional{ type_ } value =
    renderValue ref parent type_ value

renderValue _ parent _ value@Variable{} = do
    var <- createElement "var"

    setTextContent var (valueToJSString value)

    replaceChild parent var

renderValue _ parent _ (Value.Scalar (Text text))= do
    p <- createElement "p"

    setTextContent p (JSString.pack (Text.unpack text))

    replaceChild parent p

renderValue _ parent _ (Value.Scalar (Bool bool)) = do
    input <- createElement "input"

    setAttribute input "type"     "checkbox"
    setAttribute input "class"    "form-check-input"
    setAttribute input "disabled" ""

    Monad.when bool (setAttribute input "checked" "")

    replaceChild parent input

renderValue _ parent _ (Value.Scalar Null) = do
    span <- createElement "span"

    setTextContent span "∅"

    replaceChild parent span

renderValue _ parent _ value@Value.Scalar{} = do
    span <- createElement "span"

    setTextContent span (valueToJSString value)

    replaceChild parent span

renderValue ref parent outer (Value.List values) = do
    inner <- case outer of
            Type.List{ type_ } -> do
                return type_

            Type.Scalar{ scalar = Monotype.JSON } -> do
                return outer

            _ -> do
                fail "renderValue: Missing element type"

    lis <- forM values \value -> do
        li <- createElement "li"

        renderValue ref li inner value

        return li

    ul <- createElement "ul"

    replaceChildren ul (Array.fromList (toList lis))

    replaceChild parent ul

renderValue ref parent outer (Value.Record keyValues) = do
    let lookupKey = case outer of
            Type.Record{ fields = Type.Fields keyTypes _ } ->
                \key -> lookup key keyTypes

            Type.Scalar{ scalar = Monotype.JSON } ->
                \_ -> pure outer

            _ ->
                \_ -> empty

    let process key value = do
            type_ <- case lookupKey key of
                Nothing    -> fail "renderValue: Missing field type"
                Just type_ -> return type_

            dt <- createElement "dt"

            setTextContent dt (JSString.pack (Text.unpack key))

            dd <- createElement "dd"

            renderValue ref dd type_ value

            return [ dt, dd ]

    dtds <- HashMap.traverseWithKey process keyValues

    dl <- createElement "dl"

    replaceChildren dl (Array.fromList (toList (concat dtds)))

    replaceChild parent dl

renderValue ref parent outer (Application (Value.Alternative alternative) value) = do
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

    renderValue ref parent recordType recordValue

renderValue ref parent Type.Function{ input, output } function = do
    result <- Maybe.runMaybeT (renderInput ref input)

    case result of
        Nothing -> do
            renderDefault parent function
        Just (inputVal, get) -> do
            outputVal <- createElement "div"

            let invoke = do
                    value <- get

                    renderValue ref outputVal output (Normalize.apply function value)

            callback <- Callback.asyncCallback invoke

            observer <- newObserver callback

            observe observer inputVal

            addEventListener inputVal "input" callback

            invoke

            replaceChildren parent (Array.fromList [ inputVal, outputVal ])

renderValue _ parent _ value = do
    renderDefault parent value

renderDefault :: JSVal -> Value -> IO ()
renderDefault parent value = do
    code <- createElement "code"

    setTextContent code (valueToJSString value)

    replaceChild parent code

renderInput :: IORef Natural -> Type s -> MaybeT IO (JSVal, IO Value)
renderInput ref Type.Exists{ name, nameLocation, domain = Type, type_ } = do
    -- If an expression has an existential type, specialize the type to { }
    let unit = Type.Record
            { location = nameLocation
            , fields = Type.Fields [] EmptyFields
            }

    renderInput ref (Type.substituteType name 0 unit type_)

renderInput ref Type.Exists{ name, domain = Fields, type_ } = do
    let empty_ = Type.Fields [] EmptyFields

    renderInput ref (Type.substituteFields name 0 empty_ type_)

renderInput ref Type.Exists{ name, domain = Alternatives, type_ } = do
    let empty_ = Type.Alternatives [] EmptyAlternatives

    renderInput ref (Type.substituteAlternatives name 0 empty_ type_)

renderInput _ Type.Scalar{ scalar = Monotype.Bool } = do
    input <- createElement "input"

    setAttribute input "type"  "checkbox"
    setAttribute input "class" "form-check-input"

    span <- createElement "span"

    setAttribute span "class" "form-check"

    replaceChild span input

    let get = do
            bool <- getChecked input

            return (Value.Scalar (Bool bool))

    return (span, get)

renderInput _ Type.Scalar{ scalar = Monotype.Real } = do
    input <- createElement "input"

    setAttribute input "type"  "number"
    setAttribute input "step"  "any"
    setAttribute input "value" "0"

    let get = do
            double <- getDoubleValue input

            return (Value.Scalar (Real (Scientific.fromFloatDigits double)))

    return (input, get)

renderInput _ Type.Scalar{ scalar = Monotype.Integer } = do
    input <- createElement "input"

    setAttribute input "type"  "number"
    setAttribute input "value" "0"

    let get = do
            integer <- getIntegerValue input

            return (Value.Scalar (Integer integer))

    return (input, get)

renderInput _ Type.Scalar{ scalar = Monotype.Natural } = do
    input <- createElement "input"

    setAttribute input "type"  "number"
    setAttribute input "value" "0"
    setAttribute input "min"   "0"

    let get = do
            integer <- getIntegerValue input

            return (Value.Scalar (Natural (fromInteger integer)))

    return (input, get)

renderInput _ Type.Scalar{ scalar = Monotype.JSON } = do
    input <- createElement "input"

    setAttribute input "value" "null"

    let get = do
            strictText <- getValue input

            let lazyText = Text.Lazy.fromStrict strictText

            case Aeson.eitherDecode (Text.Encoding.encodeUtf8 lazyText) of
                Left _ -> do
                    setAttribute input "class" "form-control is-invalid"

                    return (Value.Scalar Null)

                Right value -> do
                    setAttribute input "class" "form-control is-valid"

                    return value

    return (input, get)

renderInput _ Type.Scalar{ scalar = Monotype.Text } = do
    textarea <- createElement "textarea"

    let get = do
            text <- getValue textarea

            return (Value.Scalar (Text text))

    return (textarea, get)

renderInput ref Type.Record{ fields = Type.Fields keyTypes _ } = do
    let process (key, type_) = do
            (fieldVal, get) <- renderInput ref type_

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

renderInput ref Type.Union{ alternatives = Type.Alternatives keyTypes _ }
    | not (null keyTypes) = do
        n <- liftIO (IORef.atomicModifyIORef ref (\a -> (a + 1, a)))

        let process (checked, (key, type_)) = do
                (nestedVal, nestedGet) <- renderInput ref type_

                input <- createElement "input"

                let keyString = JSString.pack (Text.unpack key)

                let name = "radio" <> JSString.pack (show n)

                let id = name <> "-" <> keyString

                setAttribute input "class" "form-check-input"
                setAttribute input "type"  "radio"
                setAttribute input "name"  name
                setAttribute input "id"    id

                Monad.when checked (setAttribute input "checked" "")

                label <- createElement "label"

                setAttribute label "class" "form-check-label"
                setAttribute label "for"   id

                setTextContent label keyString

                span <- createElement "span"

                setTextContent span " "

                div <- createElement "div"

                setAttribute div "class" "form-check"

                replaceChildren div (Array.fromList [ input, label, span, nestedVal ])

                let get = do
                        value <- nestedGet

                        return (Application (Alternative key) value)

                return (div, getChecked input, get)

        triples <- traverse process (zip (True : repeat False) keyTypes)

        div <- createElement "div"

        let children = do
                (node, _, _) <- triples

                return node

        replaceChildren div (Array.fromList children)

        let loop [] = do
                fail "renderInput: No radio button is enabled"
            loop ((_, checkEnabled, getNested) : rest) = do
                enabled <- checkEnabled
                if  | enabled -> do
                        getNested
                    | otherwise -> do
                        loop rest

        let get = loop triples

        return (div, get)

renderInput ref Type.Optional{ type_ } = do
    (nestedVal, getInner) <- renderInput ref type_

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

renderInput ref Type.List{ type_ } = do
    -- Do a test renderInput to verify that it won't fail later on within the
    -- async callback
    _ <- renderInput ref type_

    plus <- createElement "button"

    setAttribute plus "type"  "button"
    setAttribute plus "class" "btn btn-primary"

    setTextContent plus "+"

    childrenRef <- liftIO (IORef.newIORef IntMap.empty)

    insert <- (liftIO . Callback.asyncCallback) do
        Just (elementVal, getInner) <- Maybe.runMaybeT (renderInput ref type_)
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

renderInput _ _ = do
    empty

main :: IO ()
main = do
    input  <- getElementById "input"
    output <- getElementById "output"
    error  <- getElementById "error"

    ref <- IORef.newIORef 0

    let setError text = do
            setTextContent error (JSString.pack (Text.unpack text))

            setDisplay output "none"
            setDisplay error  "block"

    let setOutput type_ value = do
            renderValue ref output type_ value

            setDisplay error  "none"
            setDisplay output "block"

    let interpret = do
            text <- getValue input

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
