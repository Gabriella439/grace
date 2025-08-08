{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Commonmark (Html)
import Control.Applicative (empty)
import Control.Concurrent.Async (Async)
import Control.Exception.Safe (Exception(..), SomeException)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Foldable (toList, traverse_)
import Data.IORef (IORef)
import Data.JSString (JSString)
import Data.Text (Text)
import Data.Traversable (forM)
import Grace.Type (Type(..))
import GHCJS.Foreign.Callback (Callback)
import GHCJS.Types (JSVal)
import Grace.Domain (Domain(..))
import Grace.HTTP (Methods)
import Grace.Input (Input(..))
import Grace.Location (Location)
import Grace.Monotype (RemainingAlternatives(..), RemainingFields(..))
import Grace.Syntax (Scalar(..))
import Grace.Value (Value(..))
import JavaScript.Array (JSArray)
import Numeric.Natural (Natural)
import Prelude hiding (div, error, id, span, subtract)
import System.FilePath ((</>))

import qualified Commonmark
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Exception.Safe as Exception
import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import qualified Control.Monad.State as State
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
import qualified Grace.DataFile as DataFile
import qualified Grace.HTTP as HTTP
import qualified Grace.Import as Import
import qualified Grace.Infer as Infer
import qualified Grace.Interpret as Interpret
import qualified Grace.Monotype as Monotype
import qualified Grace.Normalize as Normalize
import qualified Grace.Pretty as Pretty
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type
import qualified Grace.Value as Value
import qualified JavaScript.Array as Array
import qualified Network.URI.Encode as URI.Encode

foreign import javascript unsafe "document.getElementById($1)"
    getElementById_ :: JSString -> IO JSVal

getElementById :: MonadIO io => Text -> io JSVal
getElementById a = liftIO (getElementById_ (fromText a))

foreign import javascript unsafe "$1.value"
    toValue_ :: JSVal -> IO JSString

toValue :: MonadIO io => JSVal -> io Text
toValue a = liftIO (fmap toText (toValue_ a))

toIntegerValue :: MonadIO io => JSVal -> io Integer
toIntegerValue a = liftIO (fmap (read . JSString.unpack) (toValue_ a))

foreign import javascript unsafe "$1.value"
    toDoubleValue_ :: JSVal -> IO Double

toDoubleValue :: MonadIO io => JSVal -> io Double
toDoubleValue a = liftIO (toDoubleValue_ a)

foreign import javascript unsafe "$1.checked"
    getChecked_ :: JSVal -> IO Bool

getChecked :: MonadIO io => JSVal -> io Bool
getChecked a = liftIO (getChecked_ a)

foreign import javascript unsafe "$1.textContent = $2"
    setTextContent_ :: JSVal -> JSString -> IO ()

setTextContent :: MonadIO io => JSVal -> Text -> io ()
setTextContent a b = liftIO (setTextContent_ a (fromText b))

foreign import javascript unsafe "$1.innerText = $2"
    setInnerText_ :: JSVal -> JSString -> IO ()

setInnerText :: MonadIO io => JSVal -> Text -> io ()
setInnerText a b = liftIO (setInnerText_ a (fromText b))

foreign import javascript unsafe "$1.innerHTML = $2"
    setInnerHTML_ :: JSVal -> JSString -> IO ()

setInnerHTML :: MonadIO io => JSVal -> Text -> io ()
setInnerHTML a b = liftIO (setInnerHTML_ a (fromText b))

foreign import javascript unsafe "$1.style.display = $2"
    setDisplay_ :: JSVal -> JSString -> IO ()

setDisplay :: MonadIO io => JSVal -> Text -> io ()
setDisplay a b = liftIO (setDisplay_ a (fromText b))

foreign import javascript unsafe "$1.addEventListener($2, $3)"
    addEventListener_ :: JSVal -> JSString -> Callback (IO ()) -> IO ()

addEventListener :: MonadIO io => JSVal -> Text -> Callback (IO ()) -> io ()
addEventListener a b c = liftIO (addEventListener_ a (fromText b) c)

foreign import javascript unsafe "autoResize($1)"
    autoResize_ :: JSVal -> IO ()

autoResize :: MonadIO io => JSVal -> io ()
autoResize a = liftIO (autoResize_ a)

foreign import javascript unsafe "document.createElement($1)"
    createElement_ :: JSString -> IO JSVal

createElement :: MonadIO io => Text -> io JSVal
createElement a = liftIO (createElement_ (fromText a))

foreign import javascript unsafe "$1.setAttribute($2,$3)"
    setAttribute_ :: JSVal -> JSString -> JSString -> IO ()

setAttribute :: MonadIO io => JSVal -> Text -> Text -> io ()
setAttribute a b c = liftIO (setAttribute_ a (fromText b) (fromText c))

foreign import javascript unsafe "$1.disabled = $2"
    setDisabled_ :: JSVal -> Bool -> IO ()

setDisabled :: MonadIO io => JSVal -> Bool -> io ()
setDisabled a b = liftIO (setDisabled_ a b)

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

foreign import javascript unsafe "(new URL(document.location)).searchParams"
    getSearchParams_ :: IO JSVal

getSearchParams :: MonadIO io => io JSVal
getSearchParams = liftIO getSearchParams_

foreign import javascript unsafe "$1.has($2)"
    hasParam_ :: JSVal -> JSString -> IO Bool

hasParam :: MonadIO io => JSVal -> Text -> io Bool
hasParam a b = liftIO (hasParam_ a (fromText b))

foreign import javascript unsafe "$1.get($2)"
    getParam_ :: JSVal -> JSString -> IO JSString

getParam :: MonadIO io => JSVal -> Text -> io Text
getParam a b = liftIO (fmap toText (getParam_ a (fromText b)))

foreign import javascript unsafe "$1.set($2,$3)"
    setParam_ :: JSVal -> JSString -> JSString -> IO ()

setParam :: MonadIO io => JSVal -> Text -> Text -> io ()
setParam a b c = liftIO (setParam_ a (fromText b) (fromText c))

-- @$1.delete($2)@ doesn't work because GHCJS treats delete as a forbidden
-- reserved keyword, so we work around this by defining the
-- @deleteSearchParamWorkaround@ function in JavaScript which takes care of this
-- for us
foreign import javascript unsafe "deleteSearchParamWorkaround($1, $2)"
    deleteParam_ :: JSVal -> JSString -> IO ()

deleteParam :: MonadIO io => JSVal -> Text -> io ()
deleteParam a b = liftIO (deleteParam_ a (fromText b))

foreign import javascript unsafe "history.replaceState(null, null, '?'+$1.toString())"
  saveSearchParams_ :: JSVal -> IO ()

saveSearchParams :: MonadIO io => JSVal -> io ()
saveSearchParams a = liftIO (saveSearchParams_ a)

-- @$1.replaceChildren(...$2)@ does not work because GHCJS fails to parse the
-- spread operator, so we work around this by defining the
-- @replaceChildrenWorkaround@ function in JavaScript which takes care of the
-- spread operator for us
foreign import javascript unsafe "replaceChildrenWorkaround($1, $2)"
    replaceChildren_ :: JSVal -> JSArray -> IO ()

replaceChildren :: MonadIO io => JSVal -> JSArray -> io ()
replaceChildren a b = liftIO (replaceChildren_ a b)

foreign import javascript unsafe "$1.before($2)"
    before_ :: JSVal -> JSVal -> IO ()

before :: MonadIO io => JSVal -> JSVal -> io ()
before a b = liftIO (before_ a b)

foreign import javascript unsafe "$1.after($2)"
    after_ :: JSVal -> JSVal -> IO ()

after :: MonadIO io => JSVal -> JSVal -> io ()
after a b = liftIO (after_ a b)

foreign import javascript unsafe "$1.remove()"
    remove_ :: JSVal -> IO ()

remove :: MonadIO io => JSVal -> io ()
remove a = liftIO (remove_ a)

foreign import javascript unsafe "CodeMirror.fromTextArea($1, { mode: 'python', lineNumbers: true, viewportMargin: Infinity, extraKeys: { Tab: false } })"
    setupCodemirrorInput_ :: JSVal -> IO JSVal

setupCodemirrorInput :: MonadIO io => JSVal -> io JSVal
setupCodemirrorInput a = liftIO (setupCodemirrorInput_ a)

foreign import javascript unsafe "CodeMirror.fromTextArea($1, { mode: 'python', lineNumbers: false, viewportMargin: Infinity, readOnly: true })"
    setupCodemirrorOutput_ :: JSVal -> IO JSVal

setupCodemirrorOutput :: MonadIO io => JSVal -> io JSVal
setupCodemirrorOutput a = liftIO (setupCodemirrorOutput_ a)

foreign import javascript unsafe "$1.refresh()"
    refresh_ :: JSVal -> IO ()

refresh :: MonadIO io => JSVal -> io ()
refresh a = liftIO (refresh_ a)

foreign import javascript unsafe "$1.getWrapperElement()"
    getWrapperElement :: JSVal -> JSVal

foreign import javascript unsafe "$1.on('change', $2)"
    onChange_ :: JSVal -> Callback (IO ()) -> IO ()

onChange :: MonadIO io => JSVal -> Callback (IO ()) -> io ()
onChange a b = liftIO (onChange_ a b)

foreign import javascript unsafe "$1.setValue($2)"
    setValue_ :: JSVal -> JSString -> IO ()

setValue :: MonadIO io => JSVal -> Text -> io ()
setValue a b = liftIO (setValue_ a (fromText b))

foreign import javascript unsafe "$1.getValue()"
    getValue_ :: JSVal -> IO JSString

getValue :: MonadIO io => JSVal -> io Text
getValue a = liftIO (fmap toText (getValue_ a))

foreign import javascript unsafe "document.getElementsByClassName($1)"
    getElementsByClassName_ :: JSString -> IO JSArray

getElementsByClassName :: MonadIO io => Text -> io [JSVal]
getElementsByClassName a =
    fmap Array.toList (liftIO (getElementsByClassName_ (fromText a)))

foreign import javascript unsafe "$1.classList.remove($2)"
    removeClass_ :: JSVal -> JSString -> IO ()

removeClass :: MonadIO io => JSVal -> Text -> io ()
removeClass a b = liftIO (removeClass_ a (fromText b))

foreign import javascript unsafe "$1.classList.add($2)"
    addClass_ :: JSVal -> JSString -> IO ()

addClass :: MonadIO io => JSVal -> Text -> io ()
addClass a b = liftIO (addClass_ a (fromText b))

foreign import javascript unsafe "$1.focus()"
    focus_ :: JSVal -> IO ()

focus :: MonadIO io => JSVal -> io ()
focus a = liftIO (focus_ a)

toText :: JSString -> Text
toText = Text.pack . JSString.unpack

fromText :: Text -> JSString
fromText = JSString.pack . Text.unpack

typeToText :: Type s -> Text
typeToText = Pretty.renderStrict False 80

valueToText :: Value -> Text
valueToText = Pretty.renderStrict False 80 . Normalize.strip . Normalize.quote

renderValue
    :: (Text -> Methods)
    -> IORef Natural
    -> JSVal
    -> Type Location
    -> Value
    -> IO (IO ())
renderValue keyToMethods ref parent Type.Forall{ name, nameLocation, domain = Type, type_ } value = do
    -- If an expression has a polymorphic type, specialize the type to Text
    let text = Type.Scalar{ location = nameLocation, scalar = Monotype.Text }

    renderValue keyToMethods ref parent (Type.substituteType name 0 text type_) value

renderValue keyToMethods ref parent Type.Forall{ name, domain = Fields, type_ } value = do
    let empty_ = Type.Fields [] EmptyFields

    renderValue keyToMethods ref parent (Type.substituteFields name 0 empty_ type_) value

renderValue keyToMethods ref parent Type.Forall{ name, domain = Alternatives, type_ } value = do
    let empty_ = Type.Alternatives [] EmptyAlternatives

    renderValue keyToMethods ref parent (Type.substituteAlternatives name 0 empty_ type_) value

renderValue keyToMethods ref parent Type.Optional{ type_ } value =
    renderValue keyToMethods ref parent type_ value

renderValue _ _ parent _ (Value.Text text) = do
    case Commonmark.commonmark "(input)" text of
            Left _ ->
                setInnerText parent text
            Right html ->
                setInnerHTML parent (Text.Lazy.toStrict (Commonmark.renderHtml (html :: Html ())))

    mempty

renderValue _ _ parent _ (Value.Scalar (Bool bool)) = do
    input <- createElement "input"

    setAttribute input "type"     "checkbox"
    setAttribute input "class"    "form-check-input"
    setDisabled input True

    Monad.when bool (setAttribute input "checked" "")

    replaceChild parent input

    mempty

renderValue _ _ parent _ (Value.Scalar Null) = do
    span <- createElement "span"

    setAttribute span "class" "fira"

    setTextContent span "∅"

    replaceChild parent span

    mempty

renderValue _ _ parent _ value@Value.Scalar{} = do
    span <- createElement "span"

    setTextContent span (valueToText value)

    setAttribute span "class" "fira"
    setAttribute span "style" "whitespace: pre"

    replaceChild parent span

    mempty

renderValue keyToMethods ref parent outer (Value.List values) = do
    inner <- case outer of
            Type.List{ type_ } -> do
                return type_

            Type.Scalar{ scalar = Monotype.JSON } -> do
                return outer

            _ -> do
                fail "renderValue: Missing element type"

    pairs <- forM values \value -> do
        li <- createElement "li"

        refreshInner <- renderValue keyToMethods ref li inner value

        return (li, refreshInner)

    let lis = fmap fst pairs

    let refreshInput = sequence_ (fmap snd pairs)

    ul <- createElement "ul"

    replaceChildren ul (Array.fromList (toList lis))

    replaceChild parent ul

    return refreshInput

renderValue keyToMethods ref parent outer (Value.Record keyValues) = do
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

            setAttribute dt "class" "col-auto"

            case value of
                Value.Record kvs | HashMap.null kvs -> do
                    setTextContent dt key
                Value.List xs | Seq.null xs -> do
                    setTextContent dt key
                _ -> do
                    setTextContent dt (key <> ":")

            dd <- createElement "dd"

            setAttribute dd "class" "col"

            refreshInner <- renderValue keyToMethods ref dd type_ value

            dl <- createElement "dl"

            setAttribute dl "class" "row"

            replaceChildren dl (Array.fromList [ dt, dd ])

            return (dl, refreshInner)

    pairs <- HashMap.traverseWithKey process keyValues

    let dls = fmap fst pairs

    let refreshInput = sequence_ (fmap snd pairs)

    replaceChildren parent (Array.fromList (HashMap.elems dls))

    return refreshInput

renderValue keyToMethods ref parent outer (Application (Value.Builtin Syntax.Some) value) = do
    renderValue keyToMethods ref parent outer value

renderValue keyToMethods ref parent outer (Application (Value.Alternative alternative) value) = do
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

    renderValue keyToMethods ref parent recordType recordValue

renderValue keyToMethods ref parent Type.Function{ input, output } function = do
    maybeResult <- Maybe.runMaybeT (renderInput keyToMethods ref input)

    case maybeResult of
        Nothing -> do
            renderDefault parent function
        Just (inputVal, get, refreshInput) -> do
            hr <- createElement "hr"

            outputVal <- createElement "div"

            (setBusy, setSuccess, setError) <- createForm False outputVal

            let render value = do
                    setBusy

                    eitherResult <- (liftIO . Exception.try) do
                        newValue <- Normalize.apply keyToMethods function value

                        refreshOutput <- setSuccess output newValue \htmlWrapper -> do
                            renderValue keyToMethods ref htmlWrapper output newValue

                        refreshOutput

                    case eitherResult of
                        Left exception -> do
                            setError (Text.pack (displayException (exception :: SomeException)))

                        Right r -> do
                            return r

            debouncedRender <- debounce render

            let invoke = do
                    maybeValue <- Maybe.runMaybeT get

                    traverse_ debouncedRender maybeValue

            callback <- Callback.asyncCallback invoke

            if Lens.has Value.effects function
                then do
                    button <- createElement "button"

                    setAttribute button "type"  "button"
                    setAttribute button "class" "btn btn-primary"

                    setTextContent button "Submit"

                    addEventListener button "click" callback

                    replaceChildren parent (Array.fromList [ inputVal, button, hr, outputVal ])

                else do
                    observer <- newObserver callback

                    observe observer inputVal

                    addEventListener inputVal "input" callback

                    replaceChildren parent (Array.fromList [ inputVal, hr, outputVal ])

                    invoke

            return refreshInput

-- At the time of this writing this case should (in theory) never be hit,
-- because all of the `Value` constructors are either explicitly handled (e.g.
-- `Text` / `Scalar`) or handled by the case for `Type.Function` (e.g. `Builtin`
-- / `Alternative`)
renderValue _ _ parent _ value = do
    renderDefault parent value

renderDefault :: JSVal -> Value -> IO (IO ())
renderDefault parent value = do
    code <- createElement "code"

    setTextContent code (valueToText value)

    replaceChild parent code

    mempty

renderInput
    :: (Text -> Methods)
    -> IORef Natural
    -> Type Location
    -> MaybeT IO (JSVal, MaybeT IO Value, IO ())
renderInput _ _ Type.Scalar{ scalar = Monotype.Bool } = do
    input <- createElement "input"

    setAttribute input "type"  "checkbox"
    setAttribute input "class" "form-check-input"

    span <- createElement "span"

    setAttribute span "class" "form-check"
    setAttribute span "style" "display: inline-block !important;"

    replaceChild span input

    let get = do
            bool <- getChecked input

            return (Value.Scalar (Bool bool))

    return (span, get, mempty)

renderInput _ _ Type.Scalar{ scalar = Monotype.Real } = do
    input <- createElement "input"

    setAttribute input "class" "form-control"
    setAttribute input "type"  "number"
    setAttribute input "step"  "any"
    setAttribute input "value" "0"

    let get = do
            double <- toDoubleValue input

            return (Value.Scalar (Real (Scientific.fromFloatDigits double)))

    return (input, get, mempty)

renderInput _ _ Type.Scalar{ scalar = Monotype.Integer } = do
    input <- createElement "input"

    setAttribute input "class" "form-control"
    setAttribute input "type"  "number"
    setAttribute input "value" "0"

    let get = do
            integer <- toIntegerValue input

            return (Value.Scalar (Integer integer))

    return (input, get, mempty)

renderInput _ _ Type.Scalar{ scalar = Monotype.Natural } = do
    input <- createElement "input"

    setAttribute input "class" "form-control"
    setAttribute input "type"  "number"
    setAttribute input "value" "0"
    setAttribute input "min"   "0"

    let get = do
            integer <- toIntegerValue input

            return (Value.Scalar (Natural (fromInteger integer)))

    return (input, get, mempty)

renderInput _ _ Type.Scalar{ scalar = Monotype.JSON } = do
    input <- createElement "input"

    setAttribute input "class" "form-control"
    setAttribute input "value" "null"

    let get = do
            strictText <- toValue input

            let lazyText = Text.Lazy.fromStrict strictText

            case Aeson.eitherDecode (Text.Encoding.encodeUtf8 lazyText) of
                Left _ -> do
                    setAttribute input "class" "form-control is-invalid"

                    empty

                Right value -> do
                    setAttribute input "class" "form-control is-valid"

                    return value

    return (input, get, mempty)

renderInput _ _ Type.Scalar{ scalar = Monotype.Text } = do
    input <- createElement "textarea"

    setAttribute input "class" "form-control"
    setAttribute input "rows" "1"

    autoResize input

    let get = do
            text <- toValue input

            return (Value.Text text)

    return (input, get, mempty)

renderInput _ _ Type.Scalar{ scalar = Monotype.Key } = do
    input <- createElement "input"

    setAttribute input "type" "password"
    setAttribute input "class" "form-control"
    setAttribute input "rows" "1"

    let get = do
            key <- toValue input

            return (Value.Scalar (Key key))

    return (input, get, mempty)

renderInput keyToMethods ref Type.Record{ fields = Type.Fields keyTypes _ } = do
    let process (key, type_) = do
            (fieldVal, get, refreshInner) <- renderInput keyToMethods ref type_

            dt <- createElement "dt"

            setAttribute dt "class" "col-auto"

            setTextContent dt (key <> ":")

            dd <- createElement "dd"

            setAttribute dd "class" "col"

            replaceChild dd fieldVal

            dl <- createElement "dl"

            setAttribute dl "class" "row"

            replaceChildren dl (Array.fromList [ dt, dd ])

            return (dl, key, get, refreshInner)

    quartets <- traverse process keyTypes

    let children = do
            (dl, _, _, _) <- quartets

            return dl

    div <- createElement "div"

    replaceChildren div (Array.fromList children)

    let get = do
            let getWithKey (_, key, getInner, _) = do
                    value <- getInner

                    return (key, value)

            keyValues <- traverse getWithKey quartets

            return (Value.Record (HashMap.fromList keyValues))

    let refreshInput = sequence_ do
            (_, _, _, refreshInner) <- quartets

            return refreshInner

    return (div, get, refreshInput)

renderInput keyToMethods ref Type.Union{ alternatives = Type.Alternatives keyTypes _ }
    | not (null keyTypes) = do
        n <- liftIO (IORef.atomicModifyIORef ref (\a -> (a + 1, a)))

        let process (checked, (key, type_)) = do
                (nestedVal, nestedGet, refreshInner) <- renderInput keyToMethods ref type_

                input <- createElement "input"

                let name = "radio" <> Text.pack (show n)

                let id = name <> "-" <> key

                setAttribute input "class" "form-check-input"
                setAttribute input "type"  "radio"
                setAttribute input "name"  name
                setAttribute input "id"    id

                Monad.when checked (setAttribute input "checked" "")

                label <- createElement "label"

                setAttribute label "class" "form-check-label"
                setAttribute label "for"   id

                case type_ of
                    Type.Record{ fields = Type.Fields kts _ } | null kts -> do
                        setTextContent label key
                    _ -> do
                        setTextContent label (key <> ":")


                fieldset <- createElement "fieldset"

                setDisabled fieldset (not checked)

                replaceChild fieldset nestedVal

                div <- createElement "div"

                setAttribute div "class" "form-check"

                replaceChildren div (Array.fromList [ input, label, fieldset ])

                let get = do
                        value <- nestedGet

                        return (Application (Alternative key) value)

                return (div, fieldset, getChecked input, get, refreshInner)

        quintets <- traverse process (zip (True : repeat False) keyTypes)

        div <- createElement "div"

        let children = do
                (node, _, _, _, _) <- quintets

                return node

        replaceChildren div (Array.fromList children)

        let loop get [] = do
                get
            loop get ((_, fieldset, checkEnabled, getNested, _) : rest) = do
                checked <- checkEnabled

                setDisabled fieldset (not checked)

                if  | checked -> do
                        loop getNested rest
                    | otherwise -> do
                        loop get rest

        let get = loop empty quintets

        let refreshInput = sequence_ do
                (_, _, _, _, refreshInner) <- quintets

                return refreshInner

        return (div, get, refreshInput)

renderInput keyToMethods ref Type.Optional{ type_ } = do
    (nestedVal, getInner, refreshInner) <- renderInput keyToMethods ref type_

    input <- createElement "input"

    setAttribute input "type"  "checkbox"
    setAttribute input "class" "form-check-input"

    div <- createElement "div"

    fieldset <- createElement "fieldset"

    replaceChild fieldset nestedVal

    replaceChildren div (Array.fromList [input, fieldset])

    let get = do
            checked <- getChecked input

            setDisabled fieldset (not checked)

            if  | checked -> do
                    value <- getInner

                    return (Application (Value.Builtin Syntax.Some) value)
                | otherwise -> do
                    return (Value.Scalar Null)

    return (div, get, refreshInner)

renderInput keyToMethods ref Type.List{ type_ } = do
    -- Do a test renderInput to verify that it won't fail later on within the
    -- async callback
    _ <- renderInput keyToMethods ref type_

    plus <- createElement "button"

    setAttribute plus "type"  "button"
    setAttribute plus "class" "btn btn-primary"

    setTextContent plus "+"

    add <- createElement "li"

    replaceChild add plus

    childrenRef <- liftIO (IORef.newIORef IntMap.empty)

    insert <- (liftIO . Callback.asyncCallback) do
        Just (elementVal, getInner, refreshInner) <- Maybe.runMaybeT (renderInput keyToMethods ref type_)

        minus <- createElement "button"

        setAttribute minus "type"    "button"
        setAttribute minus "class"   "btn btn-danger"
        setAttribute minus "display" "inline"

        setTextContent minus "-"

        span <- createElement "span"

        setTextContent span " "

        li <- createElement "li"

        let adapt m = (IntMap.insert n (getInner, refreshInner)  m, n)
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

        before add li

    addEventListener plus "click" insert

    ul <- createElement "ul"

    setAttribute ul "class" "list-unstyled"

    replaceChild ul add

    let get = do
            m <- liftIO (IORef.readIORef childrenRef)

            values <- sequence do
                (getInner, _) <- IntMap.elems m

                return getInner

            return (Value.List (Seq.fromList values))

    let refreshInput = do
            m <- liftIO (IORef.readIORef childrenRef)

            sequence_ do
                (_, refreshInner) <- IntMap.elems m

                return refreshInner

    return (ul, get, refreshInput)

renderInput keyToMethods _ type_ = do
    textarea <- createElement "textarea"

    setDisplay textarea "none"

    error <- createElement "pre"

    setDisplay error "none"

    div <- createElement "div"

    replaceChildren div (Array.fromList [ textarea, error ])

    codeInput <- setupCodemirrorInput textarea

    let get = do
            text <- getValue codeInput

            let input_ = Code "(input)" text

            result <- (liftIO . Exception.try) do
                (_, value) <- Interpret.interpretWith keyToMethods [] (Just type_) input_

                return value

            case result of
                Left exception -> do
                    if (text == "")
                        then do
                            setDisplay error "none"
                        else do
                            setTextContent error (Text.pack (displayException (exception :: SomeException)))

                            setDisplay error "block"

                    empty

                Right value -> do
                    setDisplay error "none"

                    setTextContent error ""

                    return value

    return (div, get, refresh codeInput)

data DebounceStatus = Ready | Lock | Running (Async ())

debounce :: (a -> IO ()) -> IO (a -> IO ())
debounce f = do
    tvar <- TVar.newTVarIO Ready

    return \args -> do
        m <- STM.atomically do
            status <- TVar.readTVar tvar

            case status of
                Ready -> do
                    TVar.writeTVar tvar Lock
                    return Nothing
                Lock -> do
                    empty
                Running async -> do
                    return (Just async)

        traverse_ Async.cancel m

        async <- Async.async (f args)

        STM.atomically (TVar.writeTVar tvar (Running async))

        Async.wait async

        STM.atomically (TVar.writeTVar tvar Ready)

createForm
    :: Bool
    -- ^ Show tabs?
    -> JSVal
    -> IO
        ( IO ()
        , Type Location -> Value -> (JSVal -> IO (IO ())) -> IO (IO ())
        , Text -> IO ()
        )
createForm showTabs output = do
    let toTab name = do
            link <- createElement "a"
            setAttribute link "class" "nav-link"
            setAttribute link "href"  "#"
            setTextContent link name

            item <- createElement "li"
            setAttribute item "class" "nav-item"

            replaceChild item link

            return (item, link)

    (formTab, formLink) <- toTab "Form"
    (codeTab, codeLink) <- toTab "Code"
    (typeTab, typeLink) <- toTab "Type"

    let tabs = [ formTab, codeTab, typeTab ]
    let links = [ formLink, codeLink, typeLink ]

    tabsList <- createElement "ul"
    setAttribute tabsList "class" "nav nav-tabs"
    setAttribute tabsList "class" "nav nav-tabs"

    replaceChildren tabsList (Array.fromList tabs)

    pane <- createElement "div"

    success <- createElement "div"

    let successChildren = if showTabs then [ tabsList, pane ] else [ pane ]

    replaceChildren success (Array.fromList successChildren)

    codemirrorBuffer <- getElementById "codemirror-buffer"

    let createCodemirrorOutput = do
            textarea <- createElement "textarea"

            replaceChild codemirrorBuffer textarea

            codeMirror <- setupCodemirrorOutput textarea

            replaceChildren codemirrorBuffer (Array.fromList [])

            return (codeMirror, getWrapperElement codeMirror)

    htmlWrapper <- createElement "div"
    setAttribute htmlWrapper "class" "form"

    (codeOutput, codeWrapper) <- createCodemirrorOutput
    (typeOutput, typeWrapper) <- createCodemirrorOutput

    let registerTabCallback selectedTab selectedLink action = do
            callback <- Callback.asyncCallback do
                let deselect link = removeClass link "active"

                traverse_ deselect links

                addClass selectedLink "active"

                action

            addEventListener selectedTab "click" callback

    registerTabCallback formTab formLink do
        replaceChild pane htmlWrapper

    registerTabCallback codeTab codeLink do
        replaceChild pane codeWrapper

        refresh codeOutput

    registerTabCallback typeTab typeLink do
        replaceChild pane typeWrapper

        refresh typeOutput

    addClass formLink "active"

    replaceChild pane htmlWrapper

    spinner <- do
        spinner <- createElement "div"
        setAttribute spinner "class"    "spinner-border text-primary"
        setAttribute spinner "role"     "status"
        setAttribute spinner "overflow" "hidden"

        return spinner

    error <- createElement "pre"

    let setBusy = do
            replaceChild output spinner

    let setError text = do
            setTextContent error text

            replaceChild output error

    let setSuccess type_ value render = do
            setValue codeOutput (valueToText value)
            setValue typeOutput (typeToText type_)

            refreshInput <- render htmlWrapper

            replaceChild output success

            refresh codeOutput
            refresh typeOutput

            return refreshInput

    return (setBusy, setSuccess, setError)

main :: IO ()
main = do
    inputArea     <- getElementById "input"
    startTutorial <- getElementById "start-tutorial"

    codeInput  <- setupCodemirrorInput inputArea

    focus codeInput

    counter <- IORef.newIORef 0

    params <- getSearchParams

    hasTutorial <- hasParam params "tutorial"

    tutorialRef <- IORef.newIORef hasTutorial

    keyToMethods <- HTTP.getMethods

    output <- getElementById "output"

    (setBusy, setSuccess, setError) <- createForm True output

    let interpret () = do
            text <- getValue codeInput

            if text == ""
                then deleteParam params "expression"
                else setParam params "expression" (URI.Encode.encodeText text)

            tutorial <- IORef.readIORef tutorialRef

            if tutorial == False
                then deleteParam params "tutorial"
                else setParam params "tutorial" "true"

            saveSearchParams params

            if  | Text.null text -> do
                    Monad.unless tutorial do
                        setDisplay startTutorial "inline-block"

                    replaceChildren output (Array.fromList [])

                | otherwise -> do
                    setDisplay startTutorial "none"

                    setBusy

                    let input_ = Code "(input)" text

                    result <- Exception.try do
                        expression <- Import.resolve input_

                        (inferred, elaboratedExpression) <- Infer.typeWith input_ [] expression

                        value <- Normalize.evaluate keyToMethods [] elaboratedExpression

                        refreshInput <- setSuccess inferred value \htmlWrapper -> do
                            renderValue keyToMethods counter htmlWrapper inferred value

                        refreshInput

                    case result of
                        Left exception -> do
                            setError (Text.pack (displayException (exception :: SomeException)))
                        Right () -> do
                            return ()

    debouncedInterpret <- debounce interpret

    inputCallback <- Callback.asyncCallback (debouncedInterpret ())

    onChange codeInput inputCallback

    let enableTutorial = do
            stopTutorial <- createElement "button"

            setAttribute stopTutorial "type"  "button"
            setAttribute stopTutorial "class" "btn btn-primary"
            setAttribute stopTutorial "id"    "stop-tutorial"

            setTextContent stopTutorial "Exit the tutorial"

            let createExample active name file = do
                    text <- liftIO (DataFile.readDataFile ("examples" </> "tutorial" </> file))
                    let code = Text.strip text

                    n <- State.get

                    State.put (n + 1)

                    let id = "example-" <> Text.pack (show n)

                    a <- createElement "a"

                    setAttribute a "id"           id
                    setAttribute a "aria-current" "page"
                    setAttribute a "href"         "#"
                    setAttribute a "onclick"      "return false;"

                    setAttribute a "class"
                        (if active then "nav-link example-tab active" else "example-tab nav-link")

                    setTextContent a name

                    li <- createElement "li"

                    setAttribute li "class" "nav-item"

                    replaceChild li a

                    callback <- (liftIO . Callback.asyncCallback) do
                        setValue codeInput code

                        elements <- getElementsByClassName "example-tab"

                        Monad.forM_ elements \element -> do
                            removeClass element "active"

                        element <- getElementById id

                        addClass element "active"

                    Monad.when active (setValue codeInput code)

                    addEventListener a "click" callback

                    return li

            ul <- createElement "ul"

            flip State.evalStateT (0 :: Int) do
                helloWorld <- createExample True "Hello, world!" "hello.ffg"

                checkboxes <- createExample False "HTML" "checkboxes.ffg"

                data_ <- createExample False "Data" "data.ffg"

                prompting <- createExample False "Prompting" "prompting.ffg"

                variables <- createExample False "Variables" "variables.ffg"

                functions <- createExample False "Functions" "functions.ffg"

                imports <- createExample False "Imports" "imports.ffg"


                lists <- createExample False "Lists" "lists.ffg"

                coding <- createExample False "Coding" "coding.ffg"

                prelude <- createExample False "Prelude" "prelude.ffg"

                setAttribute ul "class" "nav nav-tabs"

                replaceChildren ul
                    (Array.fromList
                        [ helloWorld
                        , checkboxes
                        , data_
                        , prompting
                        , variables
                        , functions
                        , imports
                        , lists
                        , coding
                        , prelude
                        ]
                    )

            before inputArea ul

            stopTutorialCallback <- Callback.asyncCallback do
                setDisplay stopTutorial  "none"

                IORef.writeIORef tutorialRef False

                debouncedInterpret ()

                remove stopTutorial

                remove ul

                focus codeInput

            addEventListener stopTutorial "click" stopTutorialCallback

            after startTutorial stopTutorial

            IORef.writeIORef tutorialRef True

            setDisplay startTutorial "none"

            focus codeInput

    startTutorialCallback <- Callback.asyncCallback enableTutorial

    addEventListener startTutorial "click" startTutorialCallback

    Monad.when hasTutorial enableTutorial

    hasExpression <- hasParam params "expression"

    Monad.when hasExpression do
        expression <- getParam params "expression"

        setValue codeInput (URI.Encode.decodeText expression)

    debouncedInterpret ()
