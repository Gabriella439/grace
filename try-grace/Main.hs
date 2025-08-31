{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Control.Applicative (empty)
import Control.Concurrent.Async (Async)
import Control.Exception.Safe (Exception(..), SomeException)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Foldable (toList, traverse_)
import Data.IORef (IORef)
import Data.JSString (JSString)
import Data.These (These(..))
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

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Exception.Safe as Exception
import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Trans.Reader as Reader
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

foreign import javascript unsafe "converter.makeHtml($1)"
    markdownToHTML_ :: JSString -> JSString

markdownToHTML :: Text -> Text
markdownToHTML a = toText (markdownToHTML_ (fromText a))

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
    setInnerHTML parent (markdownToHTML text)

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

    results <- forM values \value -> do
        li <- createElement "li"

        refreshOutput <- renderValue keyToMethods ref li inner value

        return (li, refreshOutput)

    let (lis, refreshOutputs) = unzip (toList results)

    ul <- createElement "ul"

    replaceChildren ul (Array.fromList lis)

    replaceChild parent ul

    return (sequence_ refreshOutputs)

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

            refreshOutput <- renderValue keyToMethods ref dd type_ value

            dl <- createElement "dl"

            setAttribute dl "class" "row"

            replaceChildren dl (Array.fromList [ dt, dd ])

            return (dl, refreshOutput)

    result <- HashMap.traverseWithKey process keyValues

    let (dls, refreshOutputs) = unzip (HashMap.elems result)

    replaceChildren parent (Array.fromList dls)

    return (sequence_ refreshOutputs)

renderValue keyToMethods ref parent outer (Application (Value.Builtin Syntax.Some) value) = do
    renderValue keyToMethods ref parent outer value

renderValue keyToMethods ref parent outer (Value.Alternative alternative value) = do
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

renderValue keyToMethods counter parent Type.Function{ input, output } function = do
    outputVal <- createElement "div"

    (setBusy, setSuccess, setError) <- createForm False outputVal

    let render value = do
            setBusy

            eitherResult <- (liftIO . Exception.try) do
                newValue <- Normalize.apply keyToMethods function value

                refreshOutput <- setSuccess output newValue \htmlWrapper -> do
                    renderValue keyToMethods counter htmlWrapper output newValue

                refreshOutput

            case eitherResult of
                Left exception -> do
                    setError (Text.pack (displayException (exception :: SomeException)))

                Right r -> do
                    return r

    renderOutput <- debounce render

    if Lens.has Value.effects function
        then do
            let (_, reader) = renderInput input

            result <- Maybe.runMaybeT (Reader.runReaderT reader RenderInput
                { keyToMethods
                , renderOutput
                , live = False
                , counter
                })

            case result of
                Nothing -> do
                    replaceChildren parent (Array.fromList [ ])

                    mempty

                Just (inputVal, invoke, refreshOutput) -> do
                    button <- createElement "button"

                    setAttribute button "type"  "button"
                    setAttribute button "class" "btn btn-primary"

                    setTextContent button "Submit"

                    hr <- createElement "hr"

                    callback <- Callback.asyncCallback invoke

                    addEventListener button "click" callback

                    replaceChildren parent (Array.fromList [ inputVal, button, hr, outputVal ])

                    return refreshOutput

        else do
            let (_, reader) = renderInput input

            result <- Maybe.runMaybeT (Reader.runReaderT reader RenderInput
                { keyToMethods
                , renderOutput
                , live = True
                , counter
                })

            case result of
                Nothing -> do
                    replaceChildren parent (Array.fromList [ ])

                    mempty

                Just (inputVal, invoke, refreshOutput) -> do
                    invoke

                    hr <- createElement "hr"

                    replaceChildren parent (Array.fromList [ inputVal, hr, outputVal ])
                    return refreshOutput

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

data RenderInput = RenderInput
    { keyToMethods :: Text -> Methods
    , renderOutput :: Value -> IO ()
    , live :: Bool
    , counter :: IORef Natural
    }

register
    :: MonadIO m => JSVal -> MaybeT IO Value -> ReaderT RenderInput m (IO ())
register input get = do
    RenderInput{ live, renderOutput } <- Reader.ask

    liftIO do
        let invoke = do
                maybeValue <- Maybe.runMaybeT get

                traverse_ renderOutput maybeValue

        Monad.when live do
            callback <- Callback.asyncCallback invoke

            addEventListener input "input" callback

        return invoke

renderInput
    :: Type Location
    -> (Value, ReaderT RenderInput (MaybeT IO) (JSVal, IO (), IO ()))
renderInput Type.Scalar{ scalar = Monotype.Bool } = do
    (,) (Value.Scalar (Bool False)) do
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

        invoke <- register input get

        return (span, invoke, mempty)

renderInput Type.Scalar{ scalar = Monotype.Real } = do
    (,) (Value.Scalar (Real 0)) do
        input <- createElement "input"

        setAttribute input "class" "form-control"
        setAttribute input "type"  "number"
        setAttribute input "step"  "any"
        setAttribute input "value" "0"

        let get = do
                double <- toDoubleValue input

                return (Value.Scalar (Real (Scientific.fromFloatDigits double)))

        invoke <- register input get

        return (input, invoke, mempty)

renderInput Type.Scalar{ scalar = Monotype.Integer } = do
    (,) (Value.Scalar (Integer 0)) do
        input <- createElement "input"

        setAttribute input "class" "form-control"
        setAttribute input "type"  "number"
        setAttribute input "value" "0"

        let get = do
                integer <- toIntegerValue input

                return (Value.Scalar (Integer integer))

        invoke <- register input get

        return (input, invoke, mempty)

renderInput Type.Scalar{ scalar = Monotype.Natural } = do
    (,) (Value.Scalar (Natural 0)) do
        input <- createElement "input"

        setAttribute input "class" "form-control"
        setAttribute input "type"  "number"
        setAttribute input "value" "0"
        setAttribute input "min"   "0"

        let get = do
                integer <- toIntegerValue input

                return (Value.Scalar (Natural (fromInteger integer)))

        invoke <- register input get

        return (input, invoke, mempty)

renderInput Type.Scalar{ scalar = Monotype.JSON } = do
    (,) (Value.Scalar Null) do
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

        invoke <- register input get

        return (input, invoke, mempty)

renderInput Type.Scalar{ scalar = Monotype.Text } = do
    (,) (Value.Text "") do
        input <- createElement "textarea"

        setAttribute input "class" "form-control"
        setAttribute input "rows" "1"

        autoResize input

        let get = do
                text <- toValue input

                return (Value.Text text)

        invoke <- register input get

        return (input, invoke, mempty)

renderInput Type.Scalar{ scalar = Monotype.Key } = do
    (,) (Value.Scalar (Key "")) do
        input <- createElement "input"

        setAttribute input "type" "password"
        setAttribute input "class" "form-control"
        setAttribute input "rows" "1"

        let get = do
                key <- toValue input

                return (Value.Scalar (Key key))

        invoke <- register input get

        return (input, invoke, mempty)

renderInput Type.Record{ fields = Type.Fields keyTypes _ } = do
    let triples = do
            (key, type_) <- keyTypes

            let (start, reader) = renderInput type_

            return (key, start, reader)

    let hashMap = HashMap.fromList do
            (key, start, _) <- triples

            return (key, start)

    let keyReaders = do
            (key, _, reader) <- triples

            return (key, reader)

    (,) (Value.Record hashMap) do
        ref <- liftIO (IORef.newIORef hashMap)

        let process (key, reader) = do
                let nest x = x{ renderOutput = newRenderOutput }
                      where
                        newRenderOutput value = do
                            let update m = (m', m')
                                  where
                                    m' = HashMap.insert key value m

                            m <- liftIO (IORef.atomicModifyIORef' ref update)

                            renderOutput x (Value.Record m)

                (inputField, invokeField, refreshField) <- Reader.withReaderT nest reader

                dt <- createElement "dt"

                setAttribute dt "class" "col-auto"

                setTextContent dt (key <> ":")

                dd <- createElement "dd"

                setAttribute dd "class" "col"

                replaceChild dd inputField

                dl <- createElement "dl"

                setAttribute dl "class" "row"

                replaceChildren dl (Array.fromList [ dt, dd ])

                return (dl, invokeField, refreshField)

        results <- traverse process keyReaders

        let (children, invokes, refreshOutputs) = unzip3 results

        div <- createElement "div"

        replaceChildren div (Array.fromList children)

        let invoke = sequence_ invokes

        let refreshOutput = sequence_ refreshOutputs

        return (div, invoke, refreshOutput)

renderInput Type.Union{ alternatives = Type.Alternatives keyTypes _ }
    | (key₀, type_₀) : _ <- keyTypes = do
        let (start, _) = renderInput type_₀

        (,) (Value.Alternative key₀ start) do
            RenderInput{ live, counter } <- Reader.ask

            n <- liftIO (IORef.atomicModifyIORef' counter (\a -> (a + 1, a)))

            checkedValRef <- liftIO (IORef.newIORef Nothing)

            let process (checked, (key, type_)) = do
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

                    liftIO (Monad.when checked (IORef.writeIORef checkedValRef (Just fieldset)))

                    let nest x = x{ renderOutput = newRenderOutput }
                          where
                            newRenderOutput value = do
                                enabled <- getChecked input

                                Monad.when enabled (renderOutput x (Alternative key value))

                    let (_, reader) = renderInput type_

                    (nestedInput, nestedInvoke, nestedRefresh) <- Reader.withReaderT nest reader

                    replaceChild fieldset nestedInput

                    div <- createElement "div"

                    setAttribute div "class" "form-check"

                    replaceChildren div (Array.fromList [ input, label, fieldset ])

                    liftIO do
                        let update = do
                                let adapt m = (Just fieldset, m)

                                oldFieldset <- IORef.atomicModifyIORef' checkedValRef adapt

                                traverse_ (\x -> setDisabled x True) oldFieldset

                                setDisabled fieldset False

                                Monad.when live nestedInvoke

                        callback <- Callback.asyncCallback update

                        addEventListener input "input" callback

                    let invoke = do
                            enabled <- getChecked input

                            Monad.when enabled nestedInvoke

                    return (div, invoke, nestedRefresh)

            results <- traverse process (zip (True : repeat False) keyTypes)

            let (children, invokes, refreshOutputs) = unzip3 results

            div <- createElement "div"

            replaceChildren div (Array.fromList children)

            let invoke = sequence_ invokes

            let refreshOutput = sequence_ refreshOutputs

            liftIO (Monad.when live invoke)

            return (div, invoke, refreshOutput)

renderInput Type.Optional{ type_ } = do
    let (_, reader) = renderInput type_

    (,) (Value.Scalar Null) do
        RenderInput{ live } <- Reader.ask

        input <- createElement "input"

        setAttribute input "type"  "checkbox"
        setAttribute input "class" "form-check-input"

        let nest x = x{ renderOutput = newRenderOutput }
              where
                newRenderOutput value = do
                    checked <- getChecked input
                    if checked
                        then do
                            renderOutput x (Application (Value.Builtin Syntax.Some) value)
                        else do
                            renderOutput x (Value.Scalar Null)

        (nestedInput, nestedInvoke, nestedRefresh) <- Reader.withReaderT nest reader

        div <- createElement "div"

        fieldset <- createElement "fieldset"

        replaceChild fieldset nestedInput

        replaceChildren div (Array.fromList [input, fieldset])

        liftIO do
            let update = do
                    checked <- getChecked input

                    setDisabled fieldset (not checked)

                    Monad.when live nestedInvoke

            callback <- Callback.asyncCallback update

            addEventListener input "input" callback

            update

        return (div, nestedInvoke, nestedRefresh)

renderInput Type.List{ type_ } = do
    let (_, reader) = renderInput type_

    (,) (Value.List mempty) do
        RenderInput{ live } <- Reader.ask

        plus <- createElement "button"

        setAttribute plus "type"  "button"
        setAttribute plus "class" "btn btn-primary"

        setTextContent plus "+"

        add <- createElement "li"

        replaceChild add plus

        childrenRef <- liftIO (IORef.newIORef IntMap.empty)

        input <- Reader.ask

        insert <- (liftIO . Callback.asyncCallback) do
            minus <- createElement "button"

            setAttribute minus "type"    "button"
            setAttribute minus "class"   "btn btn-danger"
            setAttribute minus "display" "inline"

            setTextContent minus "-"

            span <- createElement "span"

            setTextContent span " "

            li <- createElement "li"

            i <- do
                children <- IORef.readIORef childrenRef

                return case IntMap.lookupMax children of
                    Nothing -> 0
                    Just (n, _) -> n + 1

            delete <- Callback.asyncCallback do
                IORef.atomicModifyIORef' childrenRef (\m -> (IntMap.delete i m, ()))

                remove li

            addEventListener minus "click" delete

            let nest x = x{ renderOutput = newRenderOutput }
                  where
                    newRenderOutput value = do
                        let update m = case IntMap.lookup i m of
                                Nothing ->
                                    IntMap.insert i (This value) m

                                Just (This _) ->
                                    IntMap.insert i (This value) m
                                Just (That (invoke, refreshOutput)) ->
                                    IntMap.insert i (These value (invoke, refreshOutput)) m
                                Just (These _ (invoke, refreshOutput)) ->
                                    IntMap.insert i (These value (invoke, refreshOutput)) m

                        let adapt m = let m' = update m in (m', m')

                        children <- IORef.atomicModifyIORef' childrenRef adapt

                        let values = do
                                these <- IntMap.elems children

                                case these of
                                    This v -> return v
                                    That _ -> []
                                    These v _ -> return v

                        renderOutput x (Value.List (Seq.fromList values))

            result <- Maybe.runMaybeT (Reader.runReaderT reader (nest input))

            case result of
                Nothing -> do
                    replaceChildren li (Array.fromList [ minus ])

                Just (nestedInput, nestedInvoke, nestedRefresh) -> do
                    let update m = case IntMap.lookup i m of
                            Nothing ->
                                IntMap.insert i (That (nestedInvoke, nestedRefresh)) m

                            Just (This value) ->
                                IntMap.insert i (These value (nestedInvoke, nestedRefresh)) m
                            Just (That _)  ->
                                IntMap.insert i (That (nestedInvoke, nestedRefresh)) m
                            Just (These value _) ->
                                IntMap.insert i (These value (nestedInvoke, nestedRefresh)) m

                    IORef.atomicModifyIORef' childrenRef (\m -> (update m, ()))

                    replaceChildren li (Array.fromList [ minus, span, nestedInput ])

                    Monad.when live nestedInvoke

            before add li

        addEventListener plus "click" insert

        ul <- createElement "ul"

        setAttribute ul "class" "list-unstyled"

        replaceChild ul add

        let invoke = do
                children <- IORef.readIORef childrenRef

                sequence_ do
                    these <- IntMap.elems children

                    return case these of
                        This _ -> mempty
                        That (nestedInvoke, _) -> nestedInvoke
                        These _ (nestedInvoke, _) -> nestedInvoke

        let refreshOutput = do
                children <- IORef.readIORef childrenRef

                sequence_ do
                    these <- IntMap.elems children

                    return case these of
                        This _ -> mempty
                        That (_, nestedRefresh) -> nestedRefresh
                        These _ (_, nestedRefresh) -> nestedRefresh

        return (ul, invoke, refreshOutput)

renderInput type_ = do
    (,) (Value.Scalar Null) do
        RenderInput{ keyToMethods, live, renderOutput } <- Reader.ask

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

        liftIO do
            let invoke = do
                    maybeValue <- Maybe.runMaybeT get

                    traverse_ renderOutput maybeValue

            Monad.when live do
                callback <- Callback.asyncCallback invoke

                onChange codeInput callback

            return (div, invoke, refresh codeInput)

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

                        refreshOutput <- setSuccess inferred value \htmlWrapper -> do
                            renderValue keyToMethods counter htmlWrapper inferred value

                        refreshOutput


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
