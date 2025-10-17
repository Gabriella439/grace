{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative (empty, liftA2)
import Control.Concurrent.Async (Async)
import Control.Exception.Safe (catch, Exception(..), SomeException)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Foldable (toList, traverse_)
import Data.IORef (IORef)
import Data.JSString (JSString)
import Data.Sequence (ViewR(..), (|>))
import Data.Text (Text)
import Data.Traversable (forM)
import Data.Void (Void)
import Grace.Infer (Status(..))
import Grace.Type (Type(..))
import GHCJS.Foreign.Callback (Callback)
import GHCJS.Types (JSVal)
import Grace.Decode (FromGrace)
import Grace.Encode (ToGrace(..))
import Grace.HTTP (Methods)
import Grace.Input (Input(..))
import Grace.Location (Location)
import Grace.Monotype (RemainingFields(..))
import Grace.Syntax (FieldName(..), NameBinding(..), Scalar(..), Syntax)
import Grace.Value (Value(..))
import JavaScript.Array (JSArray)
import Numeric.Natural (Natural)
import Prelude hiding (div, error, id, length, span, subtract)
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
import qualified Data.IORef as IORef
import qualified Data.JSString as JSString
import qualified Data.JSString.Text as JSString.Text
import qualified Data.List as List
import qualified Data.Scientific as Scientific
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Encoding
import qualified GHCJS.Foreign.Callback as Callback
import qualified GHCJS.Types
import qualified Grace.Context as Context
import qualified Grace.DataFile as DataFile
import qualified Grace.Decode as Decode
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

instance (Applicative m, Semigroup a) => Semigroup (ReaderT r m a) where
    (<>) = liftA2 (<>)

instance (Applicative m, Monoid a) => Monoid (ReaderT r m a) where
    mempty = pure mempty

foreign import javascript unsafe "document.getElementById($1)"
    getElementById_ :: JSString -> IO JSVal

getElementById :: MonadIO io => Text -> io JSVal
getElementById a = liftIO (getElementById_ (fromText a))

foreign import javascript unsafe "$1.value"
    toValue_ :: JSVal -> IO JSString

toValue :: MonadIO io => JSVal -> io Text
toValue a = liftIO (fmap toText (toValue_ a))

foreign import javascript unsafe "$1.value = $2"
    setValue_ :: JSVal -> JSString -> IO ()

setValue :: MonadIO io => JSVal -> Text -> io ()
setValue a b = liftIO (setValue_ a (fromText b))

toIntegerValue :: MonadIO io => JSVal -> io Integer
toIntegerValue a = liftIO (fmap (read . JSString.unpack) (toValue_ a))

setIntegerValue :: MonadIO io => JSVal -> Integer -> io ()
setIntegerValue a b = liftIO (setValue_ a (JSString.pack (show b)))

toNaturalValue :: MonadIO io => JSVal -> io Natural
toNaturalValue a = liftIO (fmap (read . JSString.unpack) (toValue_ a))

setNaturalValue :: MonadIO io => JSVal -> Natural -> io ()
setNaturalValue a b = liftIO (setValue_ a (JSString.pack (show b)))

foreign import javascript unsafe "$1.value"
    toDoubleValue_ :: JSVal -> IO Double

toDoubleValue :: MonadIO io => JSVal -> io Double
toDoubleValue a = liftIO (toDoubleValue_ a)

foreign import javascript unsafe "$1.value = $2"
    setDoubleValue_ :: JSVal -> Double -> IO ()

setDoubleValue :: MonadIO io => JSVal -> Double -> io ()
setDoubleValue a b = liftIO (setDoubleValue_ a b)

foreign import javascript unsafe "$1.checked"
    getChecked_ :: JSVal -> IO Bool

getChecked :: MonadIO io => JSVal -> io Bool
getChecked a = liftIO (getChecked_ a)

foreign import javascript unsafe "$1.checked = $2"
    setChecked_ :: JSVal -> Bool -> IO ()

setChecked :: MonadIO io => JSVal -> Bool -> io ()
setChecked a b = liftIO (setChecked_ a b)

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
    setCodeValue_ :: JSVal -> JSString -> IO ()

setCodeValue :: MonadIO io => JSVal -> Text -> io ()
setCodeValue a b = liftIO (setCodeValue_ a (fromText b))

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

foreign import javascript unsafe "sessionStorage.setItem($1, $2)"
    setSessionStorage_ :: JSString -> JSString -> IO ()

setSessionStorage :: MonadIO io => Text -> Text -> io ()
setSessionStorage a b = liftIO (setSessionStorage_ (fromText a) (fromText b))

foreign import javascript unsafe "sessionStorage.getItem($1)"
    getSessionStorage_ :: JSString -> IO JSVal

getSessionStorage :: MonadIO io => Text -> io (Maybe Text)
getSessionStorage a = liftIO do
    jsVal <- getSessionStorage_ (fromText a)

    if GHCJS.Types.isNull jsVal
        then return Nothing
        else return (Just (JSString.Text.textFromJSVal jsVal))

foreign import javascript unsafe "localStorage.setItem($1, $2)"
    setLocalStorage_ :: JSString -> JSString -> IO ()

setLocalStorage :: MonadIO io => Text -> Text -> io ()
setLocalStorage a b = liftIO (setLocalStorage_ (fromText a) (fromText b))

foreign import javascript unsafe "localStorage.getItem($1)"
    getLocalStorage_ :: JSString -> IO JSVal

getLocalStorage :: MonadIO io => Text -> io (Maybe Text)
getLocalStorage a = liftIO do
    jsVal <- getLocalStorage_ (fromText a)

    if GHCJS.Types.isNull jsVal
        then return Nothing
        else return (Just (JSString.Text.textFromJSVal jsVal))

foreign import javascript unsafe "print($1)"
    printElement_ :: JSVal -> IO ()

printElement :: MonadIO io => JSVal -> io ()
printElement a = liftIO (printElement_ a)

foreign import javascript unsafe "navigator.clipboard.writeText($1)"
    writeClipboard_ :: JSString -> IO ()

writeClipboard :: MonadIO io => Text -> io ()
writeClipboard a = liftIO (writeClipboard_ (fromText a))

toText :: JSString -> Text
toText = Text.pack . JSString.unpack

fromText :: Text -> JSString
fromText = JSString.pack . Text.unpack

typeToText :: Type s -> Text
typeToText = Pretty.renderStrict False 80

valueToText :: Value -> Text
valueToText = Pretty.renderStrict False 80 . Normalize.strip . Normalize.quote

data RenderValue = RenderValue
    { keyToMethods :: Text -> Methods
    , counter :: IORef Natural
    , status :: Status
    , edit :: Bool
    }

renderValue
    :: JSVal
    -> Type Location
    -> Value
    -> ReaderT RenderValue IO (IO ())
renderValue parent Type.Optional{ type_ } value =
    renderValue parent type_ value

renderValue parent _ (Value.Text text) = do
    setAttribute parent "style" "position: relative;"

    markdown <- createElement "div"
    setAttribute markdown "class" "printable"
    setInnerHTML markdown (markdownToHTML text)

    printButton <- createElement "button"
    setAttribute printButton "class" "print-button btn btn-outline-light"
    setAttribute printButton "type" "button"
    setInnerText printButton "Print"

    printCallback <- liftIO (Callback.asyncCallback (printElement markdown))
    addEventListener printButton "click" printCallback

    copyButton <- createElement "button"
    setAttribute copyButton "class" "copy-button btn btn-outline-light"
    setAttribute copyButton "type" "button"
    setInnerText copyButton "Copy"

    copyCallback <- liftIO (Callback.asyncCallback (writeClipboard text))
    addEventListener copyButton "click" copyCallback

    replaceChildren parent (Array.fromList [ printButton, copyButton, markdown ])

    mempty

renderValue parent _ (Value.Scalar (Bool bool)) = do
    input <- createElement "input"

    setAttribute input "type"     "checkbox"
    setAttribute input "class"    "form-check-input"
    setDisabled input True

    Monad.when bool (setAttribute input "checked" "")

    replaceChild parent input

    mempty

renderValue parent _ (Value.Scalar Null) = do
    span <- createElement "span"

    setAttribute span "class" "fira"

    setTextContent span "∅"

    replaceChild parent span

    mempty

renderValue parent _ value@Value.Scalar{} = do
    span <- createElement "span"

    setTextContent span (valueToText value)

    setAttribute span "class" "fira"
    setAttribute span "style" "whitespace: pre"

    replaceChild parent span

    mempty

renderValue parent outer (Value.List values) = do
    inner <- case outer of
            Type.List{ type_ } -> do
                return type_

            Type.Scalar{ scalar = Monotype.JSON } -> do
                return outer

            _ -> do
                fail "renderValue: Missing element type"

    results <- forM values \value -> do
        li <- createElement "li"

        refreshOutput <- renderValue li inner value

        return (li, refreshOutput)

    let (lis, refreshOutputs) = unzip (toList results)

    ul <- createElement "ul"

    replaceChildren ul (Array.fromList lis)

    replaceChild parent ul

    return (sequence_ refreshOutputs)

renderValue parent outer (Value.Record keyValues) = do
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

            refreshOutput <- renderValue dd type_ value

            dl <- createElement "dl"

            setAttribute dl "class" "row"

            replaceChildren dl (Array.fromList [ dt, dd ])

            return (dl, refreshOutput)

    result <- HashMap.traverseWithKey process keyValues

    let (dls, refreshOutputs) = unzip (HashMap.elems result)

    replaceChildren parent (Array.fromList dls)

    return (sequence_ refreshOutputs)

renderValue parent outer (Application (Value.Builtin Syntax.Some) value) = do
    renderValue parent outer value

renderValue parent outer (Value.Alternative alternative value) = do
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
    r@RenderValue{ counter, keyToMethods, status, edit } <- Reader.ask

    outputVal <- createElement "div"

    let hasEffects = Lens.has Value.effects function

    (setBusy, setSuccess, setError) <- createForm (edit && hasEffects) outputVal

    let render value = do
            setBusy

            let interpretOutput = do
                    newValue <- Normalize.apply keyToMethods function value

                    status_@Status{ context } <- State.get

                    let solvedType = Context.solveType context output

                    refreshOutput <- liftIO $ setSuccess solvedType newValue \htmlWrapper -> do
                        Reader.runReaderT (renderValue htmlWrapper solvedType newValue) (r :: RenderValue){ status = status_ }

                    liftIO refreshOutput

            eitherResult <- liftIO (Exception.try (State.evalStateT interpretOutput status))

            case eitherResult of
                Left exception -> do
                    setError (Text.pack (displayException (exception :: SomeException)))

                Right x -> do
                    return x

    debouncedRender <- debounce render

    (_, reader) <- liftIO (renderInput [] input)

    let renderOutput Change | hasEffects = mempty
        renderOutput _                   = debouncedRender

    result <- liftIO $ Maybe.runMaybeT (Reader.runReaderT reader RenderInput
        { keyToMethods
        , renderOutput
        , counter
        , status
        , edit
        })

    case result of
        Nothing -> do
            replaceChildren parent (Array.fromList [ ])

            mempty

        Just (inputVal, invoke, refreshOutput) -> do
            if hasEffects
                then do
                    button <- createElement "button"

                    setAttribute button "type"  "button"
                    setAttribute button "class" "btn btn-primary"

                    setTextContent button "Submit"

                    hr <- createElement "hr"

                    callback <- liftIO (Callback.asyncCallback (invoke Submit))

                    addEventListener button "click" callback

                    replaceChildren parent (Array.fromList [ inputVal, button, hr, outputVal ])

                else do
                    liftIO (invoke Submit)

                    hr <- createElement "hr"

                    replaceChildren parent (Array.fromList [ inputVal, hr, outputVal ])
            return refreshOutput

-- At the time of this writing this case should (in theory) never be hit,
-- because all of the `Value` constructors are either explicitly handled (e.g.
-- `Text` / `Scalar`) or handled by the case for `Type.Function` (e.g. `Builtin`
-- / `Alternative`)
renderValue parent _ value = do
    renderDefault parent value

renderDefault :: MonadIO io => JSVal -> Value -> io (IO ())
renderDefault parent value = liftIO do
    code <- createElement "code"

    setTextContent code (valueToText value)

    replaceChild parent code

    mempty

data Mode
    = Change
    -- ^ The function is being run in response to a form input changing
    | Submit
    -- ^ The function is being run in response to form submission

data RenderInput = RenderInput
    { keyToMethods :: Text -> Methods
    , renderOutput :: Mode -> Value -> IO ()
    , counter :: IORef Natural
    , status :: Status
    , edit :: Bool
    }

register
    :: MonadIO m
    => JSVal -> MaybeT IO Value -> ReaderT RenderInput m (Mode -> IO ())
register input get = do
    RenderInput{ renderOutput } <- Reader.ask

    liftIO do
        let invoke mode = do
                maybeValue <- Maybe.runMaybeT get

                traverse_ (renderOutput mode) maybeValue

        callback <- Callback.asyncCallback (invoke Change)

        addEventListener input "input" callback

        return invoke

renderPath :: [Text] -> Type Location -> Text
renderPath path type_ = (prefix <> " : " <> suffix)
  where
    prefix =
        Text.intercalate "."
            (fmap (Pretty.toText . Type.prettyRecordLabel False) path)

    suffix = Pretty.toText type_

fromStorage :: FromGrace a => Maybe Text -> IO (Maybe a)
fromStorage Nothing = do
    return Nothing
fromStorage (Just text) = do
    load `catch` \(_ :: SomeException) -> return Nothing
  where
    load = do
        a <- Interpret.load (Code "(storage)" text)
        return (Just a)

toStorage :: ToGrace a => a -> Text
toStorage a = Pretty.toText (Normalize.quote (encode a))

renderInput
    :: [Text]
    -> Type Location
    -> IO (Value, ReaderT RenderInput (MaybeT IO) (JSVal, Mode -> IO (), IO ()))
renderInput path type_@Type.Scalar{ scalar = Monotype.Bool } = do
    maybeText <- getSessionStorage (renderPath path type_)

    maybeBool <- fromStorage maybeText

    let bool₀ = case maybeBool of
            Just b -> b
            Nothing -> False

    return $ (,) (Value.Scalar (Bool bool₀)) do
        input <- createElement "input"

        setAttribute input "type"  "checkbox"
        setAttribute input "class" "form-check-input"

        setChecked input bool₀

        span <- createElement "span"

        setAttribute span "class" "form-check"
        setAttribute span "style" "display: inline-block !important;"

        replaceChild span input

        let get = do
                bool <- getChecked input

                setSessionStorage (renderPath path type_) (toStorage bool)

                return (Value.Scalar (Bool bool))

        invoke <- register input get

        return (span, invoke, mempty)

renderInput path type_@Type.Scalar{ scalar = Monotype.Real } = do
    maybeText <- getSessionStorage (renderPath path type_)

    maybeScientific <- fromStorage maybeText

    let scientific₀ = case maybeScientific of
            Just s -> s
            Nothing -> 0

    return $ (,) (Value.Scalar (Real scientific₀)) do
        input <- createElement "input"

        setAttribute input "class" "form-control"
        setAttribute input "type"  "number"
        setAttribute input "step"  "any"
        setAttribute input "value" "0"

        setDoubleValue input (Scientific.toRealFloat scientific₀)

        let get = do
                double <- toDoubleValue input

                setSessionStorage (renderPath path type_) (toStorage double)

                return (Value.Scalar (Real (Scientific.fromFloatDigits double)))

        invoke <- register input get

        return (input, invoke, mempty)

renderInput path type_@Type.Scalar{ scalar = Monotype.Integer } = do
    maybeText <- getSessionStorage (renderPath path type_)

    maybeInteger <- fromStorage maybeText

    let integer₀ = case maybeInteger of
            Just i -> i
            Nothing -> 0

    return $ (,) (Value.Scalar (Integer integer₀)) do
        input <- createElement "input"

        setAttribute input "class" "form-control"
        setAttribute input "type"  "number"
        setAttribute input "value" "0"

        setIntegerValue input integer₀

        let get = do
                integer <- toIntegerValue input

                setSessionStorage (renderPath path type_) (toStorage integer)

                return (Value.Scalar (Integer integer))

        invoke <- register input get

        return (input, invoke, mempty)

renderInput path type_@Type.Scalar{ scalar = Monotype.Natural } = do
    maybeText <- getSessionStorage (renderPath path type_)

    maybeNatural <- fromStorage maybeText

    let natural₀ = case maybeNatural of
            Just n -> n
            Nothing -> 0

    return $ (,) (Value.Scalar (Natural natural₀)) do
        input <- createElement "input"

        setAttribute input "class" "form-control"
        setAttribute input "type"  "number"
        setAttribute input "value" "0"
        setAttribute input "min"   "0"

        setNaturalValue input natural₀

        let get = do
                natural <- toNaturalValue input

                setSessionStorage (renderPath path type_) (toStorage natural)

                return (Value.Scalar (Natural natural))

        invoke <- register input get

        return (input, invoke, mempty)

renderInput path type_@Type.Scalar{ scalar = Monotype.JSON } = do
    maybeText <- getSessionStorage (renderPath path type_)

    maybeTextValue <- fromStorage maybeText

    let text₀ = case maybeTextValue of
            Just t -> t
            Nothing -> "null"

    return $ (,) (Value.Text text₀) do
        input <- createElement "input"

        setAttribute input "class" "form-control"

        setValue input text₀

        let get = do
                strictText <- toValue input

                setSessionStorage (renderPath path type_) (toStorage strictText)

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

renderInput path type_@Type.Scalar{ scalar = Monotype.Text } = do
    maybeText <- getSessionStorage (renderPath path type_)

    maybeTextValue <- fromStorage maybeText

    let text₀ = case maybeTextValue of
            Just t -> t
            Nothing -> ""

    return $ (,) (Value.Text text₀) do
        input <- createElement "textarea"

        setAttribute input "class" "form-control"
        setAttribute input "rows" "1"

        autoResize input

        setValue input text₀

        let get = do
                text <- toValue input

                setSessionStorage (renderPath path type_) (toStorage text)

                return (Value.Text text)

        invoke <- register input get

        return (input, invoke, mempty)

renderInput path type_@Type.Scalar{ scalar = Monotype.Key } = do
    maybeText <- getLocalStorage (renderPath path type_)

    maybeKey <- fromStorage maybeText

    let key₀ = case maybeKey of
            Just (Decode.Key k) -> k
            Nothing -> ""

    return $ (,) (Value.Scalar (Key key₀)) do
        input <- createElement "input"

        setAttribute input "type" "password"
        setAttribute input "class" "form-control"
        setAttribute input "rows" "1"

        setValue input key₀

        let get = do
                key <- toValue input

                setLocalStorage (renderPath path type_) (toStorage key)

                return (Value.Scalar (Key key))

        invoke <- register input get

        return (input, invoke, mempty)

renderInput path Type.Record{ fields = Type.Fields keyTypes _ } = do
    let outer (key, type_) = do
            (start, reader) <- renderInput (key : path) type_

            return (key, start, reader)

    triples <- traverse outer keyTypes

    let hashMap = HashMap.fromList do
            (key, start, _) <- triples

            return (key, start)

    let keyReaders = do
            (key, _, reader) <- triples

            return (key, reader)

    return $ (,) (Value.Record hashMap) do
        ref <- liftIO (IORef.newIORef hashMap)

        let inner (key, reader) = do
                let nest x = x{ renderOutput = newRenderOutput }
                      where
                        newRenderOutput mode value = do
                            let update m = (m', m')
                                  where
                                    m' = HashMap.insert key value m

                            m <- liftIO (IORef.atomicModifyIORef' ref update)

                            renderOutput x mode (Value.Record m)

                (inputField, _, refreshField) <- Reader.withReaderT nest reader

                dt <- createElement "dt"

                setAttribute dt "class" "col-auto"

                setTextContent dt (key <> ":")

                dd <- createElement "dd"

                setAttribute dd "class" "col"

                replaceChild dd inputField

                dl <- createElement "dl"

                setAttribute dl "class" "row"

                replaceChildren dl (Array.fromList [ dt, dd ])

                return (dl, refreshField)

        results <- traverse inner keyReaders

        let (children, refreshOutputs) = unzip results

        div <- createElement "div"

        replaceChildren div (Array.fromList children)

        RenderInput{ renderOutput } <- Reader.ask

        let invoke mode = do
                m <- IORef.readIORef ref

                renderOutput mode (Value.Record m)

        let refreshOutput = sequence_ refreshOutputs

        return (div, invoke, refreshOutput)

renderInput path type_@Type.Union{ alternatives = Type.Alternatives keyTypes _ } = do
    maybeAlternative <- getSessionStorage (renderPath path type_)

    let predicate = case maybeAlternative of
            Nothing -> \_ -> True
            Just a  -> \(k, _) -> k == a

    case List.find predicate keyTypes of
        Nothing -> do
            renderInputDefault path type_

        Just (key₀, type_₀) -> do
            (start, _) <- renderInput (key₀ : path) type_₀

            return $ (,) (Value.Alternative key₀ start) do
                RenderInput{ counter } <- Reader.ask

                n <- liftIO (IORef.atomicModifyIORef' counter (\a -> (a + 1, a)))

                checkedValRef <- liftIO (IORef.newIORef Nothing)

                let process (key, alternativeType) = do
                        let checked = key == key₀

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

                        case alternativeType of
                            Type.Record{ fields = Type.Fields kts _ } | null kts -> do
                                setTextContent label key
                            _ -> do
                                setTextContent label (key <> ":")


                        fieldset <- createElement "fieldset"

                        setDisabled fieldset (not checked)

                        liftIO (Monad.when checked (IORef.writeIORef checkedValRef (Just fieldset)))

                        let nest x = x{ renderOutput = newRenderOutput }
                              where
                                newRenderOutput mode value = do
                                    enabled <- getChecked input

                                    Monad.when enabled (renderOutput x mode (Alternative key value))

                        (_, reader) <- liftIO (renderInput (key : path) alternativeType)

                        (nestedInput, nestedInvoke, nestedRefresh) <- Reader.withReaderT nest reader

                        replaceChild fieldset nestedInput

                        div <- createElement "div"

                        setAttribute div "class" "form-check"

                        replaceChildren div (Array.fromList [ input, label, fieldset ])

                        liftIO do
                            let update mode = do
                                    setSessionStorage (renderPath path type_) key

                                    let adapt m = (Just fieldset, m)

                                    oldFieldset <- IORef.atomicModifyIORef' checkedValRef adapt

                                    traverse_ (\x -> setDisabled x True) oldFieldset

                                    setDisabled fieldset False

                                    nestedInvoke mode

                            callback <- Callback.asyncCallback (update Change)

                            addEventListener input "input" callback

                        let invoke mode = do
                                enabled <- getChecked input

                                Monad.when enabled (nestedInvoke mode)

                        return (div, invoke, nestedRefresh)

                results <- traverse process keyTypes

                let (children, invokes, refreshOutputs) = unzip3 results

                div <- createElement "div"

                replaceChildren div (Array.fromList children)

                let invoke mode = sequence_ (map ($ mode) invokes)

                let refreshOutput = sequence_ refreshOutputs

                liftIO (invoke Change)

                return (div, invoke, refreshOutput)

renderInput path optionalType@Type.Optional{ type_ } = do
    maybeText <- getSessionStorage (renderPath path optionalType)

    maybeEnabled <- fromStorage maybeText

    let enabled = case maybeEnabled of
            Just b -> b
            Nothing -> False

    (start, reader) <- renderInput ("?" : path) type_

    let value₀ =
            if enabled
            then Application (Value.Builtin Syntax.Some) start
            else Value.Scalar Null

    return $ (,) value₀ do
        input <- createElement "input"

        setAttribute input "type"  "checkbox"
        setAttribute input "class" "form-check-input"

        setChecked input enabled

        let nest x = x{ renderOutput = newRenderOutput }
              where
                newRenderOutput mode value = do
                    checked <- getChecked input

                    if checked
                        then do
                            renderOutput x mode (Application (Value.Builtin Syntax.Some) value)
                        else do
                            renderOutput x mode (Value.Scalar Null)

        (nestedInput, nestedInvoke, nestedRefresh) <- Reader.withReaderT nest reader

        div <- createElement "div"

        fieldset <- createElement "fieldset"

        replaceChild fieldset nestedInput

        replaceChildren div (Array.fromList [input, fieldset])

        liftIO do
            let update mode = do
                    checked <- getChecked input

                    setSessionStorage (renderPath path optionalType) (toStorage checked)

                    setDisabled fieldset (not checked)

                    nestedInvoke mode

            callback <- Callback.asyncCallback (update Change)

            addEventListener input "input" callback

            update Change

        return (div, nestedInvoke, nestedRefresh)

renderInput path listType@Type.List{ type_ } = do
    maybeText <- getSessionStorage (renderPath path listType)

    maybeIndex <- fromStorage maybeText

    let length = case maybeIndex of
            Just n -> n :: Natural
            Nothing -> 0

    let process index = do
            renderInput (Text.pack (show (index :: Integer)) : path) type_

    results <- traverse process [ 0 .. (fromIntegral length - 1) ]

    let (starts, readers) = unzip results

    let seq₀ = Seq.fromList starts

    return $ (,) (Value.List seq₀) do
        childrenRef <- liftIO (IORef.newIORef Seq.empty)

        plus <- createElement "button"

        setAttribute plus "type"  "button"
        setAttribute plus "class" "btn btn-primary"

        setTextContent plus "+"

        minus <- createElement "button"

        setAttribute minus "type"    "button"
        setAttribute minus "class"   "btn btn-danger"
        setDisplay minus "none"

        setTextContent minus "-"

        buttons <- createElement "li"

        replaceChildren buttons (Array.fromList [ plus, minus ])

        ul <- createElement "ul"

        setAttribute ul "class" "list-unstyled"

        replaceChild ul buttons

        input <- Reader.ask

        let insert maybeReader = do
                setDisplay minus "inline"

                children₀ <- IORef.readIORef childrenRef

                let index = Seq.length children₀

                reader <-  case maybeReader of
                        Just reader -> do
                            return reader
                        Nothing -> do
                            (_, reader) <- process (fromIntegral index)

                            return reader

                IORef.atomicModifyIORef' childrenRef (\s -> (s |> _Child, ()))

                setSessionStorage (renderPath path listType) (toStorage (fromIntegral index + 1 :: Natural))

                let nest x = x{ renderOutput = newRenderOutput }
                      where
                        newRenderOutput mode value = do
                            let adjust =
                                    Seq.adjust (\c -> c{ value = Just value }) index

                            let adapt s = let s' = adjust s in (s', s')

                            children <- IORef.atomicModifyIORef' childrenRef adapt

                            let values = do
                                    Child{ value = Just v } <- toList children

                                    return v

                            renderOutput x mode (Value.List (Seq.fromList values))

                result <- Maybe.runMaybeT (Reader.runReaderT reader (nest input))

                li <- createElement "li"

                case result of
                    Nothing -> do
                        return ()

                    Just (nestedInput, nestedInvoke, nestedRefresh) -> do
                        replaceChild li nestedInput

                        let adjust =
                                Seq.adjust (\c -> c{ refreshOutput = nestedRefresh, li = Just li }) index

                        IORef.atomicModifyIORef' childrenRef (\m -> (adjust m, ()))

                        nestedInvoke Change

                before buttons li

        liftIO (traverse_ (insert . Just) readers)

        insertCallback <- (liftIO . Callback.asyncCallback) (insert Nothing)

        addEventListener plus "click" insertCallback

        RenderInput{ renderOutput } <- Reader.ask

        let invoke mode = do
                children <- IORef.readIORef childrenRef

                let values = do
                        Child{ value = Just v } <- toList children

                        return v

                renderOutput mode (Value.List (Seq.fromList values))

        delete <- (liftIO . Callback.asyncCallback) do
            children <- IORef.readIORef childrenRef

            case Seq.viewr children of
                prefix :> Child{ li } -> do
                    Monad.when (Seq.null prefix) (setDisplay minus "none")

                    setSessionStorage (renderPath path listType) (toStorage (fromIntegral (Seq.length prefix) :: Natural))

                    traverse_ remove li

                    IORef.writeIORef childrenRef prefix

                    invoke Change

                EmptyR -> do
                    return ()

        addEventListener minus "click" delete

        let refreshOutput = do
                children <- IORef.readIORef childrenRef

                sequence_ do
                    Child{ refreshOutput  = nestedRefresh } <- children

                    return nestedRefresh

        return (ul, invoke, refreshOutput)

renderInput path type_ = do
    renderInputDefault path type_

data Child = Child
    { value :: Maybe Value
    , refreshOutput :: IO ()
    , li :: Maybe JSVal
    }

_Child :: Child
_Child = Child
    { value = Nothing
    , refreshOutput = mempty
    , li = Nothing
    }

renderInputDefault
    :: [Text]
    -> Type Location
    -> IO (Value, ReaderT RenderInput (MaybeT IO) (JSVal, Mode -> IO (), IO ()))
renderInputDefault path type_ = do
    maybeText <- getSessionStorage (renderPath path type_)

    let text₀ = case maybeText of
            Just t -> t
            Nothing -> ""

    return $ (,) (Value.Scalar Null) do
        RenderInput{ keyToMethods, renderOutput, status } <- Reader.ask

        textarea <- createElement "textarea"

        setDisplay textarea "none"

        error <- createElement "pre"

        setDisplay error "none"

        div <- createElement "div"

        replaceChildren div (Array.fromList [ textarea, error ])

        codeInput <- setupCodemirrorInput textarea

        setCodeValue codeInput text₀

        let get = do
                text <- getValue codeInput

                setSessionStorage (renderPath path type_) text

                let newStatus = status{ Infer.input = Infer.input status <> Code "(input)" text }

                let interpretInput = do
                        (_, value) <- Interpret.interpretWith keyToMethods [] (Just type_)

                        return value

                result <- liftIO (Exception.try (State.evalStateT interpretInput newStatus))

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
            let invoke mode = do
                    maybeValue <- Maybe.runMaybeT get

                    traverse_ (renderOutput mode) maybeValue

            callback <- Callback.asyncCallback (invoke Change)

            onChange codeInput callback

            return (div, invoke, refresh codeInput)

data DebounceStatus = Ready | Lock | Running (Async ())

debounce :: MonadIO io => (a -> IO ()) -> io (a -> IO ())
debounce f = liftIO do
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
    :: MonadIO io
    => Bool
    -- ^ Show tabs?
    -> JSVal
    -> io
        ( IO ()
        , Type Location -> Value -> (JSVal -> IO (IO ())) -> IO (IO ())
        , Text -> IO ()
        )
createForm showTabs output = liftIO do
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
            setCodeValue codeOutput (valueToText value)
            setCodeValue typeOutput (typeToText type_)

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

    counter <- IORef.newIORef 0

    params <- getSearchParams

    hasGitHub <- hasParam params "github"

    hasTutorial <- hasParam params "tutorial"

    hasExpression <- hasParam params "expression"

    hasEdit <- hasParam params "edit"

    edit <- if hasTutorial
        then do
            setParam params "edit" "true"

            return True
        else do
            if hasEdit
                then do
                    return True
                else do
                    if hasExpression || hasGitHub
                        then do
                            return False
                        else do
                            setParam params "edit" "true"

                            return True

    if edit
        then do
            title <- getElementById "title"

            setDisplay title "block"

            focus codeInput
        else do
            setDisplay (getWrapperElement codeInput) "none"

    tutorialRef <- IORef.newIORef hasTutorial

    keyToMethods <- HTTP.getMethods

    output <- getElementById "output"

    (setBusy, setSuccess, setError) <- createForm edit output

    let interpret () = do
            text <- getValue codeInput

            if text == "" || hasGitHub
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

                    let initialStatus = Status
                            { count = 0
                            , input = input_
                            , context = []
                            }

                    let interpretOutput = do
                            expression <- liftIO (Import.resolve input_)

                            (inferred, elaboratedExpression) <- Infer.infer expression

                            value <- Normalize.evaluate keyToMethods [] elaboratedExpression

                            status@Status{ context } <- State.get

                            let solvedType = Context.solveType context inferred

                            refreshOutput <- liftIO $ setSuccess solvedType value \htmlWrapper -> do
                                Reader.runReaderT (renderValue htmlWrapper solvedType value) RenderValue{ keyToMethods, counter, status, edit }

                            liftIO refreshOutput

                    result <- Exception.try (State.evalStateT interpretOutput initialStatus)


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
                        setCodeValue codeInput code

                        elements <- getElementsByClassName "example-tab"

                        Monad.forM_ elements \element -> do
                            removeClass element "active"

                        element <- getElementById id

                        addClass element "active"

                    Monad.when active (setCodeValue codeInput code)

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

    Monad.when hasGitHub do
        githubText <- getParam params "github"

        hasPrivate <- hasParam params "private"

        hasReference <- hasParam params "reference"

        reference <- case hasReference of
            False -> do
                return Syntax.Scalar
                    { location = ()
                    , scalar = Syntax.Null
                    }

            True -> do
                ref <- getParam params "reference"

                return Syntax.Application
                    { location = ()
                    , function = Syntax.Builtin
                        { location = ()
                        , builtin = Syntax.Some
                        }
                    , argument = Syntax.Text
                        { location = ()
                        , chunks = Syntax.Chunks ref []
                        }
                    }


        case Text.splitOn "/" githubText of
            owner : repository : path -> do
                if hasPrivate
                    then do
                        let expression = Syntax.Lambda
                                { location = ()
                                , nameBinding = FieldNamesBinding
                                    { fieldNamesLocation = ()
                                    , fieldNames =
                                        [ FieldName
                                            { fieldNameLocation = ()
                                            , name = "GitHub personal access token"
                                            , annotation = Just Type.Scalar
                                                { location = ()
                                                , scalar = Monotype.Key
                                                }
                                            , assignment = Nothing
                                            }
                                        ]
                                    }
                                , body = Syntax.GitHub
                                    { location = ()
                                    , import_ = True
                                    , arguments = Syntax.Record
                                        { location = ()
                                        , fieldValues =
                                            [ ( "key"
                                              , Syntax.Application
                                                  { location = ()
                                                  , function = Syntax.Builtin
                                                      { location = ()
                                                      , builtin = Syntax.Some
                                                      }
                                                  , argument = "GitHub personal access token"
                                                  }
                                              )
                                            , ( "owner"
                                              , Syntax.Text
                                                  { location = ()
                                                  , chunks = Syntax.Chunks owner []
                                                  }
                                              )
                                            , ( "repository"
                                              , Syntax.Text
                                                  { location = ()
                                                  , chunks = Syntax.Chunks repository []
                                                  }
                                              )
                                            , ("reference", reference)
                                            , ( "path"
                                              , Syntax.Text
                                                  { location = ()
                                                  , chunks = Syntax.Chunks (Text.intercalate "/" path) []
                                                  }
                                              )
                                            ]
                                        }
                                    , schema = Nothing
                                    }
                                }

                        setCodeValue codeInput (Pretty.toSmart (expression :: Syntax () Void))

                    else do
                        let expression = Syntax.GitHub
                                { location = ()
                                , import_ = True
                                , arguments = Syntax.Record
                                    { location = ()
                                    , fieldValues =
                                        [ ( "key"
                                          , Syntax.Scalar
                                              { location = ()
                                              , scalar = Syntax.Null
                                              }
                                          )
                                        , ( "owner"
                                          , Syntax.Text
                                              { location = ()
                                              , chunks = Syntax.Chunks owner []
                                              }
                                          )
                                        , ( "repository"
                                          , Syntax.Text
                                              { location = ()
                                              , chunks = Syntax.Chunks repository []
                                              }
                                          )
                                        , ("reference", reference)
                                        , ( "path"
                                          , Syntax.Text
                                              { location = ()
                                              , chunks = Syntax.Chunks (Text.intercalate "/" path) []
                                              }
                                          )
                                        ]
                                    }
                                , schema = Nothing
                                }

                        setCodeValue codeInput (Pretty.toSmart (expression :: Syntax () Void))
            _ -> do
                return ()

    Monad.when hasExpression do
        expression <- getParam params "expression"

        setCodeValue codeInput (URI.Encode.decodeText expression)

    debouncedInterpret ()
