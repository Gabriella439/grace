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

import Control.Applicative (empty, liftA2, (<|>))
import Control.Concurrent.Async (Async, Concurrently(..))
import Control.Exception.Safe (catch, Exception(..), SomeException)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import Data.Foldable (toList, traverse_)
import Data.IORef (IORef)
import Data.Maybe (isJust)
import Data.JSString (JSString)
import Data.Sequence (ViewR(..), (|>))
import Data.Text (Text)
import Data.Traversable (forM)
import Data.Void (Void)
import Grace.Monad (Status(..))
import Grace.Type (Type(..))
import GHCJS.Foreign.Callback (Callback)
import GHCJS.Types (JSVal)
import Grace.Decode (FromGrace)
import Grace.Encode (ToGrace(..))
import Grace.Input (Input(..))
import Grace.Location (Location)
import Grace.Monotype (RemainingFields(..))
import Grace.Syntax (Binding(..), NameBinding(..), Scalar(..), Syntax)
import Grace.Value (Value(..))
import JavaScript.Array (JSArray)
import Numeric.Natural (Natural)
import Prelude hiding (div, error, id, length, span, subtract)
import System.FilePath ((</>))

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
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
import qualified Grace.Import as Import
import qualified Grace.Infer as Infer
import qualified Grace.Input as Input
import qualified Grace.Interpret as Interpret
import qualified Grace.Monad as Grace
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

instance (Monad m, Semigroup a) => Semigroup (StateT r m a) where
    (<>) = liftA2 (<>)

instance (Monad m, Monoid a) => Monoid (StateT r m a) where
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

replaceChildren :: MonadIO io => JSVal -> [JSVal] -> io ()
replaceChildren a b = liftIO (replaceChildren_ a (Array.fromList b))

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

foreign import javascript unsafe "CodeMirror.fromTextArea($1, { mode: 'python', lineNumbers: true, viewportMargin: Infinity, extraKeys: { 'Shift-Tab': false, 'Tab': false } })"
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

foreign import javascript unsafe "sessionStorage.removeItem($1)"
    removeSessionStorage_ :: JSString -> IO ()

removeSessionStorage :: MonadIO io => Text -> io ()
removeSessionStorage a = liftIO (removeSessionStorage_ (fromText a))

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
valueToText = Pretty.renderStrict False 80

hideElement :: MonadIO io => JSVal -> io ()
hideElement element = do
    setDisplay element "none"
    addClass element "grace-ignore"

showElement :: MonadIO io => Text -> JSVal -> io ()
showElement display element = do
    setDisplay element display
    removeClass element "grace-ignore"

data Config = Config
    { counter :: IORef Natural
    , status :: Status
    , input :: Input
    , edit :: Bool
    }

renderValue
    :: JSVal
    -> Type Location
    -> Value
    -> ReaderT Config IO (IO ())
renderValue parent Type.Optional{ type_ } value =
    renderValue parent type_ value

renderValue parent _ (Value.Text text) = do
    printable <- createElement "div"
    addClass printable "grace-printable"

    markdown <- createElement "div"
    addClass markdown "grace-output-text"

    let innerHTML = if text == "" then "<p>\x200B</p>" else markdownToHTML text
    setInnerHTML markdown innerHTML

    sidebar <- createElement "div"
    addClass sidebar "grace-printable-buttons"

    printButton <- createElement "button"
    addClass printButton "grace-print"
    setAttribute printButton "type" "button"
    setInnerText printButton "Print"
    hideElement printButton

    printCallback <- liftIO (Callback.asyncCallback (printElement markdown))
    addEventListener printButton "click" printCallback

    copyButton <- createElement "button"
    addClass copyButton "grace-copy"
    setAttribute copyButton "type" "button"
    setInnerText copyButton "Copy"
    hideElement copyButton

    copyCallback <- liftIO (Callback.asyncCallback (writeClipboard text))
    addEventListener copyButton "click" copyCallback

    showCallback <- (liftIO . Callback.asyncCallback) do
        showElement "inline-block" printButton
        showElement "inline-block" copyButton

    hideCallback <- (liftIO . Callback.asyncCallback) do
        hideElement printButton
        hideElement copyButton

    addEventListener parent "mouseenter" showCallback
    addEventListener parent "mouseleave" hideCallback

    replaceChildren sidebar [ printButton, copyButton ]
    replaceChildren printable [ markdown, sidebar ]
    replaceChild parent printable

    mempty

renderValue parent _ (Value.Scalar (Bool bool)) = do
    input <- createElement "input"
    addClass input "grace-output-bool"
    setAttribute input "type" "checkbox"
    setDisabled input True

    Monad.when bool (setAttribute input "checked" "")

    replaceChild parent input

    mempty

renderValue parent _ (Value.Scalar Null) = do
    span <- createElement "span"
    addClass span "grace-output-json"
    setTextContent span "∅"

    replaceChild parent span

    mempty

renderValue parent _ value@Value.Scalar{} = do
    span <- createElement "span"
    addClass span "grace-output-json"
    setTextContent span (valueToText value)

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
        addClass li "grace-output-element"

        refreshOutput <- renderValue li inner value

        return (li, refreshOutput)

    let (lis, refreshOutputs) = unzip (toList results)

    ul <- createElement "ul"
    addClass ul "grace-output-list"
    addClass ul "grace-stack"

    replaceChildren ul lis

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
            addClass dt "grace-output-field-name"

            setTextContent dt key

            dd <- createElement "dd"
            addClass dt "grace-output-field-value"

            refreshOutput <- renderValue dd type_ value

            definition <- createElement "div"
            replaceChildren definition [ dt, dd ]

            return (definition, refreshOutput)

    result <- HashMap.traverseWithKey process keyValues

    let (definitions, refreshOutputs) = unzip (HashMap.elems result)

    dl <- createElement "dl"
    addClass dl "grace-output-record"
    addClass dl "grace-stack"

    replaceChildren dl definitions

    replaceChild parent dl

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
    r@Config{ status, input = input_ } <- Reader.ask

    outputVal <- createElement "div"
    addClass outputVal "grace-result"

    let hasEffects = Lens.has Value.effects function

    -- TODO: Restore interior tabs with appropriate styling
    let tabbed = False -- edit && hasEffects

    (setBusy, setSuccess, setError) <- createForm tabbed outputVal

    let render Nothing = do
            replaceChildren outputVal []

        render (Just value) = do
            setBusy

            let interpretOutput = do
                    newValue <- Normalize.apply function value

                    status_@Status{ context } <- State.get

                    let completedType = Context.complete context output

                    let solvedType = Context.solveType context output

                    refreshOutput <- liftIO $ setSuccess completedType newValue \htmlWrapper -> do
                        Reader.runReaderT (renderValue htmlWrapper solvedType newValue) (r :: Config){ status = status_ }

                    liftIO refreshOutput

            eitherResult <- liftIO (Exception.try (Grace.evalGrace input_ status interpretOutput))

            case eitherResult of
                Left exception -> do
                    setError (Text.pack (displayException (exception :: SomeException)))

                Right x -> do
                    return x

    debouncedRender <- debounce render

    let renderOutput Change | hasEffects = mempty
        renderOutput _                   = debouncedRender

    (_, reader) <- renderInput [] input

    i <- Reader.ask

    result <- liftIO (Maybe.runMaybeT (Reader.runReaderT (reader renderOutput) i))

    case result of
        Nothing -> do
            replaceChildren parent [ ]

            mempty

        Just (inputVal, invoke, refreshOutput) -> do
            if hasEffects
                then do
                    button <- createElement "button"
                    addClass button "grace-submit"
                    setAttribute button "type" "button"
                    setTextContent button "Submit"

                    buttons <- createElement "div"
                    addClass buttons "grace-cluster"
                    replaceChild buttons button

                    hr <- createElement "hr"
                    addClass hr "grace-horizontal-rule"

                    stack <- createElement "div"
                    addClass stack "grace-stack-large"

                    callback <- (liftIO . Callback.asyncCallback) do
                        replaceChildren stack [ inputVal, buttons, hr, outputVal ]

                        invoke Submit

                    addEventListener button "click" callback

                    replaceChildren stack [ inputVal, buttons ]

                    replaceChild parent stack

                else do
                    liftIO (invoke Submit)

                    hr <- createElement "hr"
                    addClass hr "grace-horizontal-rule"

                    stack <- createElement "div"
                    addClass stack "grace-stack-large"

                    replaceChildren stack [ inputVal, hr, outputVal ]

                    replaceChild parent stack

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
    addClass code "grace-output-default"

    setTextContent code (valueToText value)

    replaceChild parent code

    mempty

data Mode
    = Change
    -- ^ The function is being run in response to a form input changing
    | Submit
    -- ^ The function is being run in response to form submission

register
    :: MonadIO m
    => JSVal
    -> MaybeT IO Value
    -> (Mode -> Maybe Value -> IO ())
    -> ReaderT Config m (Mode -> IO ())
register input get renderOutput = liftIO do
    let invoke mode = do
            maybeValue <- Maybe.runMaybeT get

            renderOutput mode maybeValue

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

fromStorage :: (MonadIO io, FromGrace a) => Maybe Text -> io (Maybe a)
fromStorage Nothing = do
    return Nothing
fromStorage (Just text) = liftIO do
    load `catch` \(_ :: SomeException) -> return Nothing
  where
    load = do
        a <- Interpret.load (Code "(storage)" text)
        return (Just a)

toStorage :: ToGrace a => a -> Text
toStorage a = Pretty.toText (encode a)

renderInput
    :: [Text]
    -> Type Location
    -> ReaderT Config IO
          ( Maybe Value
          ,     (Mode -> Maybe Value -> IO ())
            ->  ReaderT Config (MaybeT IO) (JSVal, Mode -> IO (), IO ())
          )
renderInput path type_@Type.Scalar{ scalar = Monotype.Bool } = do
    maybeText <- getSessionStorage (renderPath path type_)

    maybeBool <- fromStorage maybeText

    let bool₀ = case maybeBool of
            Just b -> b
            Nothing -> False

    let maybeValue₀ = Just (Value.Scalar (Bool bool₀))

    return $ (,) maybeValue₀ \renderOutput -> do
        input <- createElement "input"
        addClass input "grace-input-bool"
        setAttribute input "type" "checkbox"

        setChecked input bool₀

        let get = do
                bool <- getChecked input

                setSessionStorage (renderPath path type_) (toStorage bool)

                return (Value.Scalar (Bool bool))

        invoke <- register input get renderOutput

        return (input, invoke, mempty)

renderInput path type_@Type.Scalar{ scalar = Monotype.Real } = do
    maybeText <- getSessionStorage (renderPath path type_)

    maybeScientific <- fromStorage maybeText

    let scientific₀ = case maybeScientific of
            Just s -> s
            Nothing -> 0

    let maybeValue₀ = Just (Value.Scalar (Real scientific₀))

    return $ (,) maybeValue₀ \renderOutput -> do
        input <- createElement "input"
        addClass input "grace-input-json"
        setAttribute input "type"  "number"
        setAttribute input "step"  "any"
        setAttribute input "value" "0"

        setDoubleValue input (Scientific.toRealFloat scientific₀)

        let get = do
                double <- toDoubleValue input

                setSessionStorage (renderPath path type_) (toStorage double)

                return (Value.Scalar (Real (Scientific.fromFloatDigits double)))

        invoke <- register input get renderOutput

        return (input, invoke, mempty)

renderInput path type_@Type.Scalar{ scalar = Monotype.Integer } = do
    maybeText <- getSessionStorage (renderPath path type_)

    maybeInteger <- fromStorage maybeText

    let integer₀ = case maybeInteger of
            Just i -> i
            Nothing -> 0

    let maybeValue₀ = Just (Value.Scalar (Integer integer₀))

    return $ (,) maybeValue₀ \renderOutput -> do
        input <- createElement "input"
        addClass input "grace-input-json"
        setAttribute input "type"  "number"
        setAttribute input "value" "0"

        setIntegerValue input integer₀

        let get = do
                integer <- toIntegerValue input

                setSessionStorage (renderPath path type_) (toStorage integer)

                return (Value.Scalar (Integer integer))

        invoke <- register input get renderOutput

        return (input, invoke, mempty)

renderInput path type_@Type.Scalar{ scalar = Monotype.Natural } = do
    maybeText <- getSessionStorage (renderPath path type_)

    maybeNatural <- fromStorage maybeText

    let natural₀ = case maybeNatural of
            Just n -> n
            Nothing -> 0

    let maybeValue₀ = Just (Value.Scalar (Natural natural₀))

    return $ (,) maybeValue₀ \renderOutput -> do
        input <- createElement "input"
        addClass input "grace-input-json"
        setAttribute input "type"  "number"
        setAttribute input "value" "0"
        setAttribute input "min"   "0"

        setNaturalValue input natural₀

        let get = do
                natural <- toNaturalValue input

                setSessionStorage (renderPath path type_) (toStorage natural)

                return (Value.Scalar (Natural natural))

        invoke <- register input get renderOutput

        return (input, invoke, mempty)

renderInput path type_@Type.Scalar{ scalar = Monotype.JSON } = do
    maybeText <- getSessionStorage (renderPath path type_)

    maybeTextValue <- fromStorage maybeText

    let text₀ = case maybeTextValue of
            Just t -> t
            Nothing -> "null"

    let maybeValue₀ = do
            let lazyText = Text.Lazy.fromStrict text₀

            Aeson.decode (Text.Encoding.encodeUtf8 lazyText)

    return $ (,) maybeValue₀ \renderOutput -> do
        input <- createElement "input"
        addClass input "grace-input-json"
        addClass input "grace-input-json-valid"
        setAttribute input "placeholder" "Enter JSON…"
        setAttribute input "data-1p-ignore" ""

        setValue input text₀

        let get = do
                strictText <- toValue input

                setSessionStorage (renderPath path type_) (toStorage strictText)

                let lazyText = Text.Lazy.fromStrict strictText

                case Aeson.eitherDecode (Text.Encoding.encodeUtf8 lazyText) of
                    Left _ -> do
                        removeClass input "grace-input-json-valid"
                        addClass input "grace-input-json-invalid"

                        empty

                    Right value -> do
                        removeClass input "grace-input-json-invalid"
                        addClass input "grace-input-json-valid"

                        return value

        invoke <- register input get renderOutput

        return (input, invoke, mempty)

renderInput path type_@Type.Scalar{ scalar = Monotype.Text } = do
    maybeText <- getSessionStorage (renderPath path type_)

    maybeTextValue <- fromStorage maybeText

    let text₀ = case maybeTextValue of
            Just t -> t
            Nothing -> ""

    let maybeValue₀ = Just (Value.Text text₀)

    return $ (,) maybeValue₀ \renderOutput -> do
        input <- createElement "textarea"
        addClass input "grace-input-text"
        setAttribute input "rows" "1"
        setAttribute input "placeholder" "Enter text…"
        setAttribute input "data-1p-ignore" ""

        autoResize input

        setValue input text₀

        let get = do
                text <- toValue input

                setSessionStorage (renderPath path type_) (toStorage text)

                return (Value.Text text)

        invoke <- register input get renderOutput

        return (input, invoke, mempty)

renderInput path type_@Type.Scalar{ scalar = Monotype.Key } = do
    maybeText <- getLocalStorage (renderPath path type_)

    maybeKey <- fromStorage maybeText

    let key₀ = case maybeKey of
            Just (Decode.Key k) -> k
            Nothing -> ""

    let maybeValue₀ = Just (Value.Scalar (Key key₀))

    return $ (,) maybeValue₀ \renderOutput -> do
        input <- createElement "input"
        addClass input "grace-input-json"
        setAttribute input "placeholder" "Enter key…"
        setAttribute input "type" "password"
        setAttribute input "rows" "1"

        setValue input key₀

        let get = do
                key <- toValue input

                setLocalStorage (renderPath path type_) (toStorage key)

                return (Value.Scalar (Key key))

        invoke <- register input get renderOutput

        return (input, invoke, mempty)

renderInput path Type.Record{ fields = Type.Fields keyTypes _ } = do
    let outer (key, type_) = do
            (maybeStart, reader) <- renderInput (key : path) type_

            return (key, (key, maybeStart), (key, reader))

    result <- traverse outer keyTypes

    let (keys, keyMaybeStarts, keyReaders) = unzip3 result

    let hashMap = HashMap.fromList keyMaybeStarts

    let maybeValue₀ = do
            keyStarts <- sequence hashMap

            Monad.guard (HashMap.keys keyStarts == keys)

            return (Value.Record keyStarts)

    return $ (,) maybeValue₀ \renderOutput -> do
        ref <- liftIO (IORef.newIORef hashMap)

        let inner (key, reader) = do
                let newRenderOutput mode maybeValue = do
                        let update m = (m', m')
                              where
                                m' = HashMap.insert key maybeValue m

                        keyMaybeValues <- liftIO (IORef.atomicModifyIORef' ref update)

                        let maybeRecord = do
                                keyValues <- sequence keyMaybeValues

                                Monad.guard (HashMap.keys keyValues == keys)

                                return (Value.Record keyValues)

                        renderOutput mode maybeRecord

                (inputField, _, refreshField) <- reader newRenderOutput

                dt <- createElement "dt"
                addClass dt "grace-input-field-name"

                setTextContent dt key

                dd <- createElement "dd"
                addClass dt "grace-input-field-value"

                replaceChild dd inputField

                definition <- createElement "div"
                replaceChildren definition [ dt, dd ]

                return (definition, refreshField)

        results <- traverse inner keyReaders

        let (definitions, refreshOutputs) = unzip results

        dl <- createElement "dl"
        addClass dl "grace-input-record"
        addClass dl "grace-stack"

        replaceChildren dl definitions

        let invoke mode = do
                keyMaybeValues <- IORef.readIORef ref

                let maybeRecord = do
                        keyValues <- sequence keyMaybeValues

                        return (Value.Record keyValues)

                renderOutput mode maybeRecord

        let refreshOutput = sequence_ refreshOutputs

        return (dl, invoke, refreshOutput)

renderInput path type_@Type.Union{ alternatives = Type.Alternatives keyTypes _ } = do
    maybeAlternative <- getSessionStorage (renderPath path type_)

    let predicate = case maybeAlternative of
            Nothing -> \_ -> True
            Just a  -> \(k, _) -> k == a

    case List.find predicate keyTypes of
        Nothing -> do
            renderInputDefault path type_

        Just (key₀, type_₀) -> do
            (maybeStart, _) <- renderInput (key₀ : path) type_₀

            let maybeValue₀ = do
                    start <- maybeStart

                    return (Value.Alternative key₀ start)

            return $ (,) maybeValue₀ \renderOutput -> do
                Config{ counter } <- Reader.ask

                n <- liftIO (IORef.atomicModifyIORef' counter (\a -> (a + 1, a)))

                checkedValRef <- liftIO (IORef.newIORef Nothing)

                let process (key, alternativeType) = do
                        let checked = key == key₀

                        let name = "radio" <> Text.pack (show n)

                        let id = name <> "-" <> key

                        input <- createElement "input"
                        addClass input "grace-input-alternative-radio"
                        setAttribute input "type"  "radio"
                        setAttribute input "name"  name
                        setAttribute input "id"    id
                        Monad.when checked (setAttribute input "checked" "")

                        box <- createElement "div"
                        addClass box "grace-input-alternative-radio-box"
                        replaceChild box input

                        inputStack <- createElement "div"
                        addClass inputStack "grace-stack"
                        replaceChild inputStack box

                        let newRenderOutput mode maybeValue = do
                                enabled <- getChecked input

                                let maybeResult = do
                                        value <- maybeValue

                                        return (Alternative key value)

                                Monad.when enabled (renderOutput mode maybeResult)

                        (_, reader) <- hoist lift (renderInput (key : path) alternativeType)

                        (nestedInput, nestedInvoke, nestedRefresh) <- reader newRenderOutput

                        label <- createElement "label"
                        addClass label "grace-input-alternative-label"
                        setAttribute label "for"   id
                        setTextContent label key

                        fieldset <- createElement "fieldset"
                        setDisabled fieldset (not checked)

                        replaceChild fieldset nestedInput

                        alternativeStack <- createElement "div"
                        addClass alternativeStack "grace-input-alternative"
                        addClass alternativeStack "grace-stack"

                        case alternativeType of
                            Type.Record{ fields = Type.Fields kts _ } | null kts -> do
                                replaceChild alternativeStack label
                            _ -> do
                                replaceChildren alternativeStack [ label, fieldset ]

                        sidebar <- createElement "div"
                        addClass sidebar "grace-input-alternative-selection"

                        replaceChildren sidebar [ inputStack, alternativeStack]

                        liftIO (Monad.when checked (IORef.writeIORef checkedValRef (Just fieldset)))

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

                        return (sidebar, invoke, nestedRefresh)

                results <- traverse process keyTypes

                let (children, invokes, refreshOutputs) = unzip3 results

                div <- createElement "div"
                addClass div "grace-input-union"

                replaceChildren div children

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

    (maybeStart, reader) <- renderInput ("?" : path) type_

    let maybeValue₀ = if enabled
            then do
                start <- maybeStart

                return (Application (Value.Builtin Syntax.Some) start)
            else do
                return (Value.Scalar Null)

    return $ (,) maybeValue₀ \renderOutput -> do
        input <- createElement "input"
        addClass input "grace-input-bool"
        setAttribute input "type"  "checkbox"

        setChecked input enabled

        let newRenderOutput mode maybeValue = do
                checked <- getChecked input

                if checked
                    then do
                        let maybeSomeValue = do
                                value <- maybeValue

                                return (Application (Value.Builtin Syntax.Some) value)

                        renderOutput mode maybeSomeValue
                    else do
                        renderOutput mode (Just (Value.Scalar Null))

        (nestedInput, nestedInvoke, nestedRefresh) <- reader newRenderOutput

        box <- createElement "div"
        addClass box "grace-input-bool-box"
        replaceChild box input

        sidebar <- createElement "div"
        addClass sidebar "grace-stack"
        replaceChild sidebar box

        div <- createElement "div"
        addClass div "grace-input-optional"

        fieldset <- createElement "fieldset"

        replaceChild fieldset nestedInput

        replaceChildren div [sidebar, fieldset]

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

    let (maybeStarts, readers) = unzip results

    let maybeValue₀ = do
            starts <- sequence maybeStarts

            return (Value.List (Seq.fromList starts))

    return $ (,) maybeValue₀ \renderOutput -> do
        childrenRef <- liftIO (IORef.newIORef Seq.empty)

        plus <- createElement "button"
        addClass plus "grace-input-list-plus"
        setAttribute plus "type" "button"

        setTextContent plus "+"

        minus <- createElement "button"
        addClass minus "grace-input-list-minus"
        setAttribute minus "type" "button"
        hideElement minus

        setTextContent minus "-"

        buttons <- createElement "li"
        addClass buttons "grace-input-list-element"
        addClass buttons "grace-cluster-start"

        replaceChildren buttons [ plus, minus ]

        ul <- createElement "ul"
        addClass ul "grace-input-list"
        addClass ul "grace-stack"

        replaceChild ul buttons

        input <- Reader.ask

        let insert maybeReader = do
                showElement "inline-block" minus

                children₀ <- IORef.readIORef childrenRef

                let index = Seq.length children₀

                reader <-  case maybeReader of
                    Just reader -> do
                        return reader
                    Nothing -> do
                        (_, reader) <- Reader.runReaderT (process (fromIntegral index)) input

                        return reader

                IORef.atomicModifyIORef' childrenRef (\s -> (s |> _Child, ()))

                setSessionStorage (renderPath path listType) (toStorage (fromIntegral index + 1 :: Natural))

                let newRenderOutput mode maybeValue = do
                        let adjust =
                                Seq.adjust (\c -> c{ value = maybeValue }) index

                        let adapt s = let s' = adjust s in (s', s')

                        children <- IORef.atomicModifyIORef' childrenRef adapt

                        let maybeList = do
                                values <- traverse (\Child{ value } -> value) children

                                return (Value.List values)

                        renderOutput mode maybeList

                result <- Maybe.runMaybeT (Reader.runReaderT (reader newRenderOutput) input)

                li <- createElement "li"
                addClass li "grace-input-list-element"

                before buttons li

                case result of
                    Nothing -> do
                        return ()

                    Just (nestedInput, nestedInvoke, nestedRefresh) -> do
                        replaceChild li nestedInput

                        let adjust =
                                Seq.adjust (\c -> c{ refreshOutput = nestedRefresh, li = Just li }) index

                        IORef.atomicModifyIORef' childrenRef (\m -> (adjust m, ()))

                        nestedRefresh

                        nestedInvoke Change

        liftIO (traverse_ (insert . Just) readers)

        insertCallback <- (liftIO . Callback.asyncCallback) (insert Nothing)

        addEventListener plus "click" insertCallback

        let invoke mode = do
                children <- IORef.readIORef childrenRef

                let maybeList = do
                        values <- traverse (\Child{ value } -> value) children

                        return (Value.List values)

                renderOutput mode maybeList

        delete <- (liftIO . Callback.asyncCallback) do
            children <- IORef.readIORef childrenRef

            case Seq.viewr children of
                prefix :> Child{ li } -> do
                    Monad.when (Seq.null prefix) (hideElement minus)

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
    -> ReaderT Config IO
          ( Maybe Value
          ,   (Mode -> Maybe Value -> IO ())
          ->  ReaderT Config (MaybeT IO) (JSVal, Mode -> IO (), IO ())
          )
renderInputDefault path type_ = do
    Config{ status = status₀, input = input₀ } <- Reader.ask

    maybeText <- getSessionStorage (renderPath path type_)

    let text₀ = case maybeText of
            Just t -> t
            Nothing -> ""

    let textToValue text = do
            let newInput = input₀ <> Code "(input)" text

            let interpretInput = do
                    (_, value) <- Interpret.interpretWith [] (Just type_)

                    return value

            Grace.evalGrace newInput status₀ interpretInput

    result₀ <- Exception.try (textToValue text₀)

    let maybeValue₀ = case result₀ of
            Left (_ :: SomeException) -> Nothing
            Right v -> Just v

    return $ (,) maybeValue₀ \renderOutput -> do
        Config{ status, input } <- Reader.ask

        textarea <- createElement "textarea"
        setAttribute textarea "placeholder" "Enter code…"

        hideElement textarea

        error <- createElement "pre"
        addClass error "grace-error"

        hideElement error

        div <- createElement "div"
        addClass div "grace-pane"
        addClass div "grace-stack"

        replaceChildren div [ textarea, error ]

        codeInput <- setupCodemirrorInput textarea

        setCodeValue codeInput text₀

        let get = do
                text <- getValue codeInput

                setSessionStorage (renderPath path type_) text

                let newInput = input <> Code "(input)" text

                let interpretInput = do
                        (_, value) <- Interpret.interpretWith [] (Just type_)

                        return value

                result <- liftIO (Exception.try (Grace.evalGrace newInput status interpretInput))

                case result of
                    Left exception -> do
                        if (text == "")
                            then do
                                hideElement error
                            else do
                                setTextContent error (Text.pack (displayException (exception :: SomeException)))

                                showElement "block" error

                        empty

                    Right value -> do
                        hideElement error

                        setTextContent error ""

                        return value

        liftIO do
            let invoke mode = do
                    maybeValue <- Maybe.runMaybeT get

                    renderOutput mode maybeValue

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
            tab <- createElement "button"
            addClass tab "grace-tab"
            setAttribute tab "type" "button"
            setTextContent tab name

            return tab

    formTab <- toTab "Form"
    codeTab <- toTab "Code"
    typeTab <- toTab "Type"

    let tabs = [ formTab, codeTab, typeTab ]

    tabsList <- createElement "div"
    addClass tabsList "grace-tabs"

    replaceChildren tabsList tabs

    pane <- createElement "div"
    addClass pane "grace-pane"
    Monad.when showTabs (addClass pane "grace-tabbed")

    success <- createElement "div"
    addClass success "grace-success"

    let successChildren = if showTabs then [ tabsList, pane ] else [ pane ]

    replaceChildren success successChildren

    codemirrorBuffer <- getElementById "codemirror-buffer"

    let createCodemirrorOutput = do
            textarea <- createElement "textarea"
            setAttribute textarea "placeholder" "Enter code…"

            replaceChild codemirrorBuffer textarea

            codeMirror <- setupCodemirrorOutput textarea

            replaceChildren codemirrorBuffer []

            return (codeMirror, getWrapperElement codeMirror)

    htmlWrapper <- createElement "form"
    addClass htmlWrapper "grace-form"
    setAttribute htmlWrapper "autocomplete" "off"

    (codeOutput, codeWrapper) <- createCodemirrorOutput
    (typeOutput, typeWrapper) <- createCodemirrorOutput

    let registerTabCallback selectedTab action = do
            callback <- Callback.asyncCallback do
                let deselect tab = removeClass tab "grace-tab-selected"

                traverse_ deselect tabs

                addClass selectedTab "grace-tab-selected"

                action

            addEventListener selectedTab "click" callback

    registerTabCallback formTab do
        replaceChild pane htmlWrapper

    registerTabCallback codeTab do
        replaceChild pane codeWrapper

        refresh codeOutput

    registerTabCallback typeTab do
        replaceChild pane typeWrapper

        refresh typeOutput

    addClass formTab "grace-tab-selected"

    replaceChild pane htmlWrapper

    spinner <- do
        spinner <- createElement "div"
        addClass spinner "grace-spinner"
        setAttribute spinner "role"     "status"
        setAttribute spinner "overflow" "hidden"

        return spinner

    error <- createElement "pre"
    addClass error "grace-error"

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

    maybeExpression₀ <- do
        hasExpression <- hasParam params "expression"

        if hasExpression
            then do
                expression <- getParam params "expression"

                return (Just expression)
            else do
                return Nothing

    maybeExpression₁ <- getSessionStorage "expression"

    let maybeExpression = maybeExpression₀ <|> maybeExpression₁

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
                    if isJust maybeExpression || hasGitHub
                        then do
                            return False
                        else do
                            setParam params "edit" "true"

                            return True

    if edit
        then do
            title <- getElementById "title"

            showElement "block" title

            focus codeInput
        else do
            hideElement (getWrapperElement codeInput)

    output <- getElementById "output"
    addClass output "grace-result"

    (setBusy, setSuccess, setError) <- createForm edit output

    let interpret () = do
            text <- getValue codeInput

            if text == "" || hasGitHub
                then removeSessionStorage "expression"
                else setSessionStorage "expression" (URI.Encode.encodeText text)

            tutorial <- hasParam params "tutorial"

            saveSearchParams params

            if not tutorial && Text.null text
                then showElement "inline-block" startTutorial
                else hideElement startTutorial

            if  | Text.null text -> do
                    hideElement output
                    replaceChildren output []

                | otherwise -> do
                    hideElement startTutorial

                    setBusy

                    let input_ = Code "(input)" text

                    let initialStatus = Status
                            { count = 0
                            , context = []
                            }

                    let interpretOutput = do
                            expression <- liftIO (Import.resolve Input.AsCode input_)

                            (inferred, elaboratedExpression) <- Infer.infer expression

                            value <- Normalize.evaluate [] elaboratedExpression

                            status@Status{ context } <- State.get

                            let completedType =
                                    Context.complete context inferred

                            let solvedType = Context.solveType context inferred

                            refreshOutput <- liftIO $ setSuccess completedType value \htmlWrapper -> do
                                let config = Config
                                        { counter
                                        , status
                                        , input = input_
                                        , edit
                                        }

                                Reader.runReaderT (renderValue htmlWrapper solvedType value) config

                            liftIO refreshOutput

                    result <- Exception.try (Grace.evalGrace input_ initialStatus interpretOutput)


                    case result of
                        Left exception -> do
                            setError (Text.pack (displayException (exception :: SomeException)))
                        Right () -> do
                            return ()

                    showElement "block" output

    debouncedInterpret <- debounce interpret

    inputCallback <- Callback.asyncCallback (debouncedInterpret ())

    onChange codeInput inputCallback

    enableTutorialMVar <- MVar.newMVar Nothing

    let loadTutorial = do
            stopTutorial <- createElement "button"
            addClass stopTutorial "grace-tutorial-end"

            setAttribute stopTutorial "type" "button"
            setAttribute stopTutorial "id"   "stop-tutorial"

            setTextContent stopTutorial "Exit the tutorial"

            hideElement stopTutorial

            let createExample (name, file) = do
                    n <- State.get

                    State.put (n + 1)

                    (return . Concurrently) do
                        text <- DataFile.readDataFile ("examples" </> "tutorial" </> file)

                        let code = Text.strip text

                        let id = "example-" <> Text.pack (show n)

                        tab <- createElement "button"
                        addClass tab "example-tab"
                        addClass tab "grace-tab"

                        setAttribute tab "id"           id
                        setAttribute tab "aria-current" "page"
                        setAttribute tab "type"         "button"

                        setTextContent tab name

                        let click = do
                                setCodeValue codeInput code

                                elements <- getElementsByClassName "example-tab"

                                Monad.forM_ elements \element -> do
                                    removeClass element "grace-tab-selected"

                                element <- getElementById id

                                addClass element "grace-tab-selected"

                        callback <- Callback.asyncCallback click

                        addEventListener tab "click" callback

                        return [(tab, click)]

            let examples =
                    [ ("Hello, world!", "hello.ffg"     )
                    , ("HTML"         , "html.ffg"      )
                    , ("Data"         , "data.ffg"      )
                    , ("Prompting"    , "prompting.ffg" )
                    , ("Variables"    , "variables.ffg" )
                    , ("Functions"    , "functions.ffg" )
                    , ("Imports"      , "imports.ffg"   )
                    , ("Coding"       , "coding.ffg"    )
                    , ("Conclusion"   , "conclusion.ffg")
                    ]

            results <- Async.runConcurrently (State.evalState (foldMap createExample examples) (0 :: Int))

            let (tabs, clickFirstExample : _) = unzip results

            navigationBar <- createElement "div"
            addClass navigationBar "grace-tabs"

            replaceChildren navigationBar tabs

            hideElement navigationBar

            before inputArea navigationBar

            stopTutorialCallback <- Callback.asyncCallback do
                deleteParam params "tutorial"

                saveSearchParams params

                hideElement stopTutorial
                hideElement navigationBar

                text <- getValue codeInput

                if Text.null text
                    then do
                        showElement "inline-block" startTutorial
                    else do
                        hideElement startTutorial

                focus codeInput

            addEventListener stopTutorial "click" stopTutorialCallback

            after startTutorial stopTutorial

            return do
                setParam params "tutorial" "true"

                saveSearchParams params

                clickFirstExample

                hideElement startTutorial
                showElement "flex" navigationBar
                showElement "inline-block" stopTutorial

                focus codeInput

    let enableTutorial = do
            enable <- MVar.modifyMVar enableTutorialMVar \maybeEnable -> do
                enable <- case maybeEnable of
                    Nothing     -> loadTutorial
                    Just enable -> return enable

                return (Just enable, enable)

            enable

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
                                , binding = RecordBinding
                                    { fieldNamesLocation = ()
                                    , fieldNames =
                                        [ NameBinding
                                            { nameLocation = ()
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
                                            [ Syntax.Definition
                                                { nameLocation = ()
                                                , name = "key"
                                                , bindings = []
                                                , annotation = Nothing
                                                , assignment = Syntax.Application
                                                    { location = ()
                                                    , function = Syntax.Builtin
                                                        { location = ()
                                                        , builtin = Syntax.Some
                                                        }
                                                    , argument = "GitHub personal access token"
                                                    }
                                                }
                                            , Syntax.Definition
                                                { nameLocation = ()
                                                , name = "owner"
                                                , bindings = []
                                                , annotation = Nothing
                                                , assignment = Syntax.Text
                                                    { location = ()
                                                    , chunks = Syntax.Chunks owner []
                                                    }
                                                }
                                            , Syntax.Definition
                                                { nameLocation = ()
                                                , name = "repository"
                                                , bindings = []
                                                , annotation = Nothing
                                                , assignment = Syntax.Text
                                                    { location = ()
                                                    , chunks = Syntax.Chunks repository []
                                                    }
                                                }
                                            , Syntax.Definition
                                                { nameLocation = ()
                                                , name = "reference"
                                                , bindings = []
                                                , annotation = Nothing
                                                , assignment = reference
                                                }
                                            , Syntax.Definition
                                                { nameLocation = ()
                                                , name = "path"
                                                , bindings = []
                                                , annotation = Nothing
                                                , assignment = Syntax.Text
                                                    { location = ()
                                                    , chunks = Syntax.Chunks (Text.intercalate "/" path) []
                                                    }
                                                }
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
                                        [ Syntax.Definition
                                            { nameLocation = ()
                                            , name = "key"
                                            , bindings = []
                                            , annotation = Nothing
                                            , assignment = Syntax.Scalar
                                                { location = ()
                                                , scalar = Syntax.Null
                                                }
                                            }
                                        , Syntax.Definition
                                            { nameLocation = ()
                                            , name = "owner"
                                            , bindings = []
                                            , annotation = Nothing
                                            , assignment = Syntax.Text
                                                { location = ()
                                                , chunks = Syntax.Chunks owner []
                                                }
                                            }
                                        , Syntax.Definition
                                            { nameLocation = ()
                                            , name = "repository"
                                            , bindings = []
                                            , annotation = Nothing
                                            , assignment = Syntax.Text
                                                { location = ()
                                                , chunks = Syntax.Chunks repository []
                                                }
                                            }
                                        , Syntax.Definition
                                            { nameLocation = ()
                                            , name = "reference"
                                            , bindings = []
                                            , annotation = Nothing
                                            , assignment = reference
                                            }
                                        , Syntax.Definition
                                            { nameLocation = ()
                                            , name = "path"
                                            , bindings = []
                                            , annotation = Nothing
                                            , assignment = Syntax.Text
                                                { location = ()
                                                , chunks = Syntax.Chunks (Text.intercalate "/" path) []
                                                }
                                            }
                                        ]
                                    }
                                , schema = Nothing
                                }

                        setCodeValue codeInput (Pretty.toSmart (expression :: Syntax () Void))
            _ -> do
                return ()

    case maybeExpression of
        Just expression -> do
            setCodeValue codeInput (URI.Encode.decodeText expression)
        Nothing -> do
            return ()

    debouncedInterpret ()
