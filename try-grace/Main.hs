{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Control.Applicative (empty)
import Control.Concurrent.Async (Async)
import Control.Exception (Exception(..), SomeException)
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

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Exception as Exception
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

foreign import javascript unsafe "$1.textContent= $2"
    setTextContent_ :: JSVal -> JSString -> IO ()

setTextContent :: MonadIO io => JSVal -> Text -> io ()
setTextContent a b = liftIO (setTextContent_ a (fromText b))

foreign import javascript unsafe "$1.innerText= $2"
    setInnerText_ :: JSVal -> JSString -> IO ()

setInnerText :: MonadIO io => JSVal -> Text -> io ()
setInnerText a b = liftIO (setInnerText_ a (fromText b))

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
setParam a b c =
    liftIO (setParam_ a (fromText b) (fromText c))

-- @$1.delete($2)@ doesn't work because GHCJS treats delete as a forbidden
-- reserved keyword, so we work around this by defining the
-- @deleteSearchParamWorkaround@ function in JavaScript which takes care of this
-- for us
foreign import javascript unsafe "deleteSearchParamWorkaround($1, $2)"
    deleteParam_ :: JSVal -> JSString -> IO ()

deleteParam :: MonadIO io => JSVal -> Text -> io ()
deleteParam a b =
    liftIO (deleteParam_ a (fromText b))

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

foreign import javascript unsafe "select($1)"
    select_ :: JSString -> IO ()

select :: MonadIO io => Text -> io ()
select a = liftIO (select_ (fromText a))

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
    let json = Type.Scalar{ location = nameLocation, scalar = Monotype.Text }

    renderValue keyToMethods ref parent (Type.substituteType name 0 json type_) value

renderValue keyToMethods ref parent Type.Forall{ name, domain = Fields, type_ } value = do
    let empty_ = Type.Fields [] EmptyFields

    renderValue keyToMethods ref parent (Type.substituteFields name 0 empty_ type_) value

renderValue keyToMethods ref parent Type.Forall{ name, domain = Alternatives, type_ } value = do
    let empty_ = Type.Alternatives [] EmptyAlternatives

    renderValue keyToMethods ref parent (Type.substituteAlternatives name 0 empty_ type_) value

renderValue keyToMethods ref parent Type.Optional{ type_ } value =
    renderValue keyToMethods ref parent type_ value

renderValue _ _ parent _ (Value.Text text) = do
    span <- createElement "span"

    setAttribute span "class" "fira"
    setAttribute span "style" "whitespace: pre"

    setInnerText span text

    replaceChild parent span

    mempty

renderValue _ _ parent _ (Value.Scalar (Bool bool)) = do
    input <- createElement "input"

    setAttribute input "type"     "checkbox"
    setAttribute input "class"    "form-check-input"
    setAttribute input "disabled" ""

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
    result <- Maybe.runMaybeT (renderInput keyToMethods ref input)

    case result of
        Nothing -> do
            renderDefault parent function
        Just (inputVal, get, refreshInput) -> do
            hr <- createElement "hr"

            outputVal <- createElement "div"

            (setBusy, setSuccess, setError) <- createForm False outputVal

            let render value = do
                    setBusy

                    result <- (liftIO . Exception.try) do
                        newValue <- Normalize.apply keyToMethods function value

                        refreshInput <- setSuccess output newValue \htmlWrapper -> do
                            renderValue keyToMethods ref htmlWrapper output newValue

                        refreshInput

                    case result of
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

                span <- createElement "span"

                setTextContent span " "

                div <- createElement "div"

                setAttribute div "class" "form-check"

                replaceChildren div (Array.fromList [ input, label, span, nestedVal ])

                let get = do
                        value <- nestedGet

                        return (Application (Alternative key) value)

                return (div, getChecked input, get, refreshInner)

        quartets <- traverse process (zip (True : repeat False) keyTypes)

        div <- createElement "div"

        let children = do
                (node, _, _, _) <- quartets

                return node

        replaceChildren div (Array.fromList children)

        let loop [] = do
                empty
            loop ((_, checkEnabled, getNested, _) : rest) = do
                enabled <- checkEnabled
                if  | enabled -> do
                        getNested
                    | otherwise -> do
                        loop rest

        let get = loop quartets

        let refreshInput = sequence_ do
                (_, _, _, refreshInner) <- quartets

                return refreshInner

        return (div, get, refreshInput)

renderInput keyToMethods ref Type.Optional{ type_ } = do
    (nestedVal, getInner, refreshInner) <- renderInput keyToMethods ref type_

    input <- createElement "input"

    setAttribute input "type"  "checkbox"
    setAttribute input "class" "form-check-input"

    span <- createElement "span"

    setTextContent span " "

    div <- createElement "div"

    replaceChildren div (Array.fromList [input, span, nestedVal])

    let get = do
            bool <- getChecked input

            if  | bool  -> do
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

    success <- createElement "success"

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

            let createExample active name code = do
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
                helloWorld <- createExample True "Hello, world!"
                    helloWorldExample

                checkboxes <- createExample False "HTML" checkboxesExample

                data_ <- createExample False "Data" dataExample

                prompting <- createExample False "Prompting" promptingExample

                variables <- createExample False "Variables" variablesExample

                functions <- createExample False "Functions" functionsExample

                import_ <- createExample False "Imports" importExample


                lists <- createExample False "Lists" listsExample

                coding <- createExample False "Coding" codingExample

                prelude <- createExample False "Prelude" preludeExample

                setAttribute ul "class" "nav nav-tabs"

                replaceChildren ul
                    (Array.fromList
                        [ helloWorld
                        , checkboxes
                        , data_
                        , prompting
                        , variables
                        , functions
                        , import_
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

helloWorldExample :: Text
helloWorldExample =
    "# This is a tour of the Fall-from-Grace language (a.k.a. \"Grace\" for short).\n\
    \#\n\
    \# First, any line prefixed with a \"#\" character is a comment, like this one.\n\
    \#\n\
    \# Second, any change you make to this editable code area will show up below.\n\
    \# Try editing the string \"Hello, world!\" below to replace \"world\" with your\n\
    \# name.\n\
    \#\n\
    \# Once you are done, click on the \"HTML\" tab above to proceed to the next\n\
    \# example.\n\
    \\n\
    \\"Hello, world!\""

checkboxesExample :: Text
checkboxesExample =
    "# This Grace browser attempts to faithfully render any Grace expression\n\
    \# as an equivalent HTML representation.  For example, a list of boolean\n\
    \# values such as these will render as an HTML list of checkboxes:\n\
    \\n\
    \[ true, false, true ]\n\
    \\n\
    \# Try adding another false value to the above list."

dataExample :: Text
dataExample =
    "# A record will render as a definition list when converted to HTML\n\
    \\n\
    \{ \"An example string\": \"Mercury\"\n\
    \, \"An example string with a type annotation\": \"cosmic\" : Text\n\
    \, \"A boolean value\": true\n\
    \, \"Annotated boolean value\": false : Bool\n\
    \, \"A natural number\": 42\n\
    \, \"An integer\": -12\n\
    \, \"A real number\": 3.14159265359\n\
    \, \"A list of natural numbers\": [ 1, 1, 2, 3, 5, 8, 13 ]\n\
    \, \"Annotated list of natural numbers\": [ 1, 1, 2, 3, 5, 8, 13 ] : List Natural\n\
    \, \"Annotated record\": { x: 0, y: 0 } : { x: Natural, y: Natural }\n\
    \, \"A list of records (using JSON syntax with quoted field names)\":\n\
    \    [ { \"isActive\": true\n\
    \      , \"age\": 36\n\
    \      , \"name\": \"Dunlap Hubbard\"\n\
    \      , \"email\": \"dunlaphubbard@example.com\"\n\
    \      , \"phone\": \"+1 (555) 543-2508\"\n\
    \      }\n\
    \    , { \"isActive\": true\n\
    \      , \"age\": 24\n\
    \      , \"name\": \"Kirsten Sellers\"\n\
    \      , \"email\": \"kirstensellers@example.com\"\n\
    \      , \"phone\": \"+1 (555) 564-2190\"\n\
    \      }\n\
    \    ]\n\
    \}\n\
    \\n\
    \# What type do you think the last field has?  Switch to the \"Type\" tab below\n\
    \# to check your guess, then switch back to the \"Form\" tab before proceeding to\n\
    \# the next example."

promptingExample :: Text
promptingExample =
    "# Grace provides built-in language support for LLMs using the `prompt` function.\n\
    \# To run these examples you will need to provide an OpenAI API key below and.\n\
    \# and then click \"Submit\".\n\
    \\\arguments ->\n\
    \\n\
    \let key = arguments.\"OpenAI key\" in\n\
    \\n\
    \{ # You can prompt a model with `Text`, which will (by default) return `Text`:\n\
    \  names: prompt{ key, text: \"Give me a list of names\" }\n\
    \\n\
    \, # You can request structured output with a type annotation, like this:\n\
    \  structuredNames: prompt{ key, text: \"Give me a list of names\" } : List Text\n\
    \\n\
    \, # If you request a record with first and last name fields then the model will\n\
    \  # adjust its output to match:\n\
    \  fullNames:\n\
    \    prompt{ key, text: \"Give me a list of names\" }\n\
    \      : List { firstName: Text, lastName: Text }\n\
    \\n\
    \, # In fact, that type is descriptive enough that we can just omit the prompt:\n\
    \  tacitFullNames: prompt{ key } : List { firstName: Text, lastName: Text }\n\
    \\n\
    \, # By default the `prompt` keyword selects the `o4-mini` model, but you can\n\
    \  # specify other models using the `model` argument:\n\
    \  differentModel:\n\
    \    prompt{ key, model: \"gpt-4o\" } : List { firstName: Text, lastName: Text }\n\
    \}\n\
    \\n\
    \# Try switching to the \"Code\" tab below to view the code for the result, then\n\
    \# switch back to the \"Form\" tab and continue to the next example."

variablesExample :: Text
variablesExample =
    "# You can define a variable using `let`:\n\
    \let john = { name: \"John Doe\", age: 24 }\n\
    \\n\
    \# Variables can reference earlier variables:\n\
    \let twentyFour = john.age\n\
    \\n\
    \# You can nest `let` expressions:\n\
    \let nine = let three = 3\n\
    \           in  three * three\n\
    \\n\
    \in  nine * twentyFour\n\
    \# Grace is whitespace-insensitive (with the exception of comments, which extend\n\
    \# to the next newline character), so try deleting all of the above comments and\n\
    \# modifying the above code to fit on one line."

functionsExample :: Text
functionsExample =
    "# You can also define functions using `let` expressions:\n\
    \let greet{ name } = \"Hello, ${name}!\"\n\
    \\n\
    \let greeting = greet{ name: \"world\" }\n\
    \\n\
    \# You can add optional type annotations to a function's arguments and output:\n\
    \let greet{ name: Text } : Text = \"Hello, ${name}!\"\n\
    \# The type of the `greet` function is `{ name: Text } -> Text` which you can\n\
    \# read as \"a function whose input is a record (with a `name` field) and whose\n\
    \# output is `Text`\n\
    \\n\
    \# Function definitions can define intermediate variables:\n\
    \let makeUser{ user } =\n\
    \        let home = \"/home/${user}\"\n\
    \        let privateKey = \"${home}/.ssh/id_ed25519\"\n\
    \        let publicKey = \"${privateKey}.pub\"\n\
    \        in  { home, privateKey, publicKey }\n\
    \# What do you think the type of the `makeUser` function is?  Check the \"Type\"\n\
    \# tab below to check your guess.\n\
    \\n\
    \let users =\n\
    \        [ makeUser{ user: \"bill\" }\n\
    \        , makeUser{ user: \"jane\" }\n\
    \        ]\n\
    \\n\
    \# We include the functions we defined (i.e. `greet` and `makeUser`) in the\n\
    \# output because the Grace browser can render functions as interactive forms.\n\
    \# Switch back to the \"Form\" tab and try entering your name into the generated\n\
    \# interactive forms.\n\
    \in  { greet\n\
    \    , greeting\n\
    \    , makeUser\n\
    \    , users\n\
    \    }"

importExample :: Text
importExample =
    "# You can reference other Grace expressions by their URL.  For example,\n\
    \# the following URL encodes a function for computing US federal income\n\
    \# tax for 2022:\n\
    \https://gist.githubusercontent.com/Gabriella439/712d0648bbdcfcc83eadd0ee394beed3/raw/694198a2d114278c42e4981ed6af67b8e3229cea/incomeTax.ffg\n\
    \# You can use this feature to create reusable Grace code.\n\
    \#\n\
    \# Grace browser sessions are sharable, too!  If you copy your current tab's URL\n\
    \# and open that URL in a new tab you will get the same Grace browser session.\n\
    \# This means that all forms you create within the Grace browser are\n\
    \# automatically sharable, too."

listsExample :: Text
listsExample =
    "# Now let's cover the basic list operations.\n\
    \\n\
    \let somePrimes = [ 2, 3, 5, 7, 11 ]\n\
    \\n\
    \# You can access list elements using dot notation.  `x.n` returns the\n\
    \# (0-indexed) nth element of the list:\n\
    \let two = somePrimes.0\n\
    \let three = somePrimes.1\n\
    \\n\
    \# Just like Python, negative numbers index from the end of the list:\n\
    \let eleven = somePrimes.-1\n\
    \let seven = somePrimes.-2\n\
    \\n\
    \# Use `list[i:j]` to return a slice from the list.\n\
    \#\n\
    \# The lower bound (i) is inclusive and the upperbound (j) is not inclusive:\n\
    \let middleThreeElements = somePrimes[1:4]\n\
    \\n\
    \# You can omit the lower bound to begin from the first element:\n\
    \let firstThreeElements = somePrimes[:3]\n\
    \\n\
    \# You can omit the upper bound to end on the last element:\n\
    \let lastThreeElements = somePrimes[-3:]\n\
    \\n\
    \in  { somePrimes\n\
    \    , two\n\
    \    , three\n\
    \    , eleven\n\
    \    , seven\n\
    \    , middleThreeElements\n\
    \    , firstThreeElements\n\
    \    , lastThreeElements\n\
    \    }"

codingExample :: Text
codingExample =
    "\\arguments ->\n\
    \\n\
    \let key = arguments.\"OpenAI key\" in\n\
    \\n\
    \# What do you think this code will do?  Run it to test your guess:\n\
    \prompt{ key, code: true }\n\
    \    : { jobDescription: Text } -> { isFinance : Bool, rationale : Text }\n\
    \# You can read the above type as \"a function whose input is a record (with a\n\
    \# `jobDescription` field) and whose output is a record (with `isFinance` and\n\
    \# `rationale` fields)\n\
    \\n\
    \# The `code: true` argument instructs the model to generate Grace code matching\n\
    \# the expected type.  The generated Grace might use the `prompt` keyword, too!\n\
    \# Be sure to check out the \"Code\" tab to see what code the model generated"

preludeExample :: Text
preludeExample =
    "# Grace also has a Prelude of utility functions derived from built-in\n\
    \# functions that you can also use.\n\
    \\n\
    \# You can import functions individually, like this:\n\
    \let not = https://raw.githubusercontent.com/Gabriella439/grace/main/prelude/bool/not.ffg\n\
    \\n\
    \# You can also import the Prelude as a whole, which is a nested record:\n\
    \let prelude = https://raw.githubusercontent.com/Gabriella439/grace/main/prelude/package.ffg\n\
    \\n\
    \# Then you can access functions as record fields:\n\
    \let clamp = prelude.integer.clamp\n\
    \\n\
    \in  { \"not\": not\n\
    \    , \"clamp\": clamp\n\
    \    , \"The entire Prelude\": prelude\n\
    \    }"
