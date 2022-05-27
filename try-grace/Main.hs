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
import qualified Grace.Interpret as Interpret
import qualified Grace.Monotype as Monotype
import qualified Grace.Normalize as Normalize
import qualified Grace.Pretty as Pretty
import qualified Grace.Type as Type
import qualified Grace.Value as Value
import qualified JavaScript.Array as Array
import qualified Network.URI.Encode as URI.Encode

foreign import javascript unsafe "document.getElementById($1)"
    getElementById_ :: JSString -> IO JSVal

getElementById :: Text -> IO JSVal
getElementById a = getElementById_ (fromText a)

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

foreign import javascript unsafe "$1.style.display = $2"
    setDisplay_ :: JSVal -> JSString -> IO ()

setDisplay :: MonadIO io => JSVal -> Text -> io ()
setDisplay a b = liftIO (setDisplay_ a (fromText b))

foreign import javascript unsafe "$1.addEventListener($2, $3)"
    addEventListener_ :: JSVal -> JSString -> Callback (IO ()) -> IO ()

addEventListener :: MonadIO io => JSVal -> Text -> Callback (IO ()) -> io ()
addEventListener a b c = liftIO (addEventListener_ a (fromText b) c)

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

foreign import javascript unsafe "history.replaceState(null, null, '?'+$1.toString())"
  saveSearchParams_ :: JSVal -> IO ()

saveSearchParams :: MonadIO io => JSVal -> io ()
saveSearchParams a = liftIO (saveSearchParams_ a)

-- @$1.replaceChildren(...$2)@ does not work because GHCJS fails to parse the
-- spread operator, we work around this by defining the
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

foreign import javascript unsafe "CodeMirror.fromTextArea($1, { lineNumbers: true, mode: 'python' })"
    setupCodemirror_ :: JSVal -> IO JSVal

setupCodemirror :: MonadIO io => JSVal -> io JSVal
setupCodemirror a = liftIO (setupCodemirror_ a)

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

toText :: JSString -> Text
toText = Text.pack . JSString.unpack

fromText :: Text -> JSString
fromText = JSString.pack . Text.unpack

valueToText :: Value -> Text
valueToText = Pretty.renderStrict False 80 . Normalize.quote []

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

    setTextContent var (valueToText value)

    replaceChild parent var

renderValue _ parent _ (Value.Scalar (Text text))= do
    span <- createElement "span"

    setTextContent span text

    replaceChild parent span

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

    setTextContent span (valueToText value)

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

            setAttribute dt "class" "col-auto"

            case value of
                Value.Record kvs | HashMap.null kvs -> do
                    mempty
                Value.List xs | Seq.null xs -> do
                    mempty
                _ -> do
                    setAttribute dt "style" "border-right: solid;"

            setTextContent dt key

            dd <- createElement "dd"

            setAttribute dd "class" "col"

            renderValue ref dd type_ value

            dl <- createElement "dl"

            setAttribute dl "class" "row"

            replaceChildren dl (Array.fromList [ dt, dd ])

            return dl

    dls <- HashMap.traverseWithKey process keyValues

    replaceChildren parent (Array.fromList (HashMap.elems dls))

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
            hr <- createElement "hr"

            outputVal <- createElement "div"

            let invoke = do
                    value <- get

                    renderValue ref outputVal output (Normalize.apply function value)

            callback <- Callback.asyncCallback invoke

            observer <- newObserver callback

            observe observer inputVal

            addEventListener inputVal "input" callback

            invoke

            replaceChildren parent (Array.fromList [ inputVal, hr, outputVal ])

renderValue _ parent _ value = do
    renderDefault parent value

renderDefault :: JSVal -> Value -> IO ()
renderDefault parent value = do
    code <- createElement "code"

    setTextContent code (valueToText value)

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
    setAttribute span "style" "display: inline-block !important;"

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
            double <- toDoubleValue input

            return (Value.Scalar (Real (Scientific.fromFloatDigits double)))

    return (input, get)

renderInput _ Type.Scalar{ scalar = Monotype.Integer } = do
    input <- createElement "input"

    setAttribute input "type"  "number"
    setAttribute input "value" "0"

    let get = do
            integer <- toIntegerValue input

            return (Value.Scalar (Integer integer))

    return (input, get)

renderInput _ Type.Scalar{ scalar = Monotype.Natural } = do
    input <- createElement "input"

    setAttribute input "type"  "number"
    setAttribute input "value" "0"
    setAttribute input "min"   "0"

    let get = do
            integer <- toIntegerValue input

            return (Value.Scalar (Natural (fromInteger integer)))

    return (input, get)

renderInput _ Type.Scalar{ scalar = Monotype.JSON } = do
    input <- createElement "input"

    setAttribute input "value" "null"

    let get = do
            strictText <- toValue input

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
            text <- toValue textarea

            return (Value.Scalar (Text text))

    return (textarea, get)

renderInput ref Type.Record{ fields = Type.Fields keyTypes _ } = do
    let process (key, type_) = do
            (fieldVal, get) <- renderInput ref type_

            dt <- createElement "dt"

            setAttribute dt "class" "col-auto"

            setTextContent dt key

            dd <- createElement "dd"

            setAttribute dd "class" "col"

            replaceChild dd fieldVal

            dl <- createElement "dl"

            setAttribute dl "class" "row"

            replaceChildren dl (Array.fromList [ dt, dd ])

            return (dl, key, get)

    triples <- traverse process keyTypes

    let children = do
            (dl, _, _) <- triples

            return dl

    div <- createElement "div"

    replaceChildren div (Array.fromList children)

    let get = do
            let getWithKey (_, key, getInner) = do
                    value <- getInner

                    return (key, value)

            keyValues <- traverse getWithKey triples

            return (Value.Record (HashMap.fromList keyValues))

    return (div, get)

renderInput ref Type.Union{ alternatives = Type.Alternatives keyTypes _ }
    | not (null keyTypes) = do
        n <- liftIO (IORef.atomicModifyIORef ref (\a -> (a + 1, a)))

        let process (checked, (key, type_)) = do
                (nestedVal, nestedGet) <- renderInput ref type_

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

                setTextContent label key

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

    add <- createElement "li"

    replaceChild add plus

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

        before add li

    addEventListener plus "click" insert

    ul <- createElement "ul"

    setAttribute ul "class" "list-unstyled"

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
    input         <- getElementById "input"
    output        <- getElementById "output"
    error         <- getElementById "error"
    startTutorial <- getElementById "start-tutorial"

    codeInput <- setupCodemirror input

    spinner <- createElement "div"

    setAttribute spinner "class" "spinner-border text-primary"
    setAttribute spinner "role"  "status"

    params <- getSearchParams

    exists <- hasParam params "expression"

    Monad.when exists do
        expression <- getParam params "expression"

        setValue codeInput (URI.Encode.decodeText expression)

        replaceChild error spinner

    ref <- IORef.newIORef 0

    tutorialRef <- IORef.newIORef False

    let setError text = do
            setTextContent error text

            setDisplay output "none"
            setDisplay error  "block"

    let setOutput type_ value = do
            renderValue ref output type_ value

            setDisplay error  "none"
            setDisplay output "block"

    let interpret = do
            text <- getValue codeInput

            setParam params "expression" (URI.Encode.encodeText text)

            saveSearchParams params

            if  | Text.null text -> do
                    tutorial <- IORef.readIORef tutorialRef

                    Monad.unless tutorial do
                        setDisplay startTutorial "inline-block"

                    setError ""

                | otherwise -> do
                    setDisplay startTutorial "none"

                    let input_ = Code "(input)" text

                    replaceChild error spinner

                    setDisplay output "none"
                    setDisplay error  "block"

                    result <- Except.runExceptT (Interpret.interpret input_)

                    case result of
                        Left interpretError -> do
                            setError (Text.pack (displayException interpretError))
                        Right (type_, value) -> do
                            setOutput type_ value

    inputCallback <- Callback.asyncCallback interpret

    onChange codeInput inputCallback

    startTutorialCallback <- Callback.asyncCallback do
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

                setAttribute a "class"
                    (if active then "nav-link active" else "nav-link")

                setTextContent a name

                li <- createElement "li"

                setAttribute li "class" "nav-item"

                replaceChild li a

                callback <- (liftIO . Callback.asyncCallback) do
                    setValue codeInput code

                    elements <- getElementsByClassName "nav-link"

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

            function <- createExample False "Functions" functionExample

            import_ <- createExample False "Imports" importExample

            json_ <- createExample False "JSON" jsonExample

            programming <- createExample False "Programming" programmingExample

            polymorphism <- createExample False "Polymorphism" polymorphismExample

            builtins <- createExample False "Builtins" builtinsExample

            prelude <- createExample False "Prelude" preludeExample

            setAttribute ul "class" "nav nav-tabs"

            replaceChildren ul
                (Array.fromList
                    [ helloWorld
                    , checkboxes
                    , function
                    , import_
                    , json_
                    , programming
                    , polymorphism
                    , builtins
                    , prelude
                    ]
                )

        before input ul

        stopTutorialCallback <- Callback.asyncCallback do
            setDisplay stopTutorial  "none"

            IORef.writeIORef tutorialRef False

            interpret

            remove stopTutorial

            remove ul

        addEventListener stopTutorial "click" stopTutorialCallback

        after startTutorial stopTutorial

        IORef.writeIORef tutorialRef True

        setDisplay startTutorial "none"

    addEventListener startTutorial "click" startTutorialCallback

    Monad.when exists do
        interpret

helloWorldExample :: Text
helloWorldExample =
    "# This is a brief tour of the Fall-from-Grace language (a.k.a.\n\
    \# \"Grace\" for short).\n\
    \#\n\
    \# First, any line prefixed with a \"#\" character is a comment, like\n\
    \# this one.\n\
    \#\n\
    \# Second, any change you make to this editable code area will show up\n\
    \# below.  Try editing the string \"Hello, world!\" below to replace\n\
    \# \"world\" with your name.\n\
    \\n\
    \\"Hello, world!\""

checkboxesExample :: Text
checkboxesExample =
    "# This Grace browser attempts to faithfully render any Grace expression\n\
    \# as an equivalent HTML representation.  For example, a list of boolean\n\
    \# values such as these will render as an HTML list of checkboxes.\n\
    \\n\
    \# Try adding another false value to the list.\n\
    \\n\
    \[ true, false, true ]"

functionExample :: Text
functionExample =
    "# This Grace browser really attempts to faithfully render ANY Grace\n\
    \# expression, including functions.  For example, the following function\n\
    \# takes an integer as input and returns the next integer as the result,\n\
    \# so this demo renders that as a web form with a numeric input and a\n\
    \# numeric output\n\
    \#\n\
    \# Do not edit the code this time.  Instead, enter a number into the\n\
    \# input field and watch the output update in response.\n\
    \\n\
    \\\x -> x + 1"

importExample :: Text
importExample =
    "# You can reference other Grace expressions by their URL.  For example,\n\
    \# the following URL encodes a function for computing US federal income\n\
    \# tax for 2022.\n\
    \https://gist.githubusercontent.com/Gabriella439/712d0648bbdcfcc83eadd0ee394beed3/raw/1b03f661577521b4d3dc6ca73dd11475a30c1594/incomeTax.ffg"

jsonExample :: Text
jsonExample =
    "# Grace is a superset of JSON, so the Grace browser is also a JSON\n\
    \# browser.\n\
    \\n\
    \{ # Strongly-typed JSON requires no type annotation\n\
    \  \"Sensible JSON\": [ { \"Name\": \"John Doe\" , \"Grade\": 95 }\n\
    \                   , { \"Name\": \"Mary Jane\", \"Grade\": 98 }\n\
    \                   ]\n\
    \  # Weakly-typed JSON requires a type annotation\n\
    \, \"Weird JSON\": [ 1, true, { } ] : JSON\n\
    \, \"GitHub API\": https://api.github.com\n\
    \, \"Reddit API\": https://reddit.com/r/haskell.json : JSON\n\
    \}"

programmingExample :: Text
programmingExample =
    "# You can use let expressions to defined reusable values or functions:\n\
    \\n\
    \let makeUser = \\user ->\n\
    \      let home       = \"/home/\" + user\n\
    \      let privateKey = home + \"/.ssh/id_ed25519\"\n\
    \      let publicKey  = privateKey + \".pub\"\n\
    \      in  { home: home\n\
    \          , privateKey: privateKey\n\
    \          , publicKey: publicKey\n\
    \          }\n\
    \in  [ makeUser \"bill\"\n\
    \    , makeUser \"jane\"\n\
    \    ]"

polymorphismExample :: Text
polymorphismExample =
    "# Grace permits polymorphic functions, like this one:\n\
    \let twice : forall (a : Type) . a -> List a\n\
    \          = \\x -> [ x, x ]\n\
    \\n\
    \in  { \"Nested lists\": twice (twice (twice (twice 2)))\n\
    \\n\
    \      # If you try to render a polymorphic function, it will render as a\n\
    \      # function that accepts JSON input:\n\
    \    , \"Duplicate JSON input\": twice\n\
    \    }"

builtinsExample :: Text
builtinsExample =
    "# Grace has a limited number of built-in functions, and you can\n\
    \# test-drive them below:\n\
    \\n\
    \{ \"Real/equal\": Real/equal\n\
    \, \"Real/lessThan\": Real/lessThan\n\
    \, \"Real/negate\": Real/negate\n\
    \, \"Real/show\": Real/show\n\
    \, \"List/drop\": List/drop\n\
    \, \"List/equal\": List/equal\n\
    \, \"List/fold\": List/fold\n\
    \, \"List/equal\": List/equal\n\
    \, \"List/head\": List/head\n\
    \, \"List/indexed\": List/indexed\n\
    \, \"List/last\": List/last\n\
    \, \"List/length\": List/length\n\
    \, \"List/map\": List/map\n\
    \, \"List/reverse\": List/reverse\n\
    \, \"List/take\": List/take\n\
    \, \"Integer/even\": Integer/even\n\
    \, \"Integer/negate\": Integer/negate\n\
    \, \"Integer/odd\": Integer/odd\n\
    \, \"Integer/abs\": Integer/abs\n\
    \, \"JSON/fold\": JSON/fold\n\
    \, \"Natural/fold\": Natural/fold\n\
    \, \"Text/equal\": Text/equal\n\
    \}"

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
    \let negative = prelude.real.negative\n\
    \\n\
    \in  { \"not\": not\n\
    \    , \"negative\": negative\n\
    \    , \"The entire Prelude\": prelude\n\
    \    }"
