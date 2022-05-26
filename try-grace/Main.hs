{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE QuasiQuotes       #-}

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
import qualified Data.String.Interpolate as Interpolate
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
    getValue_ :: JSVal -> IO JSString

getValue :: MonadIO io => JSVal -> io Text
getValue a = liftIO (fmap toText (getValue_ a))

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
    before :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.remove()"
    remove :: JSVal -> IO ()

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
    input    <- getElementById "input"
    output   <- getElementById "output"
    error    <- getElementById "error"
    tutorial <- getElementById "tutorial"

    spinner <- createElement "div"

    setAttribute spinner "class" "spinner-border text-primary"
    setAttribute spinner "role"  "status"

    params <- getSearchParams

    exists <- hasParam params "expression"

    Monad.when exists do
        expression <- getParam params "expression"

        setTextContent input (URI.Encode.decodeText expression)

        replaceChild error spinner

    ref <- IORef.newIORef 0

    let setError text = do
            setTextContent error text

            setDisplay output "none"
            setDisplay error  "block"

    let setOutput type_ value = do
            renderValue ref output type_ value

            setDisplay error  "none"
            setDisplay output "block"

    let interpret = do
            text <- getValue input

            setParam params "expression" (URI.Encode.encodeText text)

            saveSearchParams params

            if  | Text.null text -> do
                    setError ""

                | otherwise -> do
                    setDisplay tutorial "none"

                    let input_ = Code "(input)" text

                    replaceChild error spinner

                    result <- Except.runExceptT (Interpret.interpret input_)

                    case result of
                        Left interpretError -> do
                            setError (Text.pack (displayException interpretError))
                        Right (type_, value) -> do
                            setOutput type_ value

    inputCallback <- Callback.asyncCallback interpret

    addEventListener input "input" inputCallback

    tutorialCallback <- Callback.asyncCallback do
        setTextContent input tutorialText

        setAttribute input "style" "width: 100%; max-width: 100%; height: 20em;"

        interpret

    addEventListener tutorial "click" tutorialCallback

    Monad.when exists do
        interpret

tutorialText :: Text
tutorialText = [Interpolate.i|\# This is an interactive tutorial for the Fall-from-Grace language (a.k.a.
\# "Grace" for short).  Feel free to make this text area larger so that you can
\# more easily read the code.
\#
\# This is not a complete tour of the Grace language, but rather just enough of a
\# tour to pique your interest.
\#
\# First off, any line prefixed with a "\#" character is a comment, like this one.
\#
\# Grace does not support special syntax for multi-line comments.  Instead, you
\# have to prefix each comment line with a single-line comment.
{ \# A Grace record begins with an opening brace (i.e. "{") and ends with a
  \# closing brace (i.e. "}").  This tutorial is implemented inside of one large
  \# Grace record.
  \#
  \# Records have zero or more fields and each field is associated with a value.
  \# For example, the first field in our record is named "Example Bool" and the
  \# associated value is `true`:
  "Example Bool": true

, \# Fields are separated by commas, such as the comma preceding this comment.
  \# Grace is not whitespace-sensitive, so commas can be present at the
  \# beginning or end of a line.  This tutorial uses the "leading comma"
  \# convention.
  \#
  \# Our next field is named "Example number", which is associated with the value
  \# `1`.  Note that each field of a record may store a different type of value.
  \# For example, the field named "Example Bool" stores a Bool value (e.g.
  \# true) whereas the field named "Example number stores a Natural number (e.g.
  \# 1):
  "Example number": 1

, \# Immediately after this code you'll see the HTML output of this code.  The
  \# Grace expression inside of this text area is converted to the equivalent
  \# web page.  For example, the field named "Example Bool" that stores the value
  \# true is rendered as a text label ("Example Bool") followed by the Bool
  \# value rendered as a checkbox (which is checked, because the value was true).
  \#
  \# This next field stores some Text, which is also rendered as Text when
  \# converted to HTML.  Try to modify the text to replace "{name}" with your
  \# name.  As you edit the text, watch how the HTML output updates in response.
  \#
  \# After doing, change the value of the "Example Bool" field above from true to
  \# false.  This will uncheck the matching checkbox below.
  "Example text": "My name is {name}"

, \# This web page is interactive in two different ways:
  \#
  \# * The HTML output interactively updates whenever you change the code in this
  \#   box
  \#
  \# * Any function you create is turned into an interactive web form
  \#
  \# We've already seen the first type of interactivity: when we edit this code
  \# the HTML output changes.  Now let's illustrate the latter type of
  \# interactivity by having the following field store a function.  This will
  \# create a web form underneath the "Example function" label with a text input
  \# and a text output.  Edit the text input underneath the "Example function"
  \# label to enter your name and watch the matching text output update as you do
  \# so.
  "Example function": \\name -> "My name is " + name

, \# This page generates inputs and outputs that intelligently match the
  \# inferred type of the code.  For example, we will annotate the following
  \# function with this type signature:
  \#
  \#     Bool -> List Bool
  \#
  \# … which means that our function has a single input (of type Bool) and
  \# a single output (a List of Bool)s.  The corresponding web form underneath
  \# the "Triplicate Bool" label will prompt the user with a single checkbox for
  \# input and the output will be a list of three checkboxes whose values all
  \# match the input:
  "Triplicate Bool": (\\x -> [ x, x, x ]) : Bool -> List Bool

, \# By default, if multiple types of values would work then this live demo
  \# will infer a type of JSON.  For example, if we omit the type annotation as
  \# in the following example then the interpreter will infer this more general
  \# type:
  \#
  \#     forall (a : Type) . a -> List a
  \#
  \# … and then for the purpose of generating a form it will replace the
  \# arbitrary type "a" with JSON, producing this more specific type:
  \#
  \#     JSON -> List JSON
  \#
  \# … so the following example will differ from the prior example by prompting
  \# the user for a JSON input and then producing three matching copies of that
  \# JSON as output.  Try replacing the "null" input underneat the "Triplicate
  \# JSON" label with "[ 1, true ]" and see what happens:
  "Triplicate JSON": \\x -> [ x, x, x ]

, \# In fact, Grace is a superset of JSON, meaning that any JSON expression is
  \# syntactically legal.  For some JSON expressions they will work without a
  \# type annotation if the interpreter can infer a sensible type for them, like
  \# this one:
  "Example JSON":
    {
      "clients": [
        {
          "isActive": true,
          "age": 36,
          "name": "Dunlap Hubbard",
          "email": "dunlaphubbard@cedward.com",
          "phone": "+1 (890) 543-2508"
        },
        {
          "isActive": true,
          "age": 24,
          "name": "Kirsten Sellers",
          "email": "kirstensellers@emergent.com",
          "phone": "+1 (831) 564-2190"
        }
      ]
    }

, \# However, even weirder JSON expressions will still type-check if you add an
  \# explicit JSON type annotation to them.  However, there's a tradeoff:
  \# expressions of type JSON are easier to produce, but harder to consume
  \# because the type-checker cannot provide as many guarantees about what they
  \# contain:
  "Weird example JSON":
    [ [ 1, true ], { x: "ABC" } ] : JSON

  \# Grace supports importing arbitrary subexpressions by URL.  For example,
  \# we can import a US federal tax income calculator for 2022 by uncommenting
  \# following "Tax calculator" field containing a URL for that calculator.  The
  \# field is commented out by default to keep the demo snappier when making code
  \# changes and you will want to comment this out again when you're done with
  \# this example:
  \# , "Tax calculator": https://gist.githubusercontent.com/Gabriella439/712d0648bbdcfcc83eadd0ee394beed3/raw/1b03f661577521b4d3dc6ca73dd11475a30c1594/incomeTax.ffg

, \# Now let's do a quick tour of all the simple (scalar) types that we can
  \# convert to HTML outputs:
  "Sample scalar values":
    { "Sample Bool values": [ false, true ]
    , "Sample Natural numbers": [ 0, 1, 2 ]
    , "Sample Integers": [ -2, -1, 0, 1, 2 ]
    , "Sample Real numbers": [ -2.1, 3.14159265, 2.718281828459 ]
    , "Sample Text strings": [ "ABC", "Hello, world!" ]
    , "Sample JSON values": [ 1, true, [ "ABC", null ] ] : List JSON
    }

, \# We can also create more complex data structures, like:
  "Sample complex data structures":
    { "Sample record": { "Name": "John Doe", "Age": 24 }
    , "Sample list": [ 1, 2, 3, 4, 5 ]
    , "Sample optional values": [ null, 1 ]
    }

, \# Note that all values in a list have to have the same type.  For example,
  \# try replacing the number 2 in the following list with a true and you'll
  \# get a type error.  Then change the element back to 2 when you're done:
  "List of numbers": [ 1, 2 ]

, \# There are two common ways to store values of different types in the same
  \# list.  The first (and most general way) is to wrap each element in a
  \# union alternative, which is any identifier beginning with a capital letter,
  \# like this:
  "List of union alternatives": [ Left 1, Right true ]

, \# The second (and more limited way) is that if all the values are valid
  \# JSON, then you can add a type annotation declaring the list elements to all
  \# be JSON and then the type-checker will accept that:
  "List of JSON": [ 1, true ] : List JSON

, \# You can also nest complex types arbitrarily, like this:
  "Sample nested data structures":
    { "List of lists": [ [ 1, 2 ], [ 3, 4, 5 ] ]
    , "Record of records":
        { "Position": { "x": 1.4, "y": -2.1 }
        , "Status": { "Health": 100, "Turn": 4 }
        }
    , "List of records":
        [ { "Name": "John Doe", "Grade": 93 }
        , { "Name": "Mary Jane", "Grade": 95 }
        ]
    , "List of unions of records":
        [ GitHub
            { "Repository":
                "https://github.com/Gabriel439/Haskell-Turtle-Library.git"
            , "Revision": "ae5edf227b515b34c1cb6c89d9c58ea0eece12d5"
            }
        , Local { "Relative path": "~/proj/optparse-applicative" }
        , Local { "Relative path": "~/proj/discrimination" }
        , Hackage { "Package": "lens", "Version": "4.15.4" }
        , GitHub
            { "Repository": "https://github.com/haskell/text.git"
            , "Revision": "ccbfabedea1cf5b38ff19f37549feaf01225e537"
            }
        , Local { "Relative path": "~/proj/servant-swagger" }
        , Hackage { "Package": "aeson", "Version": "1.2.3.0" }
        ]
    }

, \# All of these types work as function inputs, too.
  \#
  \# In Grace, you write a user-defined function using this syntax:
  \#
  \#     \\input -> output
  \#
  \# … where "input" is the name of the function input and "output" is the
  \# "body" of the function (i.e. the function's result), which can refer to the
  \# function's input.
  \#
  \# For example, the following function has this inferred type:
  \#
  \#    { "x": Bool, "y": Bool } -> { "x || y": Bool }
  \#
  \# … meaning that the function takes a record with two fields as input and
  \# produces a record with one field as output.  The generated form will
  \# match the inferred type.  In particular, the input section will have two
  \# fields labeled "x" and "y" (since our input record has two fields named
  \# "x" and "y"), and each field will be associated with a checkbox (since our
  \# input record fields both had type Bool).
  "Boolean OR": \\record -> { "x || y": record.x || record.y }

, \# Here is a quick tour of all of the supported input types, which we'll
  \# illustrate by creating a trivial function for each input type that returns
  \# the input as the output:
  "Sample inputs and outputs":
    { "Bool input and output": \\x -> x : Bool
    , "Numeric input and output": \\x -> x : Real
    , "Text input and output": \\x -> x : Text
    , "JSON input and output": \\x -> x : JSON
    , "Optional input and output": \\x -> x : Optional Bool
    , "List input and output": \\x -> x : List Bool
    , "Record input and output": \\x -> x : { a: Bool, b: Bool }
    , "Union input and output": \\x -> x : < Left : Bool | Right: Natural >
    }

, \# Not everything can be sensibly converted to HTML and when that happens the
  \# result will just render the code.  For example, the following function will
  \# just render as code:
  "Function that renders as plain code": \\f x -> f x

, \# However, the interpreter will still try to intelligently do something when
  \# possible.  For example, try entering ", " in the text input underneath the
  \# "Partially interpreted function" example and watch what happens to the
  \# rendered code:
  "Partially interpreted function":
    \\separator function -> (function "ABC" + separator + function "DEF") : Text

, \# Grace supports let expressions, which mean that you can define intermediate
  \# values anywhere within Grace code; even in the middle of a record like this
  \# one.  For example:
  "Example let expression":
    let x = 2

    in  x + x

, \# Intermediate expressions can be plain data (such as the "x" in the previous
  \# example), or functions, like this:
  "Examplet let-defined function":
    let twice = \\x -> [ x, x ]
    in  twice 2

, \# Grace also has the following built-in functions, many of which you can
  \# directly test below using the generated form for that function:
  "Built-in functions":
    { "Real/equal": Real/equal
    , "Real/lessThan": Real/lessThan
    , "Real/negate": Real/negate
    , "Real/show": Real/show
    , "List/drop": List/drop
    , "List/equal": List/equal
    , "List/fold": List/fold
    , "List/equal": List/equal
    , "List/head": List/head
    , "List/indexed": List/indexed
    , "List/last": List/last
    , "List/length": List/length
    , "List/map": List/map
    , "List/reverse": List/reverse
    , "List/take": List/take
    , "Integer/even": Integer/even
    , "Integer/negate": Integer/negate
    , "Integer/odd": Integer/odd
    , "Integer/abs": Integer/abs
    , "JSON/fold": JSON/fold
    , "Natural/fold": Natural/fold
    , "Text/equal": Text/equal
    }

  \# Grace also has a Prelude of utility functions which are derived from
  \# built-in functions.  You can test-drive the Prelude by commenting out the
  \# following field:
  \# , "Grace Prelude": https://raw.githubusercontent.com/Gabriella439/grace/main/prelude/package.ffg

  \# That's the end of this tutorial.  Have fun and if you want to learn more
  \# about Grace, visit the project on GitHub here:
  \#
  \# https://github.com/Gabriella439/grace\#grace
}|]
