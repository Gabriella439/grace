{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

-- | This module contains the logic for efficiently evaluating an expression
module Grace.Normalize
    ( -- * Normalization
      evaluate
    , quote
    ) where

import Control.Applicative (empty)
import Control.Exception (Exception)
import Data.Foldable (toList)
import Data.Functor (void)
import Data.Scientific (Scientific)
import Data.Sequence (Seq(..), ViewL(..))
import Data.Text (Text)
import Data.Void (Void)
import Grace.Location (Location)
import Grace.Syntax (Builtin(..), Scalar(..), Syntax)
import Grace.Type (Type)
import Grace.Value (Closure(..), Value)
import OpenAI.V1 (Methods(..))
import OpenAI.V1.Models (Model(..))
import OpenAI.V1.ResponseFormat(JSONSchema(..))
import Prelude hiding (succ)

import OpenAI.V1.Chat.Completions
    ( ChatCompletionObject(..)
    , CreateChatCompletion(..)
    , _CreateChatCompletion
    , ResponseFormat(..)
    )

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Data.Scientific as Scientific
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Grace.Monotype as Monotype
import qualified Grace.Pretty as Pretty
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type
import qualified Grace.Value as Value
import qualified OpenAI.V1.Chat.Completions as OpenAI
import qualified System.IO.Unsafe as Unsafe

{- $setup

   >>> :set -XOverloadedStrings
-}

{-| Lookup a variable from an ordered environment of name-value pairs using the
    variable's name and index
-}
lookupVariable
    :: Text
    -- ^ Variable name
    -> Int
    -- ^ Variable index
    -> [(Text, Value)]
    -- ^ Evaluation environment
    -> Value
lookupVariable name index environment =
    case environment of
        (key, value) : rest ->
            if name == key
            then if index == 0
                 then value
                 else lookupVariable name (index - 1) rest
            else lookupVariable name index rest
        [] ->
            -- In the `Value` type, free variables are stored using negative
            -- indices (starting at -1) to avoid collision with bound variables
            --
            -- >>> evaluate [] "x"
            -- Variable "x" (-1)
            --
            -- This has the nice property that `quote` does the right thing when
            -- converting back to the `Syntax` type.
            Value.Variable name (negate index - 1)

asInteger :: Scalar -> Maybe Integer
asInteger (Natural n) = Just (fromIntegral n)
asInteger (Integer n) = Just n
asInteger  _          = Nothing

asReal :: Scalar -> Maybe Scientific
asReal (Natural n) = Just (fromIntegral n)
asReal (Integer n) = Just (fromInteger  n)
asReal (Real    n) = Just n
asReal  _          = Nothing

toJSONSchema :: Type () -> Either UnsupportedModelOutput Aeson.Value
toJSONSchema Type.Forall{..} = toJSONSchema type_
toJSONSchema Type.Optional{..} = toJSONSchema type_
toJSONSchema Type.List{..} = do
    items <- toJSONSchema type_

    return (Aeson.object [ ("type", "array"), ("items", items) ])
toJSONSchema Type.Record{ fields = Type.Fields fieldTypes _ } = do
    let toProperty (field, type_) = do
            property <- toJSONSchema type_

            return (field, property)

    properties <- traverse toProperty fieldTypes

    return
        (Aeson.object
            [ ("type", "object")
            , ("properties", Aeson.toJSON (Map.fromList properties))
            , ("additionalProperties", Aeson.toJSON False)
            , ("required", Aeson.toJSON required)
            ]
        )
  where
    required = do
        (field, type_) <- fieldTypes

        case type_ of
            Type.Optional{ } -> empty
            _ -> return field
toJSONSchema Type.Union{ alternatives = Type.Alternatives alternativeTypes _ } = do
    let toAnyOf (alternative, type_) = do
            contents <- toJSONSchema type_

            return
                (Aeson.object
                    [ ("type", "object")
                    , ( "properties"
                      , Aeson.object
                          [ ( "tag"
                            , Aeson.object
                                [ ("type", "string")
                                , ("const", Aeson.toJSON alternative)
                                ]
                            )
                          , ("contents", contents)
                          ]
                      )
                    , ("required", Aeson.toJSON ([ "tag", "contents" ] :: [Text]))
                    , ("additionalProperties", Aeson.toJSON False)
                    ]
                )

    -- "oneOf" is not supported by OpenAI, so we use "anyOf" as the closest
    -- equivalent
    anyOfs <- traverse toAnyOf alternativeTypes

    return (Aeson.object [ ("type", "object"), ("anyOf", Aeson.toJSON anyOfs) ])
  where
toJSONSchema Type.Scalar{ scalar = Monotype.Bool } =
    return (Aeson.object [ ("type", "boolean") ])
toJSONSchema Type.Scalar{ scalar = Monotype.Real } =
    return (Aeson.object [ ("type", "number") ])
toJSONSchema Type.Scalar{ scalar = Monotype.Integer } =
    return (Aeson.object [ ("type", "integer") ])
toJSONSchema Type.Scalar{ scalar = Monotype.JSON } =
    return (Aeson.object [ ])
toJSONSchema Type.Scalar{ scalar = Monotype.Natural } =
    return
        (Aeson.object
            [ ("type", "number")
            -- , ("minimum", Aeson.toJSON (0 :: Int))
            -- ^ Not supported by OpenAI
            ]
        )
toJSONSchema Type.Scalar{ scalar = Monotype.Text } =
    return (Aeson.object [ ("type", "string") ])
toJSONSchema type_ = Left UnsupportedModelOutput{..}

fromJSON :: Aeson.Value -> Value
fromJSON (Aeson.Object [("contents", contents), ("tag", Aeson.String tag)]) =
    Value.Application (Value.Alternative tag) (fromJSON contents)
fromJSON (Aeson.Object object) = Value.Record (HashMap.fromList textValues)
  where
    textValues = do
        (key, value) <- KeyMap.toList object
        return (Key.toText key, fromJSON value)
fromJSON (Aeson.Array vector) =
    Value.List (Seq.fromList (toList elements))
  where
    elements = fmap fromJSON vector
fromJSON (Aeson.String text) = Value.Scalar (Text text)
fromJSON (Aeson.Number scientific) =
    case Scientific.floatingOrInteger scientific of
        Left (_ :: Double) -> Value.Scalar (Real scientific)
        Right (integer :: Integer)
            | 0 <= integer -> Value.Scalar (Natural (fromInteger integer))
            | otherwise    -> Value.Scalar (Integer integer)
fromJSON (Aeson.Bool bool) =
    Value.Scalar (Bool bool)
fromJSON Aeson.Null =
    Value.Scalar Null

{-| Evaluate an expression, leaving behind a `Value` free of reducible
    sub-expressions

    This function uses separate types for the input (i.e. `Syntax`) and the
    output (i.e. `Value`) in order to avoid wastefully evaluating the same
    sub-expression multiple times.
-}
evaluate
    :: Maybe Methods
    -- ^ OpenAI methods
    -> [(Text, Value)]
    -- ^ Evaluation environment (starting at @[]@ for a top-level expression)
    -> Syntax Location (Type Location, Value)
    -- ^ Surface syntax
    -> IO Value
    -- ^ Result, free of reducible sub-expressions
evaluate maybeMethods = loop
  where
    loop
      :: [(Text, Value)] -> Syntax Location (Type Location, Value) -> IO Value
    loop env syntax =
        case syntax of
            Syntax.Variable{..} ->
                return (lookupVariable name index env)

            Syntax.Application{..} -> do
                function' <- loop env function
                argument' <- loop env argument

                apply function' argument'

            Syntax.Lambda{ nameBinding = Syntax.NameBinding{ name }, ..} ->
                return (Value.Lambda (Closure name env body))

            Syntax.Annotation{..} ->
                loop env annotated

            Syntax.Let{ body = body₀, ..} -> do
                newEnv <- Monad.foldM snoc env bindings

                loop newEnv body₀
              where
                snoc environment Syntax.Binding{ nameLocation, name, nameBindings, assignment } = do
                    let cons nameBinding body =
                            Syntax.Lambda{ location = nameLocation, ..}

                    let newAssignment = foldr cons assignment nameBindings

                    value <- loop environment newAssignment

                    return ((name, value) : environment)

            Syntax.List{..} -> do
                values <- traverse (loop env) elements
                return (Value.List values)

            Syntax.Record{..} -> do
                let process (key, field) = do
                        newField <- loop env field
                        return (key, newField)

                newFieldValues <- traverse process fieldValues

                return (Value.Record (HashMap.fromList newFieldValues))

            Syntax.Field{..} -> do
                value <- loop env record

                case value of
                    Value.Record fieldValues ->
                        case HashMap.lookup field fieldValues of
                            Just v -> return v
                            Nothing -> return (Value.Scalar Syntax.Null)
                    other ->
                        return (Value.Field other field)

            Syntax.Alternative{..} ->
                return (Value.Alternative name)

            Syntax.Merge{..} -> do
                newHandlers <- loop env handlers

                return (Value.Merge newHandlers)

            Syntax.If{..} -> do
                predicate' <- loop env predicate

                ifTrue'  <- loop env ifTrue
                ifFalse' <- loop env ifFalse

                case predicate' of
                    Value.Scalar (Bool True) -> return ifTrue'
                    Value.Scalar (Bool False) -> return ifFalse'
                    _ -> return (Value.If predicate' ifTrue' ifFalse')

            Syntax.Prompt{ schema = Nothing } -> do
                Exception.throwIO MissingSchema
            Syntax.Prompt{ schema = Just schema, ..} -> do
                value <- loop env arguments

                let extractResponse (Value.Record [("response", response)]) = do
                        return response
                    extractResponse other = do
                        return other

                let (recordSchema, extract) = case void schema of
                        t@Type.Record{ } -> (t, return)
                        t -> (Type.Record () (Type.Fields [("response", t)] Monotype.EmptyFields), extractResponse)

                jsonSchema <- case toJSONSchema recordSchema of
                    Left exception -> Exception.throwIO exception
                    Right jsonSchema -> return jsonSchema

                case value of
                    Value.Record fieldValues
                        | Just (Value.Scalar (Syntax.Text prompt)) <- HashMap.lookup "text" fieldValues
                        , Just Methods{..} <- maybeMethods -> do
                            let model = case HashMap.lookup "model" fieldValues of
                                    Just (Value.Scalar (Syntax.Text m)) -> m
                                    _ -> "gpt-4o-mini"

                            let text =
                                    prompt <> "\n\nGenerate JSON output matching the following type:\n\n" <> Pretty.toSmart recordSchema

                            ChatCompletionObject{ choices = [ OpenAI.Choice{ message } ] } <- createChatCompletion _CreateChatCompletion
                                { messages = [ OpenAI.User{ content = [ OpenAI.Text{ text } ], name = Nothing } ]
                                , model = Model model
                                , response_format = Just JSON_Schema
                                    { json_schema = JSONSchema
                                        { description = Nothing
                                        , name = "result"
                                        , schema = Just jsonSchema
                                        , strict = Just True
                                        }
                                    }
                                }

                            let text_ = OpenAI.messageToContent message
                            let bytes = Encoding.encodeUtf8 text_
                            let lazyBytes =
                                    ByteString.Lazy.fromStrict bytes

                            v <- case Aeson.eitherDecode lazyBytes of
                                Left message_ -> Exception.throwIO JSONDecodingFailed{ message = message_, text = text_ }
                                Right v -> return v

                            extract (fromJSON v)
                    other ->
                        return (Value.Prompt other)

            Syntax.Scalar{..} ->
                return (Value.Scalar scalar)

            Syntax.Operator{ operator = Syntax.And, .. } -> do
                left'  <- loop env left
                right' <- loop env right

                case left' of
                    Value.Scalar (Bool True) ->
                        return right'
                    Value.Scalar (Bool False) ->
                        return (Value.Scalar (Bool False))
                    _ -> case right' of
                        Value.Scalar (Bool True) ->
                            return left'
                        Value.Scalar (Bool False) ->
                            return (Value.Scalar (Bool False))
                        _ -> do
                            return (Value.Operator left' Syntax.And right')

            Syntax.Operator{ operator = Syntax.Or, .. } -> do
                left'  <- loop env left
                right' <- loop env right

                case left' of
                    Value.Scalar (Bool True) ->
                        return (Value.Scalar (Bool True))
                    Value.Scalar (Bool False) ->
                        return right'
                    _ -> case right' of
                        Value.Scalar (Bool True) ->
                            return (Value.Scalar (Bool True))
                        Value.Scalar (Bool False) ->
                            return left'
                        _ ->
                            return (Value.Operator left' Syntax.Or right')

            Syntax.Operator{ operator = Syntax.Times, .. } -> do
                left'  <- loop env left
                right' <- loop env right

                case (left', right') of
                    (Value.Scalar (Natural 1), _) ->
                        return right'
                    (Value.Scalar (Natural 0), _) ->
                        return (Value.Scalar (Natural 0))
                    (_, Value.Scalar (Natural 1)) ->
                        return left'
                    (_, Value.Scalar (Natural 0)) ->
                        return (Value.Scalar (Natural 0))
                    (Value.Scalar l, Value.Scalar r)
                        | Natural m <- l
                        , Natural n <- r ->
                            return (Value.Scalar (Natural (m * n)))
                        | Just m <- asInteger l
                        , Just n <- asInteger r ->
                            return (Value.Scalar (Integer (m * n)))
                        | Just m <- asReal l
                        , Just n <- asReal r ->
                            return (Value.Scalar (Real (m * n)))
                    _ ->
                        return (Value.Operator left' Syntax.Times right')

            Syntax.Operator{ operator = Syntax.Plus, .. } -> do
                left'  <- loop env left
                right' <- loop env right

                case (left', right') of
                    (Value.Scalar (Natural 0), _) ->
                        return right'
                    (_, Value.Scalar (Natural 0)) ->
                        return left'
                    (Value.Scalar (Text ""), _) ->
                        return right'
                    (_, Value.Scalar (Text "")) ->
                        return left'
                    (Value.List [], _) ->
                        return right'
                    (_, Value.List []) ->
                        return left'
                    (Value.Scalar l, Value.Scalar r)
                        | Natural m <- l
                        , Natural n <- r ->
                            return (Value.Scalar (Natural (m + n)))
                        | Just m <- asInteger l
                        , Just n <- asInteger r ->
                            return (Value.Scalar (Integer (m + n)))
                        | Just m <- asReal l
                        , Just n <- asReal r ->
                            return (Value.Scalar (Real (m + n)))
                        | Text m <- l
                        , Text n <- r ->
                            return (Value.Scalar (Text (m <> n)))
                    (Value.List l, Value.List r) ->
                        return (Value.List (l <> r))
                    _ ->
                        return (Value.Operator left' Syntax.Plus right')

            Syntax.Builtin{..} ->
                return (Value.Builtin builtin)

            Syntax.Embed{ embedded = (_, value) } ->
                return value

    {-| This is the function that implements function application, including
        evaluating anonymous functions and evaluating all built-in functions.
    -}
    apply :: Value -> Value -> IO Value
    apply (Value.Lambda (Closure name capturedEnv body)) argument =
        loop ((name, argument) : capturedEnv) body
    apply
        (Value.Merge (Value.Record alternativeHandlers))
        (Value.Application (Value.Alternative alternative) x)
        | Just f <- HashMap.lookup alternative alternativeHandlers =
            apply f x
    apply
        (Value.Application (Value.Builtin ListDrop) (Value.Scalar (Natural n)))
        (Value.List elements) =
            return (Value.List (Seq.drop (fromIntegral n) elements))
    apply
        (Value.Application (Value.Builtin ListTake) (Value.Scalar (Natural n)))
        (Value.List elements) =
            return (Value.List (Seq.take (fromIntegral n) elements))
    apply (Value.Builtin ListHead) (Value.List []) =
        return (Value.Application (Value.Alternative "None") (Value.Record []))
    apply (Value.Builtin ListHead) (Value.List (x :<| _)) =
        return (Value.Application (Value.Alternative "Some") x)
    apply (Value.Builtin ListLast) (Value.List []) =
        return (Value.Application (Value.Alternative "None") (Value.Record []))
    apply (Value.Builtin ListLast) (Value.List (_ :|> x)) =
        return (Value.Application (Value.Alternative "Some") x)
    apply (Value.Builtin ListReverse) (Value.List xs) =
        return (Value.List (Seq.reverse xs))
    apply
        (Value.Application
            (Value.Application (Value.Builtin ListEqual) f)
            (Value.List rs)
        )
        (Value.List ls)
            | length ls /= length rs =
                return (Value.Scalar (Bool False))
            | Just bools <- traverse toBool (Seq.zipWith equal ls rs) =
                return (Value.Scalar (Bool (and bools)))
          where
            toBool (Value.Scalar (Bool b)) = Just b
            toBool  _                      = Nothing

            equal :: Value -> Value -> Value
            equal l r = Unsafe.unsafePerformIO do
                x <- apply f l
                apply x r
    apply
        (Value.Application
            (Value.Builtin ListFold)
            (Value.Record
                (List.sortBy (Ord.comparing fst) . HashMap.toList ->
                    [ ("cons"  , cons)
                    , ("nil"   , nil)
                    ]
                )
            )
        )
        (Value.List elements) = inner (Seq.reverse elements) nil
      where
        inner xs !result =
            case Seq.viewl xs of
                EmptyL -> do
                    return result
                y :< ys -> do
                    a <- apply cons y
                    b <- apply a result
                    inner ys b
    apply (Value.Builtin ListIndexed) (Value.List elements) =
        return (Value.List (Seq.mapWithIndex adapt elements))
      where
        adapt index value =
            Value.Record
                [ ("index", Value.Scalar (Natural (fromIntegral index)))
                , ("value", value)
                ]
    apply (Value.Builtin ListLength) (Value.List elements) =
        return (Value.Scalar (Natural (fromIntegral (length elements))))
    apply
        (Value.Application (Value.Builtin ListMap) f)
        (Value.List elements) = do
            newElements <- traverse (apply f) elements
            return (Value.List newElements)
    apply
        (Value.Application
            (Value.Application
                (Value.Builtin NaturalFold)
                (Value.Scalar (Natural n))
            )
            succ
        )
        zero =
            go n zero
      where
        go 0 !result = do
            return result
        go m !result = do
            x <- apply succ result
            go (m - 1) x
    apply (Value.Builtin IntegerEven) (Value.Scalar x)
        | Just n <- asInteger x = return (Value.Scalar (Bool (even n)))
    apply (Value.Builtin IntegerOdd) (Value.Scalar x)
        | Just n <- asInteger x = return (Value.Scalar (Bool (odd n)))
    apply
        (Value.Application (Value.Builtin RealEqual) (Value.Scalar l))
        (Value.Scalar r)
        | Just m <- asReal l
        , Just n <- asReal r =
            return (Value.Scalar (Bool (m == n)))
    apply
        (Value.Application (Value.Builtin RealLessThan) (Value.Scalar l))
        (Value.Scalar r)
        | Just m <- asReal l
        , Just n <- asReal r =
            return (Value.Scalar (Bool (m < n)))
    apply (Value.Builtin IntegerAbs) (Value.Scalar x)
        | Just n <- asInteger x =
            return (Value.Scalar (Natural (fromInteger (abs n))))
    apply (Value.Builtin RealNegate) (Value.Scalar x)
        | Just n <- asReal x =
            return (Value.Scalar (Real (negate n)))
    apply (Value.Builtin IntegerNegate) (Value.Scalar x)
        | Just n <- asInteger x =
            return (Value.Scalar (Integer (negate n)))
    apply (Value.Builtin RealShow) (Value.Scalar (Natural n)) =
        return (Value.Scalar (Text (Text.pack (show n))))
    apply (Value.Builtin RealShow) (Value.Scalar (Integer n)) =
        return (Value.Scalar (Text (Text.pack (show n))))
    apply (Value.Builtin RealShow) (Value.Scalar (Real n)) =
        return (Value.Scalar (Text (Text.pack (show n))))
    apply
        (Value.Application (Value.Builtin TextEqual) (Value.Scalar (Text l)))
        (Value.Scalar (Text r)) =
            return (Value.Scalar (Bool (l == r)))
    apply
        (Value.Application
            (Value.Builtin JSONFold)
            (Value.Record
                (List.sortBy (Ord.comparing fst) . HashMap.toList ->
                    [ ("array"  , arrayHandler )
                    , ("bool"   , boolHandler  )
                    , ("integer", integerHandler)
                    , ("natural", naturalHandler)
                    , ("null"   , nullHandler   )
                    , ("object" , objectHandler )
                    , ("real"   , realHandler  )
                    , ("string" , stringHandler )
                    ]
                )
            )
        )
        v0 = inner v0
      where
        inner (Value.Scalar (Bool b)) =
            apply boolHandler (Value.Scalar (Bool b))
        inner (Value.Scalar (Natural n)) =
            apply naturalHandler (Value.Scalar (Natural n))
        inner (Value.Scalar (Integer n)) =
            apply integerHandler (Value.Scalar (Integer n))
        inner (Value.Scalar (Real n)) =
            apply realHandler (Value.Scalar (Real n))
        inner (Value.Scalar (Text t)) =
            apply stringHandler (Value.Scalar (Text t))
        inner (Value.Scalar Null) =
            return nullHandler
        inner (Value.List elements) = do
            newElements <- traverse inner elements
            apply arrayHandler (Value.List newElements)
        inner (Value.Record keyValues) = do
            elements <- traverse adapt (HashMap.toList keyValues)
            apply objectHandler (Value.List (Seq.fromList elements))
          where
            adapt (key, value) = do
                newValue <- inner value
                return (Value.Record [("key", Value.Scalar (Text key)), ("value", newValue)])
        inner v =
            return v
    apply function argument =
        return (Value.Application function argument)

countNames :: Text -> [Text] -> Int
countNames name = length . filter (== name)

{-| Obtain a unique variable, given a list of variable names currently in scope

    >>> fresh "x" [ "x", "y", "x" ]
    Variable "x" 2
    >>> fresh "y" [ "x", "y", "x" ]
    Variable "y" 1
    >>> fresh "z" [ "x", "y", "x" ]
    Variable "z" 0
-}
fresh
    :: Text
    -- ^ Variable base name (without the index)
    -> [Text]
    -- ^ Variables currently in scope
    -> Value
    -- ^ Unique variable (including the index)
fresh name names = Value.Variable name (countNames name names)

-- | Convert a `Value` back into the surface `Syntax`
quote
    :: Maybe Methods
    -- ^ OpenAI methods
    -> [Text]
    -- ^ Variable names currently in scope (starting at @[]@ for a top-level
    --   expression)
    -> Value
    -> IO (Syntax () Void)
quote maybeMethods = loop
  where
    loop :: [Text] -> Value -> IO (Syntax () Void)
    loop names value =
        case value of
            Value.Variable name index ->
                return Syntax.Variable{ index = countNames name names - index - 1, .. }

            Value.Lambda closure@(Closure name _ _) -> do
                instantiated <- instantiate closure variable
                body <- loop (name : names) instantiated
                return Syntax.Lambda{ nameBinding = Syntax.NameBinding{ nameLocation = (), annotation = Nothing, .. }, .. }
              where
                variable = fresh name names

            Value.Application function argument -> do
                newFunction <- loop names function
                newArgument <- loop names argument
                return Syntax.Application
                    { function = newFunction
                    , argument = newArgument
                    , ..
                    }

            Value.List elements -> do
                newElements <- traverse (loop names) elements
                return Syntax.List{ elements = newElements, .. }

            Value.Record fieldValues -> do
                newFieldValues <- traverse adapt (HashMap.toList fieldValues)
                return Syntax.Record
                    { fieldValues = newFieldValues
                    , ..
                    }
              where
                adapt (field, value_) = do
                    newValue <- loop names value_
                    return (field, newValue)

            Value.Field record field -> do
                newRecord <- loop names record
                return Syntax.Field{ record = newRecord, fieldLocation = (), .. }

            Value.Alternative name ->
                return Syntax.Alternative{..}

            Value.Merge handlers -> do
                newHandlers <- loop names handlers
                return Syntax.Merge{ handlers = newHandlers, .. }

            Value.If predicate ifTrue ifFalse -> do
                newPredicate <- loop names predicate
                newIfTrue <- loop names ifTrue
                newIfFalse <- loop names ifFalse
                return Syntax.If
                    { predicate = newPredicate
                    , ifTrue = newIfTrue
                    , ifFalse = newIfFalse
                    , ..
                    }

            Value.Prompt arguments -> do
                newArguments <- loop names arguments
                return Syntax.Prompt{ arguments = newArguments, schema = Nothing, .. }

            Value.Scalar scalar ->
                return Syntax.Scalar{..}

            Value.Operator left operator right -> do
                newLeft <- loop names left
                newRight <- loop names right
                return Syntax.Operator
                    { left = newLeft
                    , operatorLocation = ()
                    , right = newRight
                    , ..
                    }

            Value.Builtin builtin ->
                return Syntax.Builtin{..}

    location = ()

    instantiate (Closure name env syntax) value =
        evaluate maybeMethods ((name, value) : env) syntax

newtype UnsupportedModelOutput = UnsupportedModelOutput{ type_ :: Type () }
    deriving (Show)

instance Exception UnsupportedModelOutput where
    displayException UnsupportedModelOutput{..} =
        "Unsupported model output type\n\
        \\n\
        \The expected type for the model output is:\n\
        \\n\
        \" <> Text.unpack (Pretty.toSmart type_) <> "\n\
        \\n\
        \… but that type cannot be encoded as JSON"

data JSONDecodingFailed = JSONDecodingFailed
    { message :: String
    , text :: Text
    } deriving (Show)

instance Exception JSONDecodingFailed where
    displayException JSONDecodingFailed{..} =
        "Failed to decode model output as JSON\n\
        \\n\
        \The model produced the following output:\n\
        \\n\
        \" <> Text.unpack text <> "\n\
        \\n\
        \… which failed to decode as JSON.\n\
        \\n\
        \Decoding error message:\n\
        \\n\
        \" <> message

data MissingSchema = MissingSchema
    deriving (Show)

instance Exception MissingSchema where
    displayException MissingSchema =
        "Internal error - Elaboration failed to infer schema for prompt"
