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

      -- * Errors related to normalization
    , MissingCredentials(..)
    , UnsupportedModelOutput(..)
    , JSONDecodingFailed(..)
    , MissingSchema(..)
    ) where

import Control.Applicative (empty)
import Control.Exception (Exception(..), SomeException)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Scientific (Scientific)
import Data.Sequence (Seq(..), ViewL(..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Grace.DataFile as DataFile
import Grace.HTTP (Methods)
import Grace.Input (Input(..))
import Grace.Location (Location)
import Grace.Syntax (Builtin(..), Scalar(..), Syntax)
import Grace.Type (Type)
import Grace.Value (Closure(..), Value)
import Numeric.Natural (Natural)
import Prelude hiding (succ)
import System.FilePath ((</>))

import {-# SOURCE #-} qualified Grace.Interpret as Interpret

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Data.Scientific as Scientific
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Void as Void
import qualified Grace.Compat as Compat
import qualified Grace.HTTP as HTTP
import qualified Grace.Monotype as Monotype
import qualified Grace.Pretty as Pretty
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type
import qualified Grace.Value as Value
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
            error "Grace.Normalize.lookupVariable: unbound variable"

asInteger :: Scalar -> Maybe Integer
asInteger (Natural n) = Just (fromIntegral n)
asInteger (Integer n) = Just n
asInteger  _          = Nothing

asReal :: Scalar -> Maybe Scientific
asReal (Natural n) = Just (fromIntegral n)
asReal (Integer n) = Just (fromInteger  n)
asReal (Real    n) = Just n
asReal  _          = Nothing

toJSONSchema :: Type a -> Either (UnsupportedModelOutput a) (Aeson.Value, Type a)
toJSONSchema original = loop original
  where
    loop Type.Forall{ location, name, type_ } = do
        loop
            (Type.substituteType name 0 Type.Scalar{ location, scalar = Monotype.Text } type_)
    loop Type.Optional{ location, type_ } = do
        (present, newType) <- loop type_

        let absent = Aeson.object [ ("type", "null") ]

        return (Aeson.object [ ("type", "object"), ("anyOf", Aeson.toJSON ([ present, absent ] :: [ Aeson.Value ]))], Type.Optional{ location, type_ = newType })

    loop Type.List{ location, type_ } = do
        (items, newType) <- loop type_

        return
            ( Aeson.object [ ("type", "array"), ("items", items) ]
            , Type.List{ location, type_ = newType }
            )
    loop Type.Record{ fields = Type.Fields fieldTypes remaining, .. } = do
        let toProperty (field, type_) = do
                (property, newType) <- loop type_

                return (field, property, newType)

        triples <- traverse toProperty fieldTypes

        let properties = do
                (field, property, _) <- triples

                return (field, property)

        let newFieldTypes = do
                (field, _, newType) <- triples

                return (field, newType)

        return
            ( Aeson.object
                [ ("type", "object")
                , ("properties", Aeson.toJSON (Map.fromList properties))
                , ("additionalProperties", Aeson.toJSON False)
                , ("required", Aeson.toJSON required)
                ]
            , Type.Record{ fields = Type.Fields newFieldTypes remaining, .. }
            )
      where
        required = do
            (field, type_) <- fieldTypes

            case type_ of
                Type.Optional{ } -> empty
                _ -> return field
    loop Type.Union{ alternatives = Type.Alternatives alternativeTypes remaining, .. } = do
        let toAnyOf (alternative, type_) = do
                (contents, newType) <- loop type_

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
                    , (alternative, newType)
                    )

        results <- traverse toAnyOf alternativeTypes

        let anyOfs = fmap fst results

        let newAlternativeTypes = fmap snd results

        return
            ( Aeson.object
                [ ("type", "object"), ("anyOf", Aeson.toJSON anyOfs) ]
            , Type.Union
                { alternatives =
                    Type.Alternatives newAlternativeTypes remaining
                , ..
                }
            )
    loop oldType@Type.Scalar{ scalar = Monotype.Bool } =
        return (Aeson.object [ ("type", "boolean") ], oldType)
    loop oldType@Type.Scalar{ scalar = Monotype.Real } =
        return (Aeson.object [ ("type", "number") ], oldType)
    loop oldType@Type.Scalar{ scalar = Monotype.Integer } =
        return (Aeson.object [ ("type", "integer") ], oldType)
    loop oldType@Type.Scalar{ scalar = Monotype.JSON } =
        return (Aeson.object [ ], oldType)
    loop oldType@Type.Scalar{ scalar = Monotype.Natural } =
        return
            (Aeson.object
                [ ("type", "number")
                -- , ("minimum", Aeson.toJSON (0 :: Int))
                -- ^ Not supported by OpenAI
                ]
            , oldType
            )
    loop oldType@Type.Scalar{ scalar = Monotype.Text } =
        return (Aeson.object [ ("type", "string") ], oldType)
    loop _ = Left UnsupportedModelOutput{..}

fromJSON :: Type a -> Aeson.Value -> Either (InvalidJSON a) Value
fromJSON Type.Union{ alternatives = Type.Alternatives alternativeTypes _ } (Aeson.Object [("contents", contents), ("tag", Aeson.String tag)])
    | Just alternativeType <- lookup tag alternativeTypes = do
        value <- fromJSON alternativeType contents

        return (Value.Application (Value.Alternative tag) value)
fromJSON type_@Type.Record{ fields = Type.Fields fieldTypes _ } value@(Aeson.Object object) = do
    let process (key, v) = do
            case lookup key fieldTypes of
                Just fieldType -> do
                    expression <- fromJSON  fieldType v

                    return (key, expression)

                Nothing -> do
                    Left InvalidJSON{..}

    textValues <- traverse process (HashMap.toList (Compat.fromAesonMap object))

    return (Value.Record (HashMap.fromList textValues))
fromJSON Type.List{ type_ } (Aeson.Array vector) = do
    elements <- traverse (fromJSON type_) vector
    return (Value.List (Seq.fromList (toList elements)))
fromJSON Type.Scalar{ scalar = Monotype.Text } (Aeson.String text) = do
    return (Value.Text text)
fromJSON type_@Type.Scalar{ scalar } value@(Aeson.Number scientific) =
    case Scientific.floatingOrInteger scientific of
        Left (_ :: Double)
            | scalar == Monotype.Real -> do
                return (Value.Scalar (Real scientific))
        Right (integer :: Integer)
            | 0 <= integer
            , scalar `elem` ([ Monotype.Natural, Monotype.Integer, Monotype.Real ] :: [Monotype.Scalar]) -> do
                return (Value.Scalar (Natural (fromInteger integer)))
            | scalar `elem` ([ Monotype.Integer, Monotype.Real ] :: [Monotype.Scalar]) -> do
                return (Value.Scalar (Integer integer))
        _ -> do
            Left InvalidJSON{..}
fromJSON Type.Scalar{ scalar = Monotype.Bool } (Aeson.Bool bool) =
    return (Value.Scalar (Bool bool))
fromJSON Type.Optional{ } Aeson.Null =
    return (Value.Scalar Null)
fromJSON type_ value = do
    Left InvalidJSON{..}

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
    -> Syntax Location Void
    -- ^ Surface syntax
    -> IO Value
    -- ^ Result, free of reducible sub-expressions
evaluate maybeMethods = loop
  where
    loop :: [(Text, Value)] -> Syntax Location Void -> IO Value
    loop env syntax =
        case syntax of
            Syntax.Variable{..} ->
                return (lookupVariable name index env)

            Syntax.Application{..} -> do
                function' <- loop env function
                argument' <- loop env argument

                apply function' argument'

            Syntax.Lambda{ nameBinding = Syntax.NameBinding{ name }, ..} ->
                return (Value.Lambda (Closure (Value.Name name) env body))

            Syntax.Lambda{ nameBinding = Syntax.FieldNamesBinding{ fieldNames }, ..} -> do
                let names = do
                        Syntax.FieldName{ name } <- fieldNames
                        return name

                return (Value.Lambda (Closure (Value.FieldNames names) env body))

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

            Syntax.Text{ chunks = Syntax.Chunks text rest } -> do
                let onChunk (interpolation, text₁) = do
                        value <- loop env interpolation
                        case value of
                            Value.Text text₀ ->
                                return (text₀ <> text₁)
                            _ ->
                                fail "Grace.Normalize.evaluate: interpolations must be text values"

                suffix <- foldMap onChunk rest

                return (Value.Text (text <> suffix))

            Syntax.Field{..} -> do
                value <- loop env record

                case value of
                    Value.Record fieldValues ->
                        case HashMap.lookup field fieldValues of
                            Just v -> return v
                            Nothing -> return (Value.Scalar Syntax.Null)
                    _ ->
                        fail "Grace.Normalize.evaluate: fields can only be accessed from record values"

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
                    _ -> fail "Grace.Normalize.evaluate: if predicate must be a boolean value"

            Syntax.Prompt{ schema = Nothing } -> do
                Exception.throwIO MissingSchema
            Syntax.Prompt{ schema = Just schema, ..} -> do
                value <- loop env arguments

                case value of
                    Value.Record fieldValues -> case maybeMethods of
                        Nothing -> do
                            Exception.throwIO MissingCredentials
                        Just methods -> do
                            let prompt = do
                                    Value.Application (Value.Builtin Some) (Value.Text p) <- HashMap.lookup "text" fieldValues
                                    return p

                            let search = case HashMap.lookup "search" fieldValues of
                                    Just (Value.Application (Value.Builtin Some) (Value.Scalar (Syntax.Bool c))) -> c
                                    _ -> False

                            let model = case HashMap.lookup "model" fieldValues of
                                    Just (Value.Application (Value.Builtin Some) (Value.Text m)) -> m
                                    _ | search -> "gpt-4o-search-preview"
                                      | otherwise -> "o4-mini"

                            let code = case HashMap.lookup "code" fieldValues of
                                    Just (Value.Application (Value.Builtin Some) (Value.Scalar (Syntax.Bool c))) -> c
                                    _ -> False

                            manager <- HTTP.newManager

                            if code
                                then do
                                    let retry :: [(Text, SomeException)] -> IO (Type Location, Value)
                                        retry errors
                                            | (_, interpretError) : rest <- errors
                                            , length rest == 3 = do
                                                Exception.throwIO interpretError
                                            | otherwise = do
                                                examples <- do
                                                    let files :: [FilePath]
                                                        files =
                                                            [ "learn-in-y-minutes.ffg"
                                                            , "chaining.ffg"
                                                            , "prompt.ffg"
                                                            , "tools.ffg"
                                                            ]

                                                    let process file = do
                                                            content <- DataFile.readDataFile ("examples" </> file)

                                                            return
                                                                ( "Example: " <> Text.pack file <> "\n\
                                                                  \\n\
                                                                  \" <> content <> "\n\
                                                                  \\n"
                                                                )

                                                    traverse process files

                                                prompts <- do
                                                    let files :: [FilePath]
                                                        files =
                                                            [ "inference.md"
                                                            , "abnf.md"
                                                            ]

                                                    let process file = do
                                                            content <- DataFile.readDataFile ("prompts" </> file)

                                                            return
                                                                ( "Post: " <> Text.pack file <> "\n\
                                                                  \\n\
                                                                  \" <> content <> "\n\
                                                                  \\n"
                                                                )

                                                    traverse process files

                                                let failedAttempts = do
                                                        (index, (program, interpretError)) <- zip [ 0 .. ] (reverse errors)
                                                        return
                                                            ( "Your failed attempt " <> Text.pack (show (index :: Natural)) <> ":\n\
                                                              \\n\
                                                              \" <> program <> "\n\
                                                              \\n\
                                                              \Error:\n\
                                                              \" <> Text.pack (displayException interpretError) <> "\n\
                                                              \\n"
                                                            )

                                                let instructions = case prompt of
                                                        Nothing ->
                                                            ""
                                                        Just p ->
                                                            "… according to these instructions:\n\
                                                            \\n\
                                                            \" <> p

                                                let input =
                                                        Text.concat prompts <> "\n\
                                                        \\n\
                                                        \" <> Text.concat examples <> "\n\
                                                        \\n\
                                                        \Now generate a standalone Grace expression matching the following type:\n\
                                                        \\n\
                                                        \" <> Pretty.toSmart schema <> "\n\
                                                        \\n\
                                                        \" <> instructions <> "\n\
                                                        \\n\
                                                        \Output a naked Grace expression without any code fence or explanation.\n\
                                                        \Your response in its entirety should be a valid input to the Grace interpreter.\n\
                                                        \\n\
                                                        \" <> Text.concat failedAttempts

                                                output <- HTTP.prompt methods input model search Nothing

                                                Interpret.interpretWith maybeMethods [] (Just schema) manager (Code "(generated)" output)
                                                    `Exception.catch` \interpretError -> do
                                                        retry ((output, interpretError) : errors)

                                    (_, e) <- retry []

                                    return e
                                else do
                                    let decode text = do
                                            let bytes = Encoding.encodeUtf8 text

                                            let lazyBytes = ByteString.Lazy.fromStrict bytes

                                            case Aeson.eitherDecode lazyBytes of
                                                Left message_ -> Exception.throwIO JSONDecodingFailed{ message = message_, text }
                                                Right v -> return v

                                    let requestJSON newSchema =
                                            instructions <> "Generate JSON output matching the following type:\n\
                                            \\n\
                                            \" <> Pretty.toSmart newSchema
                                          where
                                            instructions = case prompt of
                                                Nothing ->
                                                    ""
                                                Just p ->
                                                    p <> "\n\
                                                    \\n"

                                    let extractText = do
                                            let extract text = do
                                                    return (Value.Text text)

                                            let instructions = case prompt of
                                                    Nothing -> ""
                                                    Just p  -> p

                                            return
                                                ( instructions
                                                , Nothing
                                                , extract
                                                )

                                    let extractRecord = do
                                            (jsonSchema, newSchema) <- case toJSONSchema schema of
                                                Left exception -> Exception.throwIO exception
                                                Right result -> return result

                                            let extract text = do
                                                    v <- decode text

                                                    case fromJSON newSchema v of
                                                        Left invalidJSON -> Exception.throwIO invalidJSON
                                                        Right e -> return e

                                            return
                                                ( requestJSON newSchema
                                                , Just jsonSchema
                                                , extract
                                                )

                                    let extractNonRecord = do
                                            let adjustedSchema =
                                                    Type.Record location (Type.Fields [("response", schema)] Monotype.EmptyFields)

                                            (jsonSchema, newSchema) <- case toJSONSchema adjustedSchema of
                                                Left exception -> Exception.throwIO exception
                                                Right result -> return result

                                            let extract text = do
                                                    v <- decode text

                                                    expression <- case fromJSON newSchema v of
                                                        Left invalidJSON -> Exception.throwIO invalidJSON
                                                        Right expression -> return expression

                                                    case expression of
                                                        Value.Record [("response", response)] -> do
                                                            return response
                                                        other -> do
                                                            return other

                                            return
                                                ( requestJSON newSchema
                                                , Just jsonSchema
                                                , extract
                                                )

                                    (text, responseFormat, extract) <- case schema of
                                            Type.Scalar{ scalar = Monotype.Text } -> extractText
                                            Type.Record{ } -> extractRecord
                                            _ -> extractNonRecord

                                    output <- HTTP.prompt methods text model search responseFormat

                                    extract output
                    other ->
                        return (Value.Prompt other)

            Syntax.Scalar{..} ->
                return (Value.Scalar scalar)

            Syntax.Operator{ operator = Syntax.And, .. } -> do
                left'  <- loop env left
                right' <- loop env right

                case (left', right') of
                    (Value.Scalar (Bool l), Value.Scalar (Bool r)) ->
                        return (Value.Scalar (Bool (l && r)))
                    _ ->
                        fail "Grace.Normalize.evaluate: && arguments must be boolean values"

            Syntax.Operator{ operator = Syntax.Or, .. } -> do
                left'  <- loop env left
                right' <- loop env right

                case (left', right') of
                    (Value.Scalar (Bool l), Value.Scalar (Bool r)) ->
                        return (Value.Scalar (Bool (l || r)))
                    _ ->
                        fail "Grace.Normalize.evaluate: || arguments must be boolean values"

            Syntax.Operator{ operator = Syntax.Times, .. } -> do
                left'  <- loop env left
                right' <- loop env right

                case (left', right') of
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
                        error "Grace.Normalize.evaluate: * arguments must be numeric values"

            Syntax.Operator{ operator = Syntax.Plus, .. } -> do
                left'  <- loop env left
                right' <- loop env right

                case (left', right') of
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
                    (Value.Text l, Value.Text r) ->
                        return (Value.Text (l <> r))
                    (Value.List l, Value.List r) ->
                        return (Value.List (l <> r))
                    _ ->
                        error "Grace.Normalize.evaluate: + arguments must be values"

            Syntax.Builtin{..} ->
                return (Value.Builtin builtin)

            Syntax.Embed{ embedded } ->
                Void.absurd embedded

    {-| This is the function that implements function application, including
        evaluating anonymous functions and evaluating all built-in functions.
    -}
    apply :: Value -> Value -> IO Value
    apply (Value.Lambda (Closure (Value.Name name) capturedEnv body)) argument =
        loop ((name, argument) : capturedEnv) body
    apply (Value.Lambda (Closure (Value.FieldNames fieldNames) capturedEnv body)) (Value.Record keyValues) =
        loop (extraEnv <> capturedEnv) body
      where
        extraEnv = do
            fieldName <- fieldNames

            let value = case HashMap.lookup fieldName keyValues of
                    Nothing -> Value.Scalar Null
                    Just n  -> n

            return (fieldName, value)
    apply
        (Value.Merge (Value.Record (List.sortBy (Ord.comparing fst) . HashMap.toList -> [("null", _), ("some", someHandler)])))
        (Value.Application (Value.Builtin Some) x) =
            apply someHandler x
    apply
        (Value.Merge (Value.Record (List.sortBy (Ord.comparing fst) . HashMap.toList -> [("null", nullHandler), ("some", _)])))
        (Value.Scalar Null)  =
            return nullHandler
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
        return (Value.Scalar Null)
    apply (Value.Builtin ListHead) (Value.List (x :<| _)) =
        return (Value.Application (Value.Builtin Some) x)
    apply (Value.Builtin ListLast) (Value.List []) =
        return (Value.Scalar Null)
    apply (Value.Builtin ListLast) (Value.List (_ :|> x)) =
        return (Value.Application (Value.Builtin Some) x)
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
        return (Value.Text (Text.pack (show n)))
    apply (Value.Builtin RealShow) (Value.Scalar (Integer n)) =
        return (Value.Text (Text.pack (show n)))
    apply (Value.Builtin RealShow) (Value.Scalar (Real n)) =
        return (Value.Text (Text.pack (show n)))
    apply
        (Value.Application (Value.Builtin TextEqual) (Value.Text l))
        (Value.Text r) =
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
        inner (Value.Text t) =
            apply stringHandler (Value.Text t)
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
                return (Value.Record [("key", Value.Text key), ("value", newValue)])
        inner v =
            return v
    apply function argument =
        return (Value.Application function argument)

-- | Convert a `Value` back into the surface `Syntax`
quote
    :: [Text]
    -- ^ Variable names currently in scope (starting at @[]@ for a top-level
    --   expression)
    -> Value
    -> Syntax () Void
quote names value =
    case value of
        Value.Lambda (Closure names_ env body) ->
            Syntax.Lambda{ nameBinding, body = withEnv, ..  }
          where
            nameBinding = case names_ of
                Value.Name name ->
                    Syntax.NameBinding
                        { nameLocation = location
                        , annotation = Nothing
                        , name
                        }
                Value.FieldNames fieldNames ->
                    Syntax.FieldNamesBinding
                        { fieldNamesLocation = location
                        , fieldNames = do
                            fieldName <- fieldNames
                            return Syntax.FieldName{ name = fieldName, fieldNameLocation = location, annotation = Nothing }
                        }

            newBody = first (\_ -> location) body

            toBinding (n, v) = Syntax.Binding
                { name = n
                , nameLocation = location
                , nameBindings = []
                , annotation = Nothing
                , assignment = quote names v
                }

            withEnv = case env of
                [] -> newBody
                e : es -> Syntax.Let{ body = newBody, .. }
                  where
                    bindings = fmap toBinding (e :| es)

        Value.Application function argument ->
            Syntax.Application
                { function = quote names function
                , argument = quote names argument
                , ..
                }

        Value.List elements ->
            Syntax.List{ elements = fmap (quote names) elements, .. }

        Value.Record fieldValues ->
            Syntax.Record
                { fieldValues = map adapt (HashMap.toList fieldValues)
                , ..
                }
          where
            adapt (field, value_) = (field, quote names value_)

        Value.Alternative name ->
            Syntax.Alternative{..}

        Value.Merge handlers ->
            Syntax.Merge{ handlers = quote names handlers, .. }

        Value.Text text ->
            Syntax.Text{ chunks = Syntax.Chunks text [], .. }

        Value.Prompt arguments ->
            Syntax.Prompt
                { arguments = quote names arguments
                , schema = Nothing
                , ..
                }

        Value.Scalar scalar ->
            Syntax.Scalar{..}

        Value.Builtin builtin ->
            Syntax.Builtin{..}
  where
    location = ()

-- | Missing API credentials
data MissingCredentials = MissingCredentials
    deriving (Show)

instance Exception MissingCredentials where
    displayException MissingCredentials =
        "Missing credentials\n\
        \\n\
        \You need to provide API credentials in order to use the prompt keyword"

-- | The expected type for the model output can't be encoded as JSON
newtype UnsupportedModelOutput a = UnsupportedModelOutput{ original :: Type a }
    deriving (Show)

instance (Show a, Typeable a) => Exception (UnsupportedModelOutput a) where
    displayException UnsupportedModelOutput{..} =
        "Unsupported model output type\n\
        \\n\
        \The expected type for the model output is:\n\
        \\n\
        \" <> Text.unpack (Pretty.toSmart original) <> "\n\
        \\n\
        \… but that type cannot be encoded as JSON"

-- | JSON decoding failed
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

-- | Elaboration didn't infer a schema for the @prompt@ keyword
data MissingSchema = MissingSchema
    deriving (Show)

instance Exception MissingSchema where
    displayException MissingSchema =
        "Internal error - Elaboration failed to infer schema for prompt"

-- | Invalid JSON output which didn't match the expected type
data InvalidJSON a = InvalidJSON
    { value :: Aeson.Value
    , type_ :: Type a
    } deriving (Show)

instance (Show a, Typeable a) => Exception (InvalidJSON a) where
    displayException InvalidJSON{..} =
        "Invalid JSON\n\
        \\n\
        \The model produced the following JSON value:\n\
        \\n\
        \… which does not match the following expected type:\n\
        \\n\
        \" <> Text.unpack (Pretty.toSmart type_)
