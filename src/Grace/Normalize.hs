{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains the logic for efficiently evaluating an expression
module Grace.Normalize
    ( -- * Normalization
      evaluate
    , apply
    , quote
    , strip

      -- * Errors related to normalization
    , MissingCredentials(..)
    , Prompt.UnsupportedModelOutput(..)
    , JSONDecodingFailed(..)
    , MissingSchema(..)
    ) where

import Control.Exception.Safe (Exception(..), MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (MonadState(..))
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Sequence (ViewL(..))
import Data.Text (Text)
import Data.Void (Void)
import Grace.Aeson (JSONDecodingFailed(..))
import Grace.Decode (FromGrace(..))
import Grace.HTTP (HTTP(..), Methods)
import Grace.Infer (Status(..))
import Grace.Input (Input(..), Mode(..))
import Grace.Location (Location(..))
import Grace.Syntax (BindMonad(..), Builtin(..), Scalar(..), Syntax)
import Grace.Value (Value)
import Prelude hiding (lookup, null, succ)

import {-# SOURCE #-} qualified Grace.Interpret as Interpret

import qualified Control.Exception.Safe as Exception
import qualified Control.Lens as Lens
import qualified Control.Monad.State as State
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Yaml as YAML
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Ord as Ord
import qualified Data.Scientific as Scientific
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Void as Void
import qualified Grace.Aeson
import qualified Grace.GitHub as GitHub
import qualified Grace.HTTP as HTTP
import qualified Grace.Infer as Infer
import qualified Grace.Monotype as Monotype
import qualified Grace.Pretty as Pretty
import qualified Grace.Prompt as Prompt
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type
import qualified Grace.Value as Value
import qualified Text.URI as URI
import qualified Prelude

{- $setup

   >>> :set -XOverloadedStrings
-}

{-| Lookup a variable from an ordered environment of name-value pairs using the
    variable's name and index
-}
lookupVariable
    :: Text
    -- ^ Variable name
    -> [(Text, Value)]
    -- ^ Evaluation environment
    -> Value
lookupVariable name environment = case Prelude.lookup name environment of
    Just value -> value
    Nothing    -> error "Grace.Normalize.lookupVariable: unbound variable"

sorted :: Ord key => InsOrdHashMap key value -> [(key, value)]
sorted = List.sortBy (Ord.comparing fst) . HashMap.toList

{-| Evaluate an expression, leaving behind a `Value` free of reducible
    sub-expressions

    This function uses separate types for the input (i.e. `Syntax`) and the
    output (i.e. `Value`) in order to avoid wastefully evaluating the same
    sub-expression multiple times.
-}
evaluate
    :: (MonadCatch m, MonadState Status m, MonadIO m)
    => (Text -> Methods)
    -- ^ OpenAI methods
    -> [(Text, Value)]
    -- ^ Evaluation environment (starting at @[]@ for a top-level expression)
    -> Syntax Location Void
    -- ^ Surface syntax
    -> m Value
    -- ^ Result, free of reducible sub-expressions
evaluate keyToMethods env₀ syntax₀ = do
    loop env₀ syntax₀
  where
    generateContext location = do
        let infer (name, assignment) = do
                let expression :: Syntax Location Input
                    expression = first (\_ -> location) (fmap Void.absurd (quote assignment))

                let input = Code "(intermediate value)" (Pretty.toSmart expression)

                (type_, _) <- Infer.typeOf input expression

                return (name, type_, assignment)

        traverse infer env₀

    loop
        :: (MonadIO m, MonadState Status m, MonadCatch m)
        => [(Text, Value)] -> Syntax Location Void -> m Value
    loop env syntax =
        case syntax of
            Syntax.Variable{ name } -> do
                pure (lookupVariable name env)

            Syntax.Application{ function, argument } -> do
                function' <- loop env function
                argument' <- loop env argument
                apply keyToMethods function' argument'

            Syntax.Lambda{ binding = Syntax.PlainBinding{ plain = Syntax.NameBinding{ name, assignment } }, body } -> do
                newAssignment <- traverse (loop env) assignment

                pure (Value.Lambda env (Value.Name name newAssignment) body)

            Syntax.Lambda{ binding = Syntax.RecordBinding{ fieldNames }, body } -> do
                let process Syntax.NameBinding{ name, assignment } = do
                        newAssignment <- traverse (loop env) assignment

                        return (name, newAssignment)

                newFieldNames <- traverse process fieldNames

                pure (Value.Lambda env (Value.FieldNames newFieldNames) body)

            Syntax.Annotation{ annotated, annotation  } -> do
                newAnnotated <- loop env annotated

                return do
                    let promote (Value.Scalar (Natural n)) Type.Scalar{ scalar = Monotype.Real } =
                            Value.Scalar (Real (fromIntegral n))
                        promote (Value.Scalar (Integer n)) Type.Scalar{ scalar = Monotype.Real } =
                            Value.Scalar (Real (fromInteger n))
                        promote (Value.Scalar (Natural n)) Type.Scalar{ scalar = Monotype.Integer } =
                            Value.Scalar (Integer (fromIntegral n))
                        promote (Value.Text t) Type.Scalar{ scalar = Monotype.Key } =
                            Value.Scalar (Key t)
                        promote _ _ =
                            newAnnotated

                    promote newAnnotated annotation

            Syntax.Let{ assignments, body = body₀ } -> do
                let cons Syntax.Define{ definition = Syntax.Definition{ nameLocation, name, bindings, assignment } } action environment = do
                        let lambda binding body = Syntax.Lambda
                                { location = nameLocation
                                , binding
                                , body
                                }

                        let newAssignment = foldr lambda assignment bindings

                        value <- loop environment newAssignment

                        action ((name, value) : environment)

                    cons Syntax.Bind{ monad, binding, assignment = assignment₀ } action environment = do
                        value₀ <- loop environment assignment₀

                        let once v = case binding of
                                Syntax.PlainBinding{ plain = Syntax.NameBinding{ name, assignment = assignment₁ } } -> do
                                    v₁ <- case assignment₁ of
                                        Nothing -> do
                                            return v
                                        Just assignment₂ ->
                                            case v of
                                                Value.Scalar Null -> do
                                                    loop environment assignment₂
                                                Value.Application (Value.Builtin Some) v₁ -> do
                                                    return v₁
                                                v₁ -> do
                                                    return v₁
                                    action ((name, v₁) : environment)

                                Syntax.RecordBinding{ fieldNames } -> do
                                    case v of
                                        Value.Record hashMap -> do
                                            let process Syntax.NameBinding{ name, assignment = assignment₁} = do
                                                    value <- case HashMap.lookup name hashMap of
                                                        Nothing -> case assignment₁ of
                                                            Nothing -> do
                                                                return (Value.Scalar Syntax.Null)

                                                            Just a -> do
                                                                loop environment a

                                                        Just (Value.Scalar Syntax.Null) -> case assignment₁ of
                                                            Nothing -> do
                                                                return (Value.Scalar Syntax.Null)

                                                            Just a -> do
                                                                loop environment a

                                                        Just value -> do
                                                            return value

                                                    return (name, value)

                                            entries <- traverse process fieldNames

                                            action (entries <> environment)
                                        _ -> do
                                            error "Grace.Normalize.evaluate: non-records can't be destructured as records"

                        case monad of
                            UnknownMonad ->
                                error "Grace.Normalize.evaluate: unknown monad"

                            NoMonad ->
                                once value₀

                            OptionalMonad ->
                                case value₀ of
                                    Value.Scalar Null -> do
                                        return (Value.Scalar Null)
                                    Value.Application (Value.Builtin Some) value₁ -> do
                                        once value₁
                                    value₁ ->
                                        once value₁

                            ListMonad ->
                                case value₀ of
                                    Value.List elements -> do
                                        values <- traverse once elements

                                        let newElements = mconcat do
                                                Value.List xs <- toList values

                                                return (toList xs)

                                        return (Value.List (Seq.fromList newElements))

                                    _ ->
                                        error "Grace.Normalize.evaluate: cannot bind a non-Listin the List monad"

                let monad =
                        NonEmpty.last
                            (NoMonad :| [ m | Syntax.Bind{ monad = m } <- toList assignments ])

                let nil environment = do
                        value <- loop environment body₀

                        return case monad of
                            UnknownMonad ->
                                error "Grace.Normalize.evaluate: unknown monad"
                            NoMonad ->
                                value
                            ListMonad ->
                                Value.List [ value ]
                            OptionalMonad ->
                                Value.Application (Value.Builtin Some) value
                foldr cons nil assignments env

            Syntax.List{ elements } -> do
                values <- traverse (loop env) elements
                return (Value.List values)

            Syntax.Record{ fieldValues } -> do
                let process Syntax.Definition{ nameLocation, name, bindings, assignment = assignment₀ } = do
                        let cons binding body = Syntax.Lambda
                                { location = nameLocation
                                , binding
                                , body
                                }

                        let assignment₁ = foldr cons assignment₀ bindings

                        assignment₂ <- loop env assignment₁

                        return (name, assignment₂)

                newFieldValues <- traverse process fieldValues

                return (Value.Record (HashMap.fromList newFieldValues))

            Syntax.Text{ chunks = Syntax.Chunks text rest } -> do
                let onChunk (interpolation, text₁) = do
                        value <- loop env interpolation

                        return case value of
                            Value.Text text₀ ->
                                text₀ <> text₁
                            _ ->
                                error "Grace.Normalize.evaluate: interpolations must be text values"

                suffixes <- traverse onChunk rest

                return (Value.Text (text <> Text.concat suffixes))

            Syntax.Project{ larger, smaller } -> do
                let lookup field fieldValues = case HashMap.lookup field fieldValues of
                        Just v -> v
                        Nothing -> Value.Scalar Syntax.Null

                larger' <- loop env larger

                return case (larger', smaller) of
                    (Value.Record fieldValues, Syntax.Single{ single = Syntax.Field{ field } }) ->
                        lookup field fieldValues

                    (Value.Record fieldValues, Syntax.Multiple{ multiple }) ->
                        Value.Record newFieldValues
                      where
                        labels = do
                            Syntax.Field{ field } <- multiple

                            return field

                        process field = (field, lookup field fieldValues)

                        fvs = map process labels

                        newFieldValues = HashMap.fromList fvs

                    (Value.List xs, Syntax.Index{ index })
                        | Seq.null xs -> Value.Scalar Null
                        | otherwise ->
                            Value.Application
                                (Value.Builtin Some)
                                (Seq.index xs (fromInteger index `mod` Seq.length xs))
                    (Value.List xs, Syntax.Slice{ begin, end })
                        | Seq.null xs ->
                            Value.Scalar Null
                        | otherwise ->
                            Value.Application
                                (Value.Builtin Some)
                                (Value.List elements₂)
                      where
                        b = case begin of
                            Just x -> x
                            Nothing -> 0

                        e = case end of
                            Just x -> x
                            Nothing -> 0

                        n = Seq.length xs

                        elements₀ = Seq.cycleTaking (2 * n) xs

                        elements₁ = Seq.drop (fromInteger b `mod` n) elements₀

                        elements₂ =
                            Seq.take
                                ((fromInteger (e - b - 1) `mod` n) + 1)
                                elements₁
                    _ ->
                        error "Grace.Normalize.evaluate: invalid projection"

            Syntax.Alternative{ name, argument } -> do
                newArgument <- loop env argument

                pure (Value.Alternative name newArgument)

            Syntax.Fold{ handlers } -> do
                newHandlers <- loop env handlers

                return (Value.Fold newHandlers)

            Syntax.If{ predicate, ifTrue, ifFalse } -> do
                predicate' <- loop env predicate

                ifTrue'  <- loop env ifTrue
                ifFalse' <- loop env ifFalse

                return case predicate' of
                    Value.Scalar (Bool True) -> ifTrue'
                    Value.Scalar (Bool False) -> ifFalse'
                    _ -> error "Grace.Normalize.evaluate: if predicate must be a boolean value"

            Syntax.Prompt{ location, import_, arguments, schema } -> do
                newArguments <- loop env arguments

                prompt <- case decode newArguments of
                    Left exception -> Exception.throwIO exception
                    Right prompt -> return prompt

                Prompt.prompt (generateContext location) import_ location prompt schema

            Syntax.HTTP{ schema = Nothing } -> do
                Exception.throwIO MissingSchema
            Syntax.HTTP{ location, import_, arguments, schema = Just schema } -> do
                newArguments <- loop env arguments

                http <- case decode newArguments of
                    Left exception -> Exception.throwIO exception
                    Right http -> return http

                responseBody <- liftIO (HTTP.http import_ http)

                if import_
                    then do
                        bindings <- liftIO (generateContext location)

                        uri <- URI.mkURI (HTTP.url http)

                        Status{ input = parent } <- State.get

                        let child = parent <> URI uri AsCode

                        State.modify (\s -> s{ input = child })

                        (_, value) <- Interpret.interpretWith keyToMethods bindings (Just schema)

                        State.modify (\s -> s{ input = parent })

                        return value

                    else do
                        responseValue <- liftIO (Grace.Aeson.decode responseBody)

                        let defaultedSchema =
                                Lens.transform (Type.defaultTo Type.Scalar{ location, scalar = Monotype.JSON }) schema

                        case Value.checkJSON defaultedSchema responseValue of
                            Left exception -> Exception.throwIO exception
                            Right value    -> return value

            Syntax.Read{ schema = Nothing } -> do
                Exception.throwIO MissingSchema
            Syntax.Read{ location, import_, arguments, schema = Just schema } -> do
                newArguments <- loop env arguments

                text <- case decode newArguments of
                    Left exception -> Exception.throwIO exception
                    Right text -> return text

                if import_
                    then do
                        bindings <- generateContext location

                        Status{ input = parent } <- State.get

                        let child = parent <> Code "(read)" text

                        State.modify (\s -> s{ input = child })

                        (_, value) <- Interpret.interpretWith keyToMethods bindings (Just schema)

                        State.modify (\s -> s{ input = parent })

                        return value

                    else do
                        aesonValue <- liftIO (Grace.Aeson.decode text)

                        case Value.checkJSON schema aesonValue of
                            Left exception -> do
                                let value = Value.inferJSON aesonValue

                                let annotated =
                                        first (\_ -> Unknown)
                                            (fmap Void.absurd (quote value))

                                let expression = Syntax.Annotation
                                        { location = Unknown
                                        , annotated
                                        , annotation = schema
                                        }

                                let input =
                                        Code "(read)" (Pretty.toSmart @(Syntax Location Input) expression)

                                _ <- Infer.typeOf input expression

                                Exception.throwIO exception

                            Right value -> do
                                return value

            Syntax.GitHub{ schema = Nothing } -> do
                Exception.throwIO MissingSchema
            Syntax.GitHub{ location, import_, arguments, schema = Just schema } -> do
                newArguments <- loop env arguments

                github <- case decode newArguments of
                    Left exception -> Exception.throwIO exception
                    Right http -> return http

                url <- liftIO (GitHub.github github)

                if import_
                    then do
                        bindings <- generateContext location

                        uri <- URI.mkURI url

                        Status{ input = parent } <- State.get

                        let child = parent <> URI uri AsCode

                        State.modify (\s -> s{ input = child })

                        (_, value) <- Interpret.interpretWith keyToMethods bindings (Just schema)

                        State.modify (\s -> s{ input = parent })

                        return value

                    else do
                        responseBody <- liftIO $ HTTP.http import_ GET
                            { url = url
                            , headers = Nothing
                            , parameters = Nothing
                            }

                        aesonValue <- liftIO (Grace.Aeson.decode responseBody)

                        let defaultedSchema =
                                Lens.transform (Type.defaultTo Type.Scalar{ location, scalar = Monotype.JSON }) schema

                        case Value.checkJSON defaultedSchema aesonValue of
                            Left exception -> do
                                let value = Value.inferJSON aesonValue

                                let annotated =
                                        first (\_ -> Unknown)
                                            (fmap Void.absurd (quote value))

                                let expression = Syntax.Annotation
                                        { location = Unknown
                                        , annotated
                                        , annotation = schema
                                        }

                                let input =
                                        Code "(read)" (Pretty.toSmart @(Syntax Location Input) expression)

                                _ <- Infer.typeOf input expression

                                Exception.throwIO exception

                            Right value -> do
                                return value


            Syntax.Scalar{ scalar } ->
                pure (Value.Scalar scalar)

            Syntax.Operator{ operator = Syntax.And, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                return case (left', right') of
                    (Value.Scalar (Bool l), Value.Scalar (Bool r)) ->
                        Value.Scalar (Bool (l && r))
                    _ ->
                        error "Grace.Normalize.evaluate: && arguments must be boolean values"

            Syntax.Operator{ operator = Syntax.Or, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                return case (left', right') of
                    (Value.Scalar (Bool l), Value.Scalar (Bool r)) ->
                        Value.Scalar (Bool (l || r))
                    _ ->
                        error "Grace.Normalize.evaluate: || arguments must be boolean values"

            Syntax.Operator{ operator = Syntax.Equal, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                return (Value.Scalar (Bool (left' == right')))

            Syntax.Operator{ operator = Syntax.NotEqual, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                return (Value.Scalar (Bool (left' /= right')))

            Syntax.Operator{ operator = Syntax.LessThan, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                return case (left', right') of
                    (Value.Scalar (Natural m), Value.Scalar (Natural n)) ->
                        Value.Scalar (Bool (m < n))
                    (Value.Scalar (Integer m), Value.Scalar (Integer n)) ->
                        Value.Scalar (Bool (m < n))
                    (Value.Scalar (Real m), Value.Scalar (Real n)) ->
                        Value.Scalar (Bool (m < n))
                    _ ->
                        error "Grace.Normalize.evaluate: < arguments must be numeric values of the same type"


            Syntax.Operator{ operator = Syntax.LessThanOrEqual, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                return case (left', right') of
                    (Value.Scalar (Natural m), Value.Scalar (Natural n)) ->
                        Value.Scalar (Bool (m <= n))
                    (Value.Scalar (Integer m), Value.Scalar (Integer n)) ->
                        Value.Scalar (Bool (m <= n))
                    (Value.Scalar (Real m), Value.Scalar (Real n)) ->
                        Value.Scalar (Bool (m <= n))
                    _ ->
                        error "Grace.Normalize.evaluate: <= arguments must be numeric values of the same type"

            Syntax.Operator{ operator = Syntax.GreaterThan, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                return case (left', right') of
                    (Value.Scalar (Natural m), Value.Scalar (Natural n)) ->
                        Value.Scalar (Bool (m > n))
                    (Value.Scalar (Integer m), Value.Scalar (Integer n)) ->
                        Value.Scalar (Bool (m > n))
                    (Value.Scalar (Real m), Value.Scalar (Real n)) ->
                        Value.Scalar (Bool (m > n))
                    _ ->
                        error "Grace.Normalize.evaluate: > arguments must be numeric values of the same type"

            Syntax.Operator{ operator = Syntax.GreaterThanOrEqual, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                return case (left', right') of
                    (Value.Scalar (Natural m), Value.Scalar (Natural n)) ->
                        Value.Scalar (Bool (m >= n))
                    (Value.Scalar (Integer m), Value.Scalar (Integer n)) ->
                        Value.Scalar (Bool (m >= n))
                    (Value.Scalar (Real m), Value.Scalar (Real n)) ->
                        Value.Scalar (Bool (m >= n))
                    _ ->
                        error "Grace.Normalize.evaluate: >= arguments must be numeric values of the same type"

            Syntax.Operator{ operator = Syntax.Times, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                return case (left', right') of
                    (Value.Scalar (Natural m), Value.Scalar (Natural n)) ->
                        Value.Scalar (Natural (m * n))
                    (Value.Scalar (Integer m), Value.Scalar (Integer n)) ->
                        Value.Scalar (Integer (m * n))
                    (Value.Scalar (Real m), Value.Scalar (Real n)) ->
                        Value.Scalar (Real (m * n))
                    _ ->
                        error "Grace.Normalize.evaluate: * arguments must be numeric values of the same type"

            Syntax.Operator{ operator = Syntax.Plus, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                return case (left', right') of
                    (Value.Scalar (Natural m), Value.Scalar (Natural n)) ->
                        Value.Scalar (Natural (m + n))
                    (Value.Scalar (Integer m), Value.Scalar (Integer n)) ->
                        Value.Scalar (Integer (m + n))
                    (Value.Scalar (Real m), Value.Scalar (Real n)) ->
                        Value.Scalar (Real (m + n))
                    (Value.Text l, Value.Text r) ->
                        Value.Text (l <> r)
                    (Value.List l, Value.List r) ->
                        Value.List (l <> r)
                    _ ->
                        error "Grace.Normalize.evaluate: + arguments must be numeric values of the same type"

            Syntax.Operator{ operator = Syntax.Minus, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                return case (left', right') of
                    (Value.Scalar (Natural m), Value.Scalar (Natural n)) ->
                        Value.Scalar (Integer (fromIntegral m - fromIntegral n))
                    (Value.Scalar (Integer m), Value.Scalar (Integer n)) ->
                        Value.Scalar (Integer (m - n))
                    (Value.Scalar (Real m), Value.Scalar (Real n)) ->
                        Value.Scalar (Real (m - n))
                    _ ->
                        error "Grace.Normalize.evaluate: - arguments must be numeric values of the same type"

            Syntax.Operator{ operator = Syntax.Modulus, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                pure do
                    let divisor = case right' of
                            Value.Scalar (Natural n) -> n
                            _ -> error "Grace.Normalize.evaluate: right argument to % must be a Natural number literal"

                    let (quotient, remainder) = case left' of
                            Value.Scalar (Natural n) ->
                                ( Value.Scalar (Natural q)
                                , Value.Scalar (Natural r)
                                )
                              where
                                (q, r) = n `divMod` divisor
                            Value.Scalar (Integer n) ->
                                ( Value.Scalar (Integer q)
                                , Value.Scalar (Integer r)
                                )
                              where
                                (q, r) = n `divMod` fromIntegral divisor
                            Value.Scalar (Real x) ->
                                ( Value.Scalar (Integer q)
                                , Value.Scalar (Real (fromIntegral r + f'))
                                )
                              where
                                (n, f) = properFraction x

                                (n', f')
                                    | f < 0     = (n - 1, f + 1)
                                    | otherwise = (n, f)

                                (q, r) =
                                    n' `divMod` fromIntegral divisor
                            _ ->
                                error "Grace.Normalize.evaluate: left argument to % must be a numeric value"

                    Value.Record
                        [ ("quotient", quotient)
                        , ("remainder", remainder)
                        ]

            Syntax.Operator{ operator = Syntax.Divide, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                pure do
                    let numerator = case left' of
                            Value.Scalar (Natural n) -> fromIntegral n
                            Value.Scalar (Integer n) -> fromInteger n
                            Value.Scalar (Real    n) -> Scientific.toRealFloat n
                            _ -> error "Grace.Normalize.evaluate: / arguments must be real numbers"

                    let denominator = case right' of
                            Value.Scalar (Natural n) -> fromIntegral n
                            Value.Scalar (Integer n) -> fromInteger n
                            Value.Scalar (Real    n) -> Scientific.toRealFloat n
                            _ -> error "Grace.Normalize.evaluate: / arguments must be real numbers"

                    Value.Scalar (Real (Scientific.fromFloatDigits (numerator / denominator :: Double)))

            Syntax.Builtin{ builtin } ->
                pure (Value.Builtin builtin)

            Syntax.Embed{ embedded } ->
                Void.absurd embedded

{-| This is the function that implements function application, including
    evaluating anonymous functions and evaluating all built-in functions.
-}
apply
    :: (MonadCatch m, MonadState Status m, MonadIO m)
    => (Text -> Methods)
    -- ^ OpenAI methods
    -> Value
    -- ^ Function
    -> Value
    -- ^ Argument
    -> m Value
apply keyToMethods function₀ argument₀ = loop function₀ argument₀
  where
    loop (Value.Lambda capturedEnv (Value.Name name Nothing) body) argument =
        evaluate keyToMethods ((name, argument) : capturedEnv) body
    loop (Value.Lambda capturedEnv (Value.Name name (Just assignment)) body) (Value.Scalar Null) =
        evaluate keyToMethods ((name, assignment) : capturedEnv) body
    loop (Value.Lambda capturedEnv (Value.Name name (Just _)) body) (Value.Application (Value.Builtin Some) argument) =
        evaluate keyToMethods ((name, argument) : capturedEnv) body
    loop (Value.Lambda capturedEnv (Value.FieldNames fieldNames) body) (Value.Record keyValues) =
        evaluate keyToMethods (extraEnv <> capturedEnv) body
      where
        extraEnv = do
            (fieldName, assignment) <- fieldNames

            let value = case assignment of
                    Nothing -> case HashMap.lookup fieldName keyValues of
                        Just n -> n
                        Nothing -> Value.Scalar Null
                    Just a -> case HashMap.lookup fieldName keyValues of
                        Just (Value.Application (Value.Builtin Some) n) ->
                            n
                        Just (Value.Scalar Null) ->
                            a
                        Nothing ->
                            a
                        -- This case should only be hit if elaboration fails
                        Just n ->
                            n

            return (fieldName, value)
    loop
        (Value.Fold (Value.Record (sorted -> [("false", falseHandler), ("true", trueHandler)])))
        (Value.Scalar (Bool b)) =
            pure (if b then trueHandler else falseHandler)
    loop
        (Value.Fold (Value.Record (sorted -> [("succ", succ), ("zero", zero)])))
        (Value.Scalar (Natural n)) = go n zero
      where
        go 0 !result = do
            return result
        go m !result = do
            x <- loop succ result
            go (m - 1) x
    loop
        (Value.Fold (Value.Record (sorted -> [("null", _), ("some", some)])))
        (Value.Application (Value.Builtin Some) x) =
            loop some x
    loop
        (Value.Fold (Value.Record (sorted -> [("null", null), ("some", _)])))
        (Value.Scalar Null)  =
            pure null
    loop
        (Value.Fold (Value.Record (sorted -> [("cons", cons), ("nil", nil)])))
        (Value.List elements) = do
            inner (Seq.reverse elements) nil
      where
        inner xs !result =
            case Seq.viewl xs of
                EmptyL -> do
                    return result
                y :< ys -> do
                    a <- loop cons y
                    b <- loop a result
                    inner ys b
    loop
        (Value.Fold
            (Value.Record
                (sorted ->
                    [ ("array"  , array  )
                    , ("bool"   , bool   )
                    , ("integer", integer)
                    , ("natural", natural)
                    , ("null"   , null   )
                    , ("object" , object )
                    , ("real"   , real   )
                    , ("string" , string )
                    ]
                )
            )
        )
        v0 = inner v0
      where
        inner (Value.Scalar (Bool b)) =
            loop bool (Value.Scalar (Bool b))
        inner (Value.Scalar (Natural n)) =
            loop natural (Value.Scalar (Natural n))
        inner (Value.Scalar (Integer n)) =
            loop integer (Value.Scalar (Integer n))
        inner (Value.Scalar (Real n)) =
            loop real (Value.Scalar (Real n))
        inner (Value.Text t) =
            loop string (Value.Text t)
        inner (Value.Scalar Null) =
            pure null
        inner (Value.List elements) = do
            newElements <- traverse inner elements
            loop array (Value.List newElements)
        inner (Value.Record keyValues) = do
            elements <- traverse adapt (HashMap.toList keyValues)
            loop object (Value.List (Seq.fromList elements))
          where
            adapt (key, value) = do
                newValue <- inner value
                return (Value.Record [("key", Value.Text key), ("value", newValue)])
        inner v =
            pure v
    loop
        (Value.Fold (Value.Record alternativeHandlers))
        (Value.Alternative alternative x)
        | Just f <- HashMap.lookup alternative alternativeHandlers =
            loop f x
    loop (Value.Builtin Indexed) (Value.List elements) =
        pure (Value.List (Seq.mapWithIndex adapt elements))
      where
        adapt index value =
            Value.Record
                [ ("index", Value.Scalar (Natural (fromIntegral index)))
                , ("value", value)
                ]
    loop (Value.Builtin Length) (Value.List elements) =
        pure (Value.Scalar (Natural (fromIntegral (length elements))))
    loop
        (Value.Application (Value.Builtin Map) f)
        (Value.List elements) = do
            newElements <- traverse (loop f) elements
            return (Value.List newElements)
    loop (Value.Builtin Abs) (Value.Scalar (Integer n)) =
        pure (Value.Scalar (Natural (fromInteger (abs n))))
    loop (Value.Builtin Show) v = do
        case Value.toJSON v of
            Just value -> do
                let lazyBytes = Aeson.encode value

                let strictBytes = ByteString.Lazy.toStrict lazyBytes

                case Encoding.decodeUtf8' strictBytes of
                    Left _ ->
                        error "Grace.Normalize.evaluate: show produced non-UTF8 text"
                    Right text ->
                        pure (Value.Text text)
            Nothing -> do
                error "Grace.Normalize.evaluate: show argument is not valid JSON"
    loop (Value.Builtin YAML) v = do
        case Value.toJSON v of
            Just value -> do
                let lazyBytes = YAML.encodeQuoted value

                let strictBytes = ByteString.Lazy.toStrict lazyBytes

                case Encoding.decodeUtf8' strictBytes of
                    Left _ ->
                        error "Grace.Normalize.evaluate: yaml produced non-UTF8 text"
                    Right text ->
                        pure (Value.Text text)
            Nothing -> do
                error "Grace.Normalize.evaluate: yaml argument is not valid JSON"
    loop (Value.Builtin Reveal) (Value.Scalar (Key text)) =
        pure (Value.Text text)
    loop function argument =
        pure (Value.Application function argument)

-- | Convert a `Value` back into the surface `Syntax`
quote :: Value -> Syntax () Void
quote value = case value of
    Value.Lambda env names_ body₀ ->
        foldl snoc newLambda env
      where
        binding = case names_ of
            Value.Name name assignment ->
                Syntax.PlainBinding
                    { plain = Syntax.NameBinding
                        { nameLocation = location
                        , name
                        , annotation = Nothing
                        , assignment = fmap quote assignment
                        }
                    }

            Value.FieldNames fieldNames ->
                Syntax.RecordBinding
                    { fieldNamesLocation = location
                    , fieldNames = do
                        (name, assignment) <- fieldNames
                        return Syntax.NameBinding
                            { nameLocation = location
                            , name
                            , annotation = Nothing
                            , assignment = fmap quote assignment
                            }
                    }

        newLambda = Syntax.Lambda
            { location
            , binding
            , body = first (\_ -> location) body₀
            }

        toBinding n v = Syntax.Define
            { assignmentLocation = location
            , definition = Syntax.Definition
                { name = n
                , nameLocation = location
                , bindings = []
                , annotation = Nothing
                , assignment = quote v
                }
            }

        snoc e@Syntax.Let{ assignments = a :| as, body = body₁ } (n, v)
            | Syntax.usedIn n e = Syntax.Let
                { location
                , assignments = toBinding n v :| (a : as)
                , body = body₁
                }
            | otherwise = e
        snoc e (n, v)
            | Syntax.usedIn n e = Syntax.Let
                { location
                , assignments = toBinding n v :| []
                , body = e
                }
            | otherwise = e

    Value.Application function argument ->
        Syntax.Application
            { location
            , function = quote function
            , argument = quote argument
            }

    Value.List elements ->
        Syntax.List{ location, elements = fmap quote elements }

    Value.Record fieldValues ->
        Syntax.Record
            { location
            , fieldValues = map adapt (HashMap.toList fieldValues)
            }
      where
        adapt (field, value_) = Syntax.Definition
            { nameLocation = location
            , name = field
            , bindings = []
            , annotation = Nothing
            , assignment = quote value_
            }

    Value.Alternative name argument ->
        Syntax.Alternative{ location, name, argument  = quote argument }

    Value.Fold handlers ->
        Syntax.Fold{ location, handlers = quote handlers }

    Value.Text text ->
        Syntax.Text{ location, chunks = Syntax.Chunks text [] }

    Value.Scalar scalar ->
        Syntax.Scalar{ location, scalar }

    Value.Builtin builtin ->
        Syntax.Builtin{ location, builtin }
  where
    location = ()

-- | Strip all `Some`s from a `Syntax` tree
strip :: Syntax s a -> Syntax s a
strip = Lens.transform transformation
  where
    transformation Syntax.Application{ function = Syntax.Builtin{ builtin = Some }, argument } =
        argument
    transformation e =
        e

-- | Missing API credentials
data MissingCredentials = MissingCredentials
    deriving stock (Show)

instance Exception MissingCredentials where
    displayException MissingCredentials =
        "Missing credentials\n\
        \\n\
        \You need to provide API credentials in order to use the prompt keyword"

-- | Elaboration didn't infer a schema
data MissingSchema = MissingSchema
    deriving stock (Show)

instance Exception MissingSchema where
    displayException MissingSchema =
        "Internal error - Elaboration failed to infer schema"
