{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains the logic for efficiently evaluating an expression
module Grace.Normalize
    ( -- * Normalization
      evaluate
    , apply
    , Value.quote
    , strip

      -- * Errors related to normalization
    , MissingCredentials(..)
    , Prompt.UnsupportedModelOutput(..)
    , JSONDecodingFailed(..)
    , MissingSchema(..)
    ) where

import Control.Exception.Safe (Exception(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (toList)
import Data.Functor (void)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Sequence (ViewL(..))
import Data.Text (Text)
import Data.Void (Void)
import Grace.Aeson (JSONDecodingFailed(..))
import Grace.Decode (FromGrace(..))
import Grace.HTTP (HTTP(..))
import Grace.Input (Input(..), Mode(..))
import Grace.Location (Location(..))
import Grace.Monad (Grace, Status(..))
import Grace.Syntax (BindMonad(..), Builtin(..), Scalar(..), Syntax)
import Grace.Value (Value)
import Prelude hiding (lookup, null, succ)

import {-# SOURCE #-} qualified Grace.Interpret as Interpret

import qualified Control.Exception.Safe as Exception
import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Yaml as YAML
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.List as List
import qualified Data.Scientific as Scientific
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Void as Void
import qualified Grace.Aeson
import qualified Grace.Context as Context
import qualified Grace.GitHub as GitHub
import qualified Grace.HTTP as HTTP
import qualified Grace.Import as Import
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
    -> [(Text, Value Location)]
    -- ^ Evaluation environment
    -> Value Location
lookupVariable name environment = case Prelude.lookup name environment of
    Just value -> value
    Nothing    -> error "Grace.Normalize.lookupVariable: unbound variable"

sorted :: Ord key => InsOrdHashMap key value -> [(key, value)]
sorted = List.sortOn fst . HashMap.toList

{-| Evaluate an expression, leaving behind a `Value` free of reducible
    sub-expressions

    This function uses separate types for the input (i.e. `Syntax`) and the
    output (i.e. `Value`) in order to avoid wastefully evaluating the same
    sub-expression multiple times.
-}
evaluate
    :: [(Text, Value Location)]
    -- ^ Evaluation environment (starting at @[]@ for a top-level expression)
    -> Syntax Location Void
    -- ^ Surface syntax
    -> Grace (Value Location)
    -- ^ Result, free of reducible sub-expressions
evaluate env₀ syntax₀ = do
    loop env₀ syntax₀
  where
    generateContext env = do
        let infer (name, assignment) = do
                let expression :: Syntax Location Input
                    expression = fmap Void.absurd (Value.quote assignment)

                let input = Code "(intermediate value)" (Pretty.toSmart expression)

                (type_, _) <- Infer.typeOf input expression

                return (name, type_, assignment)

        traverse infer env

    loop
        :: [(Text, Value Location)]
        -> Syntax Location Void
        -> Grace (Value Location)
    loop env syntax =
        case syntax of
            Syntax.Variable{ name } -> do
                pure (lookupVariable name env)

            Syntax.Application{ location, function, argument } -> Monad.join do
                function' <- loop env function
                argument' <- loop env argument
                pure (apply location function' argument')

            Syntax.Lambda{ location, binding = Syntax.PlainBinding{ plain = Syntax.NameBinding{ nameLocation, name, assignment } }, body } -> do
                newAssignment <- traverse (loop env) assignment

                pure (Value.Lambda location env (Value.Name nameLocation name newAssignment) body)

            Syntax.Lambda{ location, binding = Syntax.RecordBinding{ fieldNamesLocation, fieldNames }, body } -> do
                let process Syntax.NameBinding{ nameLocation, name, assignment } = do
                        newAssignment <- traverse (loop env) assignment

                        pure (nameLocation, name, newAssignment)

                newFieldNames <- traverse process fieldNames

                pure (Value.Lambda location env (Value.FieldNames fieldNamesLocation newFieldNames) body)

            Syntax.Annotation{ annotated, annotation  } -> do
                newAnnotated <- loop env annotated

                pure do
                    let promote (Value.Scalar location (Natural n)) Type.Scalar{ scalar = Monotype.Real } =
                            Value.Scalar location (Real (fromIntegral n))
                        promote (Value.Scalar location (Integer n)) Type.Scalar{ scalar = Monotype.Real } =
                            Value.Scalar location (Real (fromInteger n))
                        promote (Value.Scalar location (Natural n)) Type.Scalar{ scalar = Monotype.Integer } =
                            Value.Scalar location (Integer (fromIntegral n))
                        promote (Value.Text location t) Type.Scalar{ scalar = Monotype.Key } =
                            Value.Scalar location (Key t)
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
                                                Value.Scalar _ Null -> do
                                                    loop environment assignment₂
                                                Value.Application _ (Value.Builtin _ Some) v₁ -> do
                                                    return v₁
                                                v₁ -> do
                                                    return v₁

                                    action ((name, v₁) : environment)

                                Syntax.RecordBinding{ fieldNames } -> do
                                    case v of
                                        Value.Record location hashMap -> do
                                            let process Syntax.NameBinding{ name, assignment = assignment₁} = do
                                                    let missing = case assignment₁ of
                                                            Nothing -> do
                                                                return (Value.Scalar location Syntax.Null)

                                                            Just a -> do
                                                                loop environment a

                                                    value <- case HashMap.lookup name hashMap of
                                                        -- This case shouldn't happen in theory, all missing
                                                        -- fields should be elaborated to present fields set
                                                        -- to `null` but we handle it as a precaution by
                                                        -- just treating it as if elaboration had happened.
                                                        Nothing ->
                                                            missing

                                                        Just (_, Value.Scalar _ Syntax.Null) ->
                                                            missing

                                                        -- If the field had a default assignment then that
                                                        -- means that the right-hand side would be elaborated
                                                        -- to be wrapped in a `some`, which we need to undo
                                                        -- here
                                                        Just (_, Value.Application _ (Value.Builtin _ Some) a)
                                                            | Just _ <- assignment₁ ->
                                                                return a

                                                        Just (_, a) -> do
                                                            return a

                                                    return (name, value)

                                            entries <- traverse process fieldNames

                                            action (entries <> environment)
                                        _ -> do
                                            error "Grace.Normalize.evaluate: non-records can't be destructured as records"

                        case monad of
                            IdentityMonad ->
                                once value₀

                            OptionalMonad ->
                                case value₀ of
                                    Value.Scalar location Null -> do
                                        return (Value.Scalar location Null)
                                    Value.Application _ (Value.Builtin _ Some) value₁ -> do
                                        once value₁
                                    value₁ ->
                                        once value₁

                            ListMonad ->
                                case value₀ of
                                    Value.List location elements -> do
                                        values <- traverse once elements

                                        let newElements = mconcat do
                                                Value.List _ xs <- toList values

                                                return (toList xs)

                                        return (Value.List location (Seq.fromList newElements))

                                    _ ->
                                        error "Grace.Normalize.evaluate: cannot bind a non-Listin the List monad"

                let monad = (maximum . (IdentityMonad :)) do
                        Syntax.Bind{ monad = m } <- toList assignments

                        return m

                let nil environment = do
                        value <- loop environment body₀

                        return case monad of
                            IdentityMonad ->
                                value
                            ListMonad ->
                                Value.List (Value.location value) [ value ]
                            OptionalMonad ->
                                Value.Application location (Value.Builtin location Some) value
                              where
                                location = Value.location value

                foldr cons nil assignments env

            Syntax.List{ location, elements } -> do
                values <- traverse (loop env) elements

                pure (Value.List location values)

            Syntax.Record{ location, fieldValues } -> do
                let process Syntax.Definition{ nameLocation, name, bindings, assignment = assignment₀ } = do
                        let cons binding body = Syntax.Lambda
                                { location = nameLocation
                                , binding
                                , body
                                }

                        let assignment₁ = foldr cons assignment₀ bindings

                        assignment₂ <- loop env assignment₁

                        pure (name, (nameLocation, assignment₂))

                newFieldValues <- traverse process fieldValues

                pure (Value.Record location (HashMap.fromList newFieldValues))

            Syntax.Text{ location, chunks = Syntax.Chunks text rest } -> do
                let onChunk (interpolation, text₁) = do
                        value <- loop env interpolation

                        pure case value of
                            Value.Text _ text₀ ->
                                text₀ <> text₁
                            _ ->
                                error "Grace.Normalize.evaluate: interpolations must be text values"

                suffixes <- traverse onChunk rest

                pure (Value.Text location (text <> Text.concat suffixes))

            Syntax.Project{ larger, smaller } -> do
                let lookup location field fieldValues = case HashMap.lookup field fieldValues of
                        Just (_, v) -> v
                        Nothing -> Value.Scalar location Syntax.Null

                larger' <- loop env larger

                pure case (larger', smaller) of
                    (Value.Record location fieldValues, Syntax.Single{ single = Syntax.Field{ field } }) ->
                        lookup location field fieldValues

                    (Value.Record location fieldValues, Syntax.Multiple{ multiple }) ->
                        Value.Record location newFieldValues
                      where
                        process Syntax.Field{ fieldLocation, field } =
                            (field, (fieldLocation, lookup location field fieldValues))

                        fvs = map process multiple

                        newFieldValues = HashMap.fromList fvs

                    (Value.List location xs, Syntax.Index{ index })
                        | Seq.null xs -> Value.Scalar location Null
                        | otherwise ->
                            Value.Application location
                                (Value.Builtin location Some)
                                (Seq.index xs (fromInteger index `mod` Seq.length xs))
                    (Value.List location xs, Syntax.Slice{ begin, end })
                        | Seq.null xs ->
                            Value.Scalar location Null
                        | otherwise ->
                            Value.Application
                                location
                                (Value.Builtin location Some)
                                (Value.List location elements₂)
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

            Syntax.Alternative{ location, name, argument } -> do
                newArgument <- loop env argument

                pure (Value.Alternative location name newArgument)

            Syntax.Fold{ location, handlers } -> do
                newHandlers <- loop env handlers

                pure (Value.Fold location newHandlers)

            Syntax.If{ predicate, ifTrue, ifFalse } -> do
                predicate' <- loop env predicate

                ifTrue'  <- loop env ifTrue
                ifFalse' <- loop env ifFalse

                pure case predicate' of
                    Value.Scalar _ (Bool True) -> ifTrue'
                    Value.Scalar _ (Bool False) -> ifFalse'
                    _ -> error "Grace.Normalize.evaluate: if predicate must be a boolean value"

            Syntax.Prompt{ location, import_, arguments, schema } -> do
                newArguments <- loop env arguments

                prompt <- case decode newArguments of
                    Left exception -> Exception.throwIO exception
                    Right prompt -> return prompt

                Status{ context } <- State.get

                let solvedSchema = fmap (Context.solveType context) schema

                Prompt.prompt (generateContext env) import_ location prompt solvedSchema

            Syntax.HTTP{ schema = Nothing } -> do
                Exception.throwIO MissingSchema
            Syntax.HTTP{ import_, arguments, schema = Just schema } -> do
                newArguments <- loop env arguments

                http <- case decode newArguments of
                    Left exception -> Exception.throwIO exception
                    Right http -> return http

                responseBody <- liftIO (HTTP.http import_ http)

                if import_
                    then do
                        bindings <- liftIO (generateContext env)

                        uri <- liftIO (URI.mkURI (HTTP.url http))

                        parent <- Reader.ask

                        Reader.local (\i -> i <> URI uri AsCode) do
                            child <- Reader.ask

                            Import.referentiallySane parent child

                            (_, value) <- Interpret.interpretWith bindings (Just schema)

                            return value

                    else do
                        Status{ context } <- State.get

                        let solvedSchema = Context.solveType context schema

                        case solvedSchema of
                            Type.Scalar{ location, scalar = Monotype.Text } ->
                                return (Value.Text location responseBody)

                            _ -> do
                                responseValue <- liftIO (Grace.Aeson.decode responseBody)

                                value <- Infer.checkJSON solvedSchema responseValue
                                return (fmap (\_ -> Unknown) value)

            Syntax.Read{ schema = Nothing } -> do
                Exception.throwIO MissingSchema
            Syntax.Read{ import_, arguments, schema = Just schema } -> do
                newArguments <- loop env arguments

                text <- case decode newArguments of
                    Left exception -> Exception.throwIO exception
                    Right text -> return text

                if import_
                    then do
                        bindings <- generateContext env

                        parent <- Reader.ask

                        Reader.local (\i -> i <> Code "(read)" text) do
                            child <- Reader.ask

                            Import.referentiallySane parent child

                            (_, value) <- Interpret.interpretWith bindings (Just schema)

                            return value

                    else do
                        aesonValue <- liftIO (Grace.Aeson.decode text)

                        Status{ context } <- State.get

                        let solvedSchema = Context.solveType context schema

                        value <- Infer.checkJSON solvedSchema aesonValue

                        return (fmap (\_ -> Unknown) value)

            Syntax.GitHub{ schema = Nothing } -> do
                Exception.throwIO MissingSchema
            Syntax.GitHub{ import_, arguments, schema = Just schema } -> do
                newArguments <- loop env arguments

                github <- case decode newArguments of
                    Left exception -> Exception.throwIO exception
                    Right http -> return http

                url <- liftIO (GitHub.github github)

                if import_
                    then do
                        bindings <- generateContext env

                        uri <- liftIO (URI.mkURI url)

                        parent <- Reader.ask

                        Reader.local (\i -> i <> URI uri AsCode) do
                            child <- Reader.ask

                            Import.referentiallySane parent child

                            (_, value) <- Interpret.interpretWith bindings (Just schema)

                            return value

                    else do
                        responseBody <- liftIO $ HTTP.http import_ GET
                            { url = url
                            , headers = Nothing
                            , parameters = Nothing
                            }

                        Status{ context } <- State.get

                        let solvedSchema = Context.solveType context schema

                        case solvedSchema of
                            Type.Scalar{ location, scalar = Monotype.Text } ->
                                return (Value.Text location responseBody)

                            _ -> do
                                aesonValue <- liftIO (Grace.Aeson.decode responseBody)

                                value <- Infer.checkJSON solvedSchema aesonValue

                                return (fmap (\_ -> Unknown) value)

            Syntax.Show{ location, export = False, arguments = v } -> do
                v' <- loop env v

                case Value.toJSON v' of
                    Just value -> do
                        let lazyBytes = Aeson.encode value

                        let strictBytes = ByteString.Lazy.toStrict lazyBytes

                        case Encoding.decodeUtf8' strictBytes of
                            Left _ ->
                                error "Grace.Normalize.evaluate: show produced non-UTF8 text"
                            Right text ->
                                pure (Value.Text location text)

                    Nothing -> do
                        error "Grace.Normalize.evaluate: show argument is not valid JSON"

            Syntax.Show{ location, export = True, arguments = v } -> do
                v' <- loop env v

                return (Value.Text location (Pretty.toSmart (Value.quote v')))

            Syntax.Scalar{ location, scalar } ->
                pure (Value.Scalar location scalar)

            Syntax.Operator{ location, operator = Syntax.And, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                pure case (left', right') of
                    (Value.Scalar _ (Bool l), Value.Scalar _ (Bool r)) ->
                        Value.Scalar location (Bool (l && r))
                    _ ->
                        error "Grace.Normalize.evaluate: && arguments must be boolean values"

            Syntax.Operator{ location, operator = Syntax.Or, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                pure case (left', right') of
                    (Value.Scalar _ (Bool l), Value.Scalar _ (Bool r)) ->
                        Value.Scalar location (Bool (l || r))
                    _ ->
                        error "Grace.Normalize.evaluate: || arguments must be boolean values"

            Syntax.Operator{ location, operator = Syntax.Equal, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                pure (Value.Scalar location (Bool (void left' == void right')))

            Syntax.Operator{ location, operator = Syntax.NotEqual, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                pure (Value.Scalar location (Bool (left' /= right')))

            Syntax.Operator{ location, operator = Syntax.LessThan, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                pure case (left', right') of
                    (Value.Scalar _ (Natural m), Value.Scalar _ (Natural n)) ->
                        Value.Scalar location (Bool (m < n))
                    (Value.Scalar _ (Integer m), Value.Scalar _ (Integer n)) ->
                        Value.Scalar location (Bool (m < n))
                    (Value.Scalar _ (Real m), Value.Scalar _ (Real n)) ->
                        Value.Scalar location (Bool (m < n))
                    _ ->
                        error "Grace.Normalize.evaluate: < arguments must be numeric values of the same type"


            Syntax.Operator{ location, operator = Syntax.LessThanOrEqual, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                pure case (left', right') of
                    (Value.Scalar _ (Natural m), Value.Scalar _ (Natural n)) ->
                        Value.Scalar location (Bool (m <= n))
                    (Value.Scalar _ (Integer m), Value.Scalar _ (Integer n)) ->
                        Value.Scalar location (Bool (m <= n))
                    (Value.Scalar _ (Real m), Value.Scalar _ (Real n)) ->
                        Value.Scalar location (Bool (m <= n))
                    _ ->
                        error "Grace.Normalize.evaluate: <= arguments must be numeric values of the same type"

            Syntax.Operator{ location, operator = Syntax.GreaterThan, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                pure case (left', right') of
                    (Value.Scalar _ (Natural m), Value.Scalar _ (Natural n)) ->
                        Value.Scalar location (Bool (m > n))
                    (Value.Scalar _ (Integer m), Value.Scalar _ (Integer n)) ->
                        Value.Scalar location (Bool (m > n))
                    (Value.Scalar _ (Real m), Value.Scalar _ (Real n)) ->
                        Value.Scalar location (Bool (m > n))
                    _ ->
                        error "Grace.Normalize.evaluate: > arguments must be numeric values of the same type"

            Syntax.Operator{ location, operator = Syntax.GreaterThanOrEqual, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                pure case (left', right') of
                    (Value.Scalar _ (Natural m), Value.Scalar _ (Natural n)) ->
                        Value.Scalar location (Bool (m >= n))
                    (Value.Scalar _ (Integer m), Value.Scalar _ (Integer n)) ->
                        Value.Scalar location (Bool (m >= n))
                    (Value.Scalar _ (Real m), Value.Scalar _ (Real n)) ->
                        Value.Scalar location (Bool (m >= n))
                    _ ->
                        error "Grace.Normalize.evaluate: >= arguments must be numeric values of the same type"

            Syntax.Operator{ location, operator = Syntax.Times, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                pure case (left', right') of
                    (Value.Scalar _ (Natural m), Value.Scalar _ (Natural n)) ->
                        Value.Scalar location (Natural (m * n))
                    (Value.Scalar _ (Integer m), Value.Scalar _ (Integer n)) ->
                        Value.Scalar location (Integer (m * n))
                    (Value.Scalar _ (Real m), Value.Scalar _ (Real n)) ->
                        Value.Scalar location (Real (m * n))
                    _ ->
                        error "Grace.Normalize.evaluate: * arguments must be numeric values of the same type"

            Syntax.Operator{ location, operator = Syntax.Plus, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                pure case (left', right') of
                    (Value.Scalar _ (Natural m), Value.Scalar _ (Natural n)) ->
                        Value.Scalar location (Natural (m + n))
                    (Value.Scalar _ (Integer m), Value.Scalar _ (Integer n)) ->
                        Value.Scalar location (Integer (m + n))
                    (Value.Scalar _ (Real m), Value.Scalar _ (Real n)) ->
                        Value.Scalar location (Real (m + n))
                    (Value.Text _ l, Value.Text _ r) ->
                        Value.Text location (l <> r)
                    (Value.List _ l, Value.List _ r) ->
                        Value.List location (l <> r)
                    _ ->
                        error "Grace.Normalize.evaluate: + arguments must be numeric values of the same type"

            Syntax.Operator{ location, operator = Syntax.Minus, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                pure case (left', right') of
                    (Value.Scalar _ (Natural m), Value.Scalar _ (Natural n)) ->
                        Value.Scalar location (Integer (fromIntegral m - fromIntegral n))
                    (Value.Scalar _ (Integer m), Value.Scalar _ (Integer n)) ->
                        Value.Scalar location (Integer (m - n))
                    (Value.Scalar _ (Real m), Value.Scalar _ (Real n)) ->
                        Value.Scalar location (Real (m - n))
                    _ ->
                        error "Grace.Normalize.evaluate: - arguments must be numeric values of the same type"

            Syntax.Operator{ location, operator = Syntax.Modulus, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                pure do
                    let divisor = case right' of
                            Value.Scalar _ (Natural n) -> n
                            _ -> error "Grace.Normalize.evaluate: right argument to % must be a Natural number literal"

                    let (quotient, remainder) = case left' of
                            Value.Scalar _ (Natural n) ->
                                ( Value.Scalar location (Natural q)
                                , Value.Scalar location (Natural r)
                                )
                              where
                                (q, r) = n `divMod` divisor
                            Value.Scalar _ (Integer n) ->
                                ( Value.Scalar location (Integer q)
                                , Value.Scalar location (Integer r)
                                )
                              where
                                (q, r) = n `divMod` fromIntegral divisor
                            Value.Scalar _ (Real x) ->
                                ( Value.Scalar location (Integer q)
                                , Value.Scalar location (Real (fromIntegral r + f'))
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
                        location
                        [ ("quotient", (location, quotient))
                        , ("remainder", (location, remainder))
                        ]

            Syntax.Operator{ location, operator = Syntax.Divide, left, right } -> do
                left'  <- loop env left
                right' <- loop env right

                pure do
                    let numerator = case left' of
                            Value.Scalar _ (Natural n) -> fromIntegral n
                            Value.Scalar _ (Integer n) -> fromInteger n
                            Value.Scalar _ (Real    n) -> Scientific.toRealFloat n
                            _ -> error "Grace.Normalize.evaluate: / arguments must be real numbers"

                    let denominator = case right' of
                            Value.Scalar _ (Natural n) -> fromIntegral n
                            Value.Scalar _ (Integer n) -> fromInteger n
                            Value.Scalar _ (Real    n) -> Scientific.toRealFloat n
                            _ -> error "Grace.Normalize.evaluate: / arguments must be real numbers"

                    Value.Scalar location (Real (Scientific.fromFloatDigits (numerator / denominator :: Double)))

            Syntax.Builtin{ location, builtin } ->
                pure (Value.Builtin location builtin)

            Syntax.Embed{ embedded } ->
                Void.absurd embedded

{-| This is the function that implements function application, including
    evaluating anonymous functions and evaluating all built-in functions.
-}
apply
    :: Location
    -> Value Location
    -- ^ Function
    -> Value Location
    -- ^ Argument
    -> Grace (Value Location)
apply applicationLocation function₀ argument₀ = loop function₀ argument₀
  where
    loop (Value.Lambda _ capturedEnv (Value.Name _ name Nothing) body) argument =
        evaluate ((name, argument) : capturedEnv) body
    loop (Value.Lambda _ capturedEnv (Value.Name _ name (Just assignment)) body) (Value.Scalar _ Null) =
        evaluate ((name, assignment) : capturedEnv) body
    loop (Value.Lambda _ capturedEnv (Value.Name _ name (Just _)) body) (Value.Application _ (Value.Builtin _ Some) argument) =
        evaluate ((name, argument) : capturedEnv) body
    loop (Value.Lambda _ capturedEnv (Value.FieldNames _ fieldNames) body) (Value.Record keyValuesLocation keyValues) =
        evaluate (extraEnv <> capturedEnv) body
      where
        extraEnv = do
            (_, fieldName, assignment) <- fieldNames

            let value = case assignment of
                    Nothing -> case HashMap.lookup fieldName keyValues of
                        Just (_, n) -> n
                        Nothing -> Value.Scalar keyValuesLocation Null
                    Just a -> case HashMap.lookup fieldName keyValues of
                        Just (_, Value.Application _ (Value.Builtin _ Some) n) ->
                            n
                        Just (_, Value.Scalar _ Null) ->
                            a
                        Nothing ->
                            a
                        -- This case should only be hit if elaboration fails
                        Just (_, n) ->
                            n

            return (fieldName, value)
    loop
        (Value.Fold _ (Value.Record fieldValuesLocation fieldValues))
        (Value.Scalar _ (Bool b)) =
            pure (if b then trueHandler else falseHandler)
      where
        falseHandler = case HashMap.lookup "false" fieldValues of
            Nothing     -> Value.Scalar fieldValuesLocation Null
            Just (_, v) -> v

        trueHandler = case HashMap.lookup "true" fieldValues of
            Nothing     -> Value.Scalar fieldValuesLocation Null
            Just (_, v) -> v
    loop
        (Value.Fold _ (Value.Record fieldValuesLocation fieldValues))
        (Value.Scalar _ (Natural n)) = go n zero
      where
        zero = case HashMap.lookup "zero" fieldValues of
            Nothing     -> Value.Scalar fieldValuesLocation Null
            Just (_, v) -> v

        succ = case HashMap.lookup "succ" fieldValues of
            Nothing     -> Value.Scalar fieldValuesLocation Null
            Just (_, v) -> v

        go 0 !result = do
            return result
        go m !result = do
            x <- loop succ result
            go (m - 1) x
    loop
        (Value.Fold _ (Value.Record fieldValuesLocation fieldValues))
        (Value.Application _ (Value.Builtin _ Some) x) =
            loop some x
      where
        some = case HashMap.lookup "some" fieldValues of
            Nothing     -> Value.Scalar fieldValuesLocation Null
            Just (_, v) -> v

    loop
        (Value.Fold _ (Value.Record fieldValuesLocation fieldValues))
        (Value.Scalar _ Null)  =
            pure null
      where
        null = case HashMap.lookup "null" fieldValues of
            Nothing     -> Value.Scalar fieldValuesLocation Null
            Just (_, v) -> v

    loop
        (Value.Fold _ (Value.Record _ (sorted -> [("cons", (_, cons)), ("nil", (_, nil))])))
        (Value.List _ elements) = do
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
        (Value.Fold _ (Value.Record _ alternativeHandlers))
        (Value.Alternative _ alternative x)
        | Just (_, f) <- HashMap.lookup alternative alternativeHandlers =
            loop f x
    loop (Value.Fold _ (Value.Record fieldValuesLocation fieldValues)) v0 = inner v0
      where
        array = case HashMap.lookup "array" fieldValues of
            Nothing     -> Value.Scalar fieldValuesLocation Null
            Just (_, v) -> v

        bool = case HashMap.lookup "bool" fieldValues of
            Nothing     -> Value.Scalar fieldValuesLocation Null
            Just (_, v) -> v

        integer = case HashMap.lookup "integer" fieldValues of
            Nothing     -> Value.Scalar fieldValuesLocation Null
            Just (_, v) -> v

        natural = case HashMap.lookup "natural" fieldValues of
            Nothing     -> Value.Scalar fieldValuesLocation Null
            Just (_, v) -> v

        null = case HashMap.lookup "null" fieldValues of
            Nothing     -> Value.Scalar fieldValuesLocation Null
            Just (_, v) -> v

        object = case HashMap.lookup "object" fieldValues of
            Nothing     -> Value.Scalar fieldValuesLocation Null
            Just (_, v) -> v

        real = case HashMap.lookup "real" fieldValues of
            Nothing     -> Value.Scalar fieldValuesLocation Null
            Just (_, v) -> v

        string = case HashMap.lookup "string" fieldValues of
            Nothing     -> Value.Scalar fieldValuesLocation Null
            Just (_, v) -> v

        inner (Value.Scalar location (Bool b)) =
            loop bool (Value.Scalar location (Bool b))
        inner (Value.Scalar location (Natural n)) =
            loop natural (Value.Scalar location (Natural n))
        inner (Value.Scalar location (Integer n)) =
            loop integer (Value.Scalar location (Integer n))
        inner (Value.Scalar location (Real n)) =
            loop real (Value.Scalar location (Real n))
        inner (Value.Text location t) =
            loop string (Value.Text location t)
        inner (Value.Scalar _ Null) =
            pure null
        inner (Value.List location elements) = do
            newElements <- traverse inner elements
            loop array (Value.List location newElements)
        inner (Value.Record location keyValues) = do
            elements <- traverse adapt (HashMap.toList keyValues)
            loop object (Value.List location (Seq.fromList elements))
          where
            adapt (key, (keyLocation, value)) = do
                newValue <- inner value
                return
                    ( Value.Record keyLocation
                        [ ("key", (keyLocation, Value.Text keyLocation key))
                        , ("value", (keyLocation, newValue))
                        ]
                    )
        inner v =
            pure v
    loop (Value.Builtin builtinLocation Indexed) (Value.List location elements) =
        pure (Value.List location (Seq.mapWithIndex adapt elements))
      where
        adapt index value =
            Value.Record
                builtinLocation
                [ ("index", (builtinLocation, Value.Scalar builtinLocation (Natural (fromIntegral index))))
                , ("value", (builtinLocation, value))
                ]
    loop (Value.Builtin location Length) (Value.List _ elements) =
        pure (Value.Scalar location (Natural (fromIntegral (length elements))))
    loop
        (Value.Application _ (Value.Builtin _ Map) f)
        (Value.List location elements) = do
            newElements <- traverse (loop f) elements
            return (Value.List location newElements)
    loop (Value.Builtin _ Abs) (Value.Scalar location (Integer n)) =
        pure (Value.Scalar location (Natural (fromInteger (abs n))))
    loop (Value.Builtin location YAML) v = do
        case Value.toJSON v of
            Just value -> do
                let lazyBytes = YAML.encodeQuoted value

                let strictBytes = ByteString.Lazy.toStrict lazyBytes

                case Encoding.decodeUtf8' strictBytes of
                    Left _ ->
                        error "Grace.Normalize.evaluate: yaml produced non-UTF8 text"
                    Right text ->
                        pure (Value.Text location text)
            Nothing -> do
                error "Grace.Normalize.evaluate: yaml argument is not valid JSON"
    loop (Value.Builtin _ Reveal) (Value.Scalar location (Key text)) =
        pure (Value.Text location text)
    loop function argument =
        pure (Value.Application applicationLocation function argument)

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
