module Grace.Normalize where

import Data.Text (Text)
import Grace.Value (Closure(..), Value)
import Grace.Syntax (Syntax)

import qualified Grace.Value  as Value
import qualified Grace.Syntax as Syntax

lookupVariable :: Text -> Int -> [(Text, Value)] -> Value
lookupVariable name index environment =
    case environment of
        (key, value) : rest ->
            if name == key
            then if index == 0
                 then value
                 else lookupVariable name (index - 1) rest
            else lookupVariable name index rest
        [] ->
            Value.Variable name (negate index - 1)

evaluate :: [(Text, Value)] -> Syntax -> Value
evaluate environment syntax =
    case syntax of
        Syntax.Variable name index ->
            lookupVariable name index environment

        Syntax.Application function argument ->
            case newFunction of
                Value.Lambda _ (Closure capturedEnvironment name body) ->
                    evaluate newCapturedEnvironment body
                  where
                    newCapturedEnvironment =
                        (name, newArgument) : capturedEnvironment
                _ ->
                    Value.Application newFunction newArgument
          where
            newFunction = evaluate environment function

            newArgument = evaluate environment argument

        Syntax.Lambda name inputType body ->
            Value.Lambda newInputType (Closure environment name body)
          where
            newInputType = evaluate environment inputType

        Syntax.Forall name inputType outputType ->
            Value.Forall newInputType (Closure environment name outputType)
          where
            newInputType = evaluate environment inputType

        Syntax.Let name _ assignment body ->
            evaluate newEnvironment body
          where
            newAssignment = evaluate environment assignment

            newEnvironment = (name, newAssignment) : environment

        Syntax.Annotation body _ ->
            evaluate environment body

        Syntax.And left right ->
            case left' of
                Value.True -> right'
                Value.False -> Value.False
                _ -> case right' of
                    Value.True -> left'
                    Value.False -> Value.False
                    _ -> Value.And left' right'
          where
            left' = evaluate environment left

            right' = evaluate environment right

        Syntax.Or left right ->
            case left' of
                Value.True -> Value.True
                Value.False -> right'
                _ -> case right' of
                    Value.True -> Value.True
                    Value.False -> left'
                    _ -> Value.Or left' right'
          where
            left' = evaluate environment left

            right' = evaluate environment right
        Syntax.Bool -> Value.Bool
        Syntax.True -> Value.True
        Syntax.False -> Value.False
        Syntax.Type -> Value.Type
        Syntax.Kind -> Value.Kind

countNames :: Text -> [Text] -> Int
countNames name = length . filter (== name)

instantiate :: Closure -> Value -> Value
instantiate (Closure environment name syntax) value =
    evaluate ((name, value) : environment) syntax

fresh :: Text -> [Text] -> Value
fresh name names = Value.Variable name (countNames name names)

quote :: [Text] -> Value -> Syntax
quote names value =
    case value of
        Value.Variable name index ->
            Syntax.Variable name (countNames name names - index - 1)

        Value.Application function argument ->
            Syntax.Application (quote names function) (quote names argument)

        Value.Lambda inputType closure@(Closure _ name _) ->
            Syntax.Lambda name (quote names inputType) newBody
          where
            variable = fresh name names

            newBody = quote (name : names) (instantiate closure variable)

        Value.Forall inputType closure@(Closure _ name _) ->
            Syntax.Forall name (quote names inputType) newOutputType
          where
            variable = fresh name names

            newOutputType = quote (name : names) (instantiate closure variable)

        Value.And left right ->
            Syntax.And (quote names left) (quote names right)

        Value.Or left right ->
            Syntax.Or (quote names left) (quote names right)

        Value.Bool -> Syntax.Bool
        Value.True -> Syntax.True
        Value.False -> Syntax.False
        Value.Type -> Syntax.Type
        Value.Kind -> Syntax.Kind

normalize :: Syntax -> Syntax
normalize = quote [] . evaluate []
