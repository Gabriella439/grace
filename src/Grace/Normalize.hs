-- | This module contains the logic for efficiently evaluating an expression
module Grace.Normalize
    ( -- * Normalization
      normalize

      -- * Internal utilities
    , evaluate
    , fresh
    , lookupVariable
    , instantiate
    , quote
    ) where

import Data.Text (Text)
import Grace.Value (Closure(..), Value)
import Grace.Syntax (Syntax)

import qualified Grace.Value  as Value
import qualified Grace.Syntax as Syntax

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

{-| Substitute an expression into a `Closure`

    > instantiate (Closure name env expression) value =
    >    evaluate ((name, value) : env) expression
-}
instantiate :: Closure -> Value -> Value
instantiate (Closure name env syntax) value =
    evaluate ((name, value) : env) syntax

{-| Evaluate an expression, leaving behind a `Value` free of reducible
    sub-expressions

    This function uses separate types for the input (i.e. `Syntax`) and the
    output (i.e. `Value`) in order to avoid wastefully evaluating the same
    sub-expression multiple times.
-}
evaluate
    :: [(Text, Value)]
    -- ^ Evaluation environment (starting at @[]@ for a top-level expression)
    -> Syntax
    -- ^ Surface syntax
    -> Value
    -- ^ Result, free of reducible sub-expressions
evaluate env syntax =
    case syntax of
        Syntax.Variable name index ->
            lookupVariable name index env

        Syntax.Application function argument ->
            case function' of
                Value.Lambda (Closure name capturedEnv body) ->
                    evaluate ((name, argument') : capturedEnv) body
                _ ->
                    Value.Application function' argument'
          where
            function' = evaluate env function

            argument' = evaluate env argument

        Syntax.Lambda name body ->
            Value.Lambda (Closure name env body)

        Syntax.Annotation annotated _ ->
            evaluate env annotated

        Syntax.Let name _ assignment body ->
            evaluate ((name, evaluate env assignment) : env) body

        Syntax.List elements ->
            Value.List (map (evaluate env) elements)

        Syntax.Record keyValues ->
            Value.Record (map adapt keyValues)
          where
            adapt (key, value) = (key, evaluate env value)

        Syntax.If predicate ifTrue ifFalse ->
            case predicate' of
                Value.True  -> ifTrue'
                Value.False -> ifFalse'
                _           -> Value.If predicate' ifTrue' ifFalse'
          where
            predicate' = evaluate env predicate

            ifTrue' = evaluate env ifTrue

            ifFalse' = evaluate env ifFalse

        Syntax.And left right ->
            case left' of
                Value.True -> right'
                Value.False -> Value.False
                _ -> case right' of
                    Value.True -> left'
                    Value.False -> Value.False
                    _ -> Value.And left' right'
          where
            left' = evaluate env left

            right' = evaluate env right

        Syntax.Or left right ->
            case left' of
                Value.True -> Value.True
                Value.False -> right'
                _ -> case right' of
                    Value.True -> Value.True
                    Value.False -> left'
                    _ -> Value.Or left' right'
          where
            left' = evaluate env left

            right' = evaluate env right

        Syntax.True ->
            Value.True

        Syntax.False ->
            Value.False

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
    :: [Text]
    -- ^ Variable names currently in scope (starting at @[]@ for a top-level
    --   expression)
    -> Value
    -> Syntax
quote names value =
    case value of
        Value.Variable name index ->
            Syntax.Variable name (countNames name names - index - 1)

        Value.Lambda closure@(Closure name _ _) ->
            Syntax.Lambda name body
          where
            variable = fresh name names

            body = quote (name : names) (instantiate closure variable)

        Value.Application function argument ->
            Syntax.Application (quote names function) (quote names argument)

        Value.List elements ->
            Syntax.List (map (quote names) elements)

        Value.Record keyValues ->
            Syntax.Record (map adapt keyValues)
          where
            adapt (key, value_) = (key, quote names value_)

        Value.If predicate ifTrue ifFalse ->
            Syntax.If
                (quote names predicate)
                (quote names ifTrue)
                (quote names ifFalse)

        Value.And left right ->
            Syntax.And (quote names left) (quote names right)

        Value.Or left right ->
            Syntax.Or (quote names left) (quote names right)

        Value.True ->
            Syntax.True

        Value.False ->
            Syntax.False

{-| Evaluate an expression

    This is a convenient wrapper around `evaluate` and `quote` in order to
    evaluate a top-level expression
-}
normalize :: Syntax -> Syntax
normalize = quote [] . evaluate []
