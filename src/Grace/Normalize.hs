{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module contains the logic for efficiently evaluating an expression
module Grace.Normalize
    ( -- * Normalization
      evaluate
    , quote
    ) where

import Data.Text (Text)
import Data.Void (Void)
import Grace.Location (Location)
import Grace.Syntax (Syntax)
import Grace.Type (Type)
import Grace.Value (Closure(..), Value)
import Prelude hiding (succ)

import qualified Grace.Value  as Value
import qualified Grace.Syntax as Syntax

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
    -> Syntax Location (Type Location, Value)
    -- ^ Surface syntax
    -> Value
    -- ^ Result, free of reducible sub-expressions
evaluate env Syntax.Syntax{..} =
    case node of
        Syntax.Variable name index ->
            lookupVariable name index env

        Syntax.Application function argument ->
            case (function', argument') of
                (Value.Merge (Value.Record alternativeHandlers)
                  , Value.Application (Value.Alternative alternative) x
                  )
                  | Just f <- lookup alternative alternativeHandlers ->
                      evaluateApplication f x
                (Value.Application
                    (Value.Application Value.NaturalFold (Value.Natural n))
                    succ
                  , nil
                  ) ->
                    go n nil
                  where
                    go 0 !result = result
                    go m !result = go (m - 1) (evaluateApplication succ result)
                _ ->
                    evaluateApplication function' argument'
          where
            function' = evaluate env function
            argument' = evaluate env argument

        Syntax.Lambda _ name body ->
            Value.Lambda (Closure name env body)

        Syntax.Annotation annotated _ ->
            evaluate env annotated

        Syntax.Let bindings body ->
            evaluate (foldl snoc env bindings) body
          where
            snoc environment Syntax.Binding{ name, assignment} =
                (name, evaluate environment assignment) : environment

        Syntax.List elements ->
            Value.List (map (evaluate env) elements)

        Syntax.Record keyValues ->
            Value.Record (map adapt keyValues)
          where
            adapt (key, value) = (key, evaluate env value)

        Syntax.Field record _ key ->
            case evaluate env record of
                Value.Record keyValues
                    | Just value <- lookup key keyValues ->
                        value
                other ->
                    Value.Field other key

        Syntax.Alternative name ->
            Value.Alternative name

        Syntax.Merge record ->
            Value.Merge (evaluate env record)

        Syntax.True ->
            Value.True

        Syntax.False ->
            Value.False

        Syntax.And left _ right ->
            case left' of
                Value.True -> right'
                Value.False -> Value.False
                _ -> case right' of
                    Value.True -> left'
                    Value.False -> Value.False
                    _ -> Value.And left' right'
          where
            left'  = evaluate env left
            right' = evaluate env right

        Syntax.Or left _ right ->
            case left' of
                Value.True -> Value.True
                Value.False -> right'
                _ -> case right' of
                    Value.True -> Value.True
                    Value.False -> left'
                    _ -> Value.Or left' right'
          where
            left'  = evaluate env left
            right' = evaluate env right

        Syntax.If predicate ifTrue ifFalse ->
            case predicate' of
                Value.True  -> ifTrue'
                Value.False -> ifFalse'
                _           -> Value.If predicate' ifTrue' ifFalse'
          where
            predicate' = evaluate env predicate
            ifTrue'    = evaluate env ifTrue
            ifFalse'   = evaluate env ifFalse

        Syntax.Integer n ->
            Value.Integer n

        Syntax.Natural n ->
            Value.Natural n

        Syntax.Times left _ right ->
            case (left', right') of
                (Value.Natural 1, _              ) -> right'
                (Value.Natural 0, _              ) -> Value.Natural 0
                (_              , Value.Natural 1) -> left'
                (_              , Value.Natural 0) -> Value.Natural 0
                (Value.Natural l, Value.Natural r) -> Value.Natural (l * r)
                _                                  -> Value.Times left' right'
          where
            left'  = evaluate env left
            right' = evaluate env right

        Syntax.Plus left _ right ->
            case (left', right') of
                (Value.Natural 0, _              ) -> right'
                (_              , Value.Natural 0) -> left'
                (Value.Natural l, Value.Natural r) -> Value.Natural (l + r)
                _                                  -> Value.Plus left' right'
          where
            left'  = evaluate env left
            right' = evaluate env right

        Syntax.NaturalFold ->
            Value.NaturalFold

        Syntax.Text text ->
            Value.Text text

        Syntax.Append left _ right ->
            case (left', right') of
                (Value.Text "", _            ) -> right'
                (_            , Value.Text "") -> left'
                (Value.Text l , Value.Text r ) -> Value.Text (l <> r)
                _                              -> Value.Append left' right'
          where
            left'  = evaluate env left
            right' = evaluate env right

        Syntax.Embed (_, value) ->
            value

evaluateApplication :: Value -> Value -> Value
evaluateApplication (Value.Lambda (Closure name capturedEnv body)) argument =
    evaluate ((name, argument) : capturedEnv) body
evaluateApplication function argument =
    Value.Application function argument


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
    -> Syntax () Void
quote names value = Syntax.Syntax{..}
  where
    location = ()

    node =
        case value of
            Value.Variable name index ->
                Syntax.Variable name (countNames name names - index - 1)

            Value.Lambda closure@(Closure name _ _) ->
                Syntax.Lambda () name body
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

            Value.Field record key ->
                Syntax.Field (quote names record) () key

            Value.Alternative name ->
                Syntax.Alternative name

            Value.Merge record ->
                Syntax.Merge (quote names record)

            Value.True ->
                Syntax.True

            Value.False ->
                Syntax.False

            Value.And left right ->
                Syntax.And (quote names left) () (quote names right)

            Value.Or left right ->
                Syntax.Or (quote names left) () (quote names right)

            Value.If predicate ifTrue ifFalse ->
                Syntax.If
                    (quote names predicate)
                    (quote names ifTrue)
                    (quote names ifFalse)

            Value.Integer n ->
                Syntax.Integer n

            Value.Natural n ->
                Syntax.Natural n

            Value.Times left right ->
                Syntax.Times (quote names left) () (quote names right)

            Value.Plus left right ->
                Syntax.Plus (quote names left) () (quote names right)

            Value.NaturalFold ->
                Syntax.NaturalFold

            Value.Text text ->
                Syntax.Text text

            Value.Append left right ->
                Syntax.Append (quote names left) () (quote names right)
