{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | This module contains the logic for efficiently evaluating an expression
module Grace.Normalize
    ( -- * Normalization
      evaluate
    , quote
    , apply
    ) where

import Data.Scientific (Scientific)
import Data.Sequence (Seq(..), ViewL(..))
import Data.Text (Text)
import Data.Void (Void)
import Grace.Location (Location)
import Grace.Syntax (Builtin(..), Scalar(..), Syntax)
import Grace.Type (Type)
import Grace.Value (Closure(..), Value)
import Prelude hiding (succ)

import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Grace.Syntax as Syntax
import qualified Grace.Value as Value

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

asInteger :: Scalar -> Maybe Integer
asInteger (Natural n) = Just (fromIntegral n)
asInteger (Integer n) = Just n
asInteger  _          = Nothing

asReal :: Scalar -> Maybe Scientific
asReal (Natural n) = Just (fromIntegral n)
asReal (Integer n) = Just (fromInteger  n)
asReal (Real    n) = Just n
asReal  _          = Nothing

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
evaluate env syntax =
    case syntax of
        Syntax.Variable{..} ->
            lookupVariable name index env

        Syntax.Application{..} -> apply function' argument'
          where
            function' = evaluate env function
            argument' = evaluate env argument

        Syntax.Lambda{ nameBinding = Syntax.NameBinding{ name }, ..} ->
            Value.Lambda (Closure name env body)

        Syntax.Annotation{..} ->
            evaluate env annotated

        Syntax.Let{ body = body₀, ..} ->
            evaluate (foldl snoc env bindings) body₀
          where
            snoc environment Syntax.Binding{ nameLocation, name, nameBindings, assignment } =
                (name, evaluate environment newAssignment) : environment
              where
                newAssignment = foldr cons assignment nameBindings
                  where
                    cons nameBinding body =
                      Syntax.Lambda{ location = nameLocation, ..}

        Syntax.List{..} ->
            Value.List (fmap (evaluate env) elements)

        Syntax.Record{..} ->
            Value.Record (HashMap.fromList (map adapt fieldValues))
          where
            adapt (key, value) = (key, evaluate env value)

        Syntax.Field{..} ->
            case evaluate env record of
                Value.Record fieldValues ->
                    case HashMap.lookup field fieldValues of
                        Just value -> value
                        Nothing -> Value.Scalar Syntax.Null
                other ->
                    Value.Field other field

        Syntax.Alternative{..} ->
            Value.Alternative name

        Syntax.Merge{..} ->
            Value.Merge (evaluate env handlers)

        Syntax.If{..} ->
            case predicate' of
                Value.Scalar (Bool True) -> ifTrue'
                Value.Scalar (Bool False) -> ifFalse'
                _ -> Value.If predicate' ifTrue' ifFalse'
          where
            predicate' = evaluate env predicate
            ifTrue'    = evaluate env ifTrue
            ifFalse'   = evaluate env ifFalse

        Syntax.Scalar{..} ->
            Value.Scalar scalar

        Syntax.Operator{ operator = Syntax.And, .. } ->
            case left' of
                Value.Scalar (Bool True) -> right'
                Value.Scalar (Bool False) -> Value.Scalar (Bool False)
                _ -> case right' of
                    Value.Scalar (Bool True) -> left'
                    Value.Scalar (Bool False) -> Value.Scalar (Bool False)
                    _ -> Value.Operator left' Syntax.And right'
          where
            left'  = evaluate env left
            right' = evaluate env right

        Syntax.Operator{ operator = Syntax.Or, .. } ->
            case left' of
                Value.Scalar (Bool True) -> Value.Scalar (Bool True)
                Value.Scalar (Bool False) -> right'
                _ -> case right' of
                    Value.Scalar (Bool True) -> Value.Scalar (Bool True)
                    Value.Scalar (Bool False) -> left'
                    _ -> Value.Operator left' Syntax.Or right'
          where
            left'  = evaluate env left
            right' = evaluate env right

        Syntax.Operator{ operator = Syntax.Times, .. } ->
            case (left', right') of
                (Value.Scalar (Natural 1), _) ->
                    right'
                (Value.Scalar (Natural 0), _) ->
                    Value.Scalar (Natural 0)
                (_, Value.Scalar (Natural 1)) ->
                    left'
                (_, Value.Scalar (Natural 0)) ->
                    Value.Scalar (Natural 0)
                (Value.Scalar l, Value.Scalar r)
                    | Natural m <- l
                    , Natural n <- r ->
                        Value.Scalar (Natural (m * n))
                    | Just m <- asInteger l
                    , Just n <- asInteger r ->
                        Value.Scalar (Integer (m * n))
                    | Just m <- asReal l
                    , Just n <- asReal r ->
                        Value.Scalar (Real (m * n))
                _ ->
                    Value.Operator left' Syntax.Times right'
          where
            left'  = evaluate env left
            right' = evaluate env right

        Syntax.Operator{ operator = Syntax.Plus, .. } ->
            case (left', right') of
                (Value.Scalar (Natural 0), _) ->
                    right'
                (_, Value.Scalar (Natural 0)) ->
                    left'
                (Value.Scalar (Text ""), _) ->
                    right'
                (_, Value.Scalar (Text "")) ->
                    left'
                (Value.List [], _) ->
                    right'
                (_, Value.List []) ->
                    left'
                (Value.Scalar l, Value.Scalar r)
                    | Natural m <- l
                    , Natural n <- r ->
                        Value.Scalar (Natural (m + n))
                    | Just m <- asInteger l
                    , Just n <- asInteger r ->
                        Value.Scalar (Integer (m + n))
                    | Just m <- asReal l
                    , Just n <- asReal r ->
                        Value.Scalar (Real (m + n))
                    | Text m <- l
                    , Text n <- r ->
                        Value.Scalar (Text (m <> n))
                (Value.List l, Value.List r) ->
                    Value.List (l <> r)
                _ ->
                    Value.Operator left' Syntax.Plus right'
          where
            left'  = evaluate env left
            right' = evaluate env right

        Syntax.Builtin{..} ->
            Value.Builtin builtin

        Syntax.Embed{ embedded = (_, value) } ->
            value

{-| This is the function that implements function application, including
    evaluating anonymous functions and evaluating all built-in functions.
-}
apply :: Value -> Value -> Value
apply (Value.Lambda (Closure name capturedEnv body)) argument =
    evaluate ((name, argument) : capturedEnv) body
apply
    (Value.Merge (Value.Record alternativeHandlers))
    (Value.Application (Value.Alternative alternative) x)
    | Just f <- HashMap.lookup alternative alternativeHandlers =
        apply f x
apply
    (Value.Application (Value.Builtin ListDrop) (Value.Scalar (Natural n)))
    (Value.List elements) =
        Value.List (Seq.drop (fromIntegral n) elements)
apply
    (Value.Application (Value.Builtin ListTake) (Value.Scalar (Natural n)))
    (Value.List elements) =
        Value.List (Seq.take (fromIntegral n) elements)
apply (Value.Builtin ListHead) (Value.List []) =
    Value.Application (Value.Alternative "None") (Value.Record [])
apply (Value.Builtin ListHead) (Value.List (x :<| _)) =
    Value.Application (Value.Alternative "Some") x
apply (Value.Builtin ListLast) (Value.List []) =
    Value.Application (Value.Alternative "None") (Value.Record [])
apply (Value.Builtin ListLast) (Value.List (_ :|> x)) =
    Value.Application (Value.Alternative "Some") x
apply (Value.Builtin ListReverse) (Value.List xs) =
    Value.List (Seq.reverse xs)
apply
    (Value.Application
        (Value.Application (Value.Builtin ListEqual) f)
        (Value.List rs)
    )
    (Value.List ls)
        | length ls /= length rs =
            Value.Scalar (Bool False)
        | Just bools <- traverse toBool (Seq.zipWith equal ls rs) =
        Value.Scalar (Bool (and bools))
      where
        toBool (Value.Scalar (Bool b)) = Just b
        toBool  _                      = Nothing

        equal l r = apply (apply f l) r
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
    (Value.List elements) = loop (Seq.reverse elements) nil
  where
    loop xs !result =
        case Seq.viewl xs of
            EmptyL  -> result
            y :< ys -> loop ys (apply (apply cons y) result)
apply (Value.Builtin ListIndexed) (Value.List elements) =
    Value.List (Seq.mapWithIndex adapt elements)
  where
    adapt index value =
        Value.Record
            [ ("index", Value.Scalar (Natural (fromIntegral index)))
            , ("value", value)
            ]
apply (Value.Builtin ListLength) (Value.List elements) =
    Value.Scalar (Natural (fromIntegral (length elements)))
apply
    (Value.Application (Value.Builtin ListMap) f)
    (Value.List elements) =
        Value.List (fmap (apply f) elements)
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
    go 0 !result = result
    go m !result = go (m - 1) (apply succ result)
apply (Value.Builtin IntegerEven) (Value.Scalar x)
    | Just n <- asInteger x = Value.Scalar (Bool (even n))
apply (Value.Builtin IntegerOdd) (Value.Scalar x)
    | Just n <- asInteger x = Value.Scalar (Bool (odd n))
apply
    (Value.Application (Value.Builtin RealEqual) (Value.Scalar l))
    (Value.Scalar r)
    | Just m <- asReal l
    , Just n <- asReal r =
        Value.Scalar (Bool (m == n))
apply
    (Value.Application (Value.Builtin RealLessThan) (Value.Scalar l))
    (Value.Scalar r)
    | Just m <- asReal l
    , Just n <- asReal r =
        Value.Scalar (Bool (m < n))
apply (Value.Builtin IntegerAbs) (Value.Scalar x)
    | Just n <- asInteger x = Value.Scalar (Natural (fromInteger (abs n)))
apply (Value.Builtin RealNegate) (Value.Scalar x)
    | Just n <- asReal x = Value.Scalar (Real (negate n))
apply (Value.Builtin IntegerNegate) (Value.Scalar x)
    | Just n <- asInteger x = Value.Scalar (Integer (negate n))
apply (Value.Builtin RealShow) (Value.Scalar (Natural n)) =
    Value.Scalar (Text (Text.pack (show n)))
apply (Value.Builtin RealShow) (Value.Scalar (Integer n)) =
    Value.Scalar (Text (Text.pack (show n)))
apply (Value.Builtin RealShow) (Value.Scalar (Real n)) =
    Value.Scalar (Text (Text.pack (show n)))
apply
    (Value.Application (Value.Builtin TextEqual) (Value.Scalar (Text l)))
    (Value.Scalar (Text r)) =
        Value.Scalar (Bool (l == r))
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
    v0 = loop v0
  where
    loop (Value.Scalar (Bool b)) =
        apply boolHandler (Value.Scalar (Bool b))
    loop (Value.Scalar (Natural n)) =
        apply naturalHandler (Value.Scalar (Natural n))
    loop (Value.Scalar (Integer n)) =
        apply integerHandler (Value.Scalar (Integer n))
    loop (Value.Scalar (Real n)) =
        apply realHandler (Value.Scalar (Real n))
    loop (Value.Scalar (Text t)) =
        apply stringHandler (Value.Scalar (Text t))
    loop (Value.Scalar Null) =
        nullHandler
    loop (Value.List elements) =
        apply arrayHandler (Value.List (fmap loop elements))
    loop (Value.Record keyValues) =
        apply objectHandler (Value.List (Seq.fromList (map adapt (HashMap.toList keyValues))))
      where
        adapt (key, value) =
            Value.Record
                [("key", Value.Scalar (Text key)), ("value", loop value)]
    loop v =
        v
apply function argument =
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
quote names value =
    case value of
        Value.Variable name index ->
            Syntax.Variable{ index = countNames name names - index - 1, .. }

        Value.Lambda closure@(Closure name _ _) ->
            Syntax.Lambda{ nameBinding = Syntax.NameBinding{ nameLocation = (), annotation = Nothing, .. }, .. }
          where
            variable = fresh name names

            body = quote (name : names) (instantiate closure variable)

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

        Value.Field record field ->
            Syntax.Field{ record = quote names record, fieldLocation = (), .. }

        Value.Alternative name ->
            Syntax.Alternative{..}

        Value.Merge handlers ->
            Syntax.Merge{ handlers = quote names handlers, .. }

        Value.If predicate ifTrue ifFalse ->
            Syntax.If
                { predicate = quote names predicate
                , ifTrue = quote names ifTrue
                , ifFalse = quote names ifFalse
                , ..
                }

        Value.Scalar scalar ->
            Syntax.Scalar{..}

        Value.Operator left operator right ->
            Syntax.Operator
                { left = quote names left
                , operatorLocation = ()
                , right = quote names right
                , ..
                }

        Value.Builtin builtin ->
            Syntax.Builtin{..}
  where
    location = ()
