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

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Scientific (Scientific)
import Data.Sequence (Seq(..), ViewL(..))
import Data.Text (Text)
import Data.Void (Void)
import Grace.Location (Location)
import Grace.Syntax (Builtin(..), Scalar(..), Syntax)
import Grace.Type (Type)
import Grace.Value (Closure(..), Value)
import Prelude hiding (succ)

import qualified Control.Monad as Monad
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

        Syntax.Text{ chunks = Syntax.Chunks text rest } ->
            Value.Text (Value.Chunks text [] <> foldMap onChunk rest)
          where
            onChunk (interpolation, text₁) = case evaluate env interpolation of
                Value.Text (Value.Chunks text₀ []) ->
                    Value.Chunks (text₀ <> text₁) []
                v ->
                    Value.Chunks mempty [(v, text₁)]

        Syntax.Scalar{..} ->
            Value.Scalar scalar

        Syntax.Operator{ operator = Syntax.And, .. } ->
            case (left', right') of
                (Value.Scalar (Bool l), Value.Scalar (Bool r)) ->
                    Value.Scalar (Bool (l && r))
                _ ->
                    Value.Operator left' Syntax.And right'
          where
            left'  = evaluate env left
            right' = evaluate env right

        Syntax.Operator{ operator = Syntax.Or, .. } ->
            case (left', right') of
                (Value.Scalar (Bool l), Value.Scalar (Bool r)) ->
                    Value.Scalar (Bool (l || r))
                _ ->
                    Value.Operator left' Syntax.Or right'
          where
            left'  = evaluate env left
            right' = evaluate env right

        Syntax.Operator{ operator = Syntax.Times, .. } ->
            case (left', right') of
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
                (Value.Text l, Value.Text r) ->
                    Value.Text (l <> r)
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
    Value.Text (Value.Chunks (Text.pack (show n)) [])
apply (Value.Builtin RealShow) (Value.Scalar (Integer n)) =
    Value.Text (Value.Chunks (Text.pack (show n)) [])
apply (Value.Builtin RealShow) (Value.Scalar (Real n)) =
    Value.Text (Value.Chunks (Text.pack (show n)) [])
apply
    (Value.Application (Value.Builtin TextEqual) (Value.Text (Value.Chunks l [])))
    (Value.Text (Value.Chunks r [])) =
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
    loop (Value.Text chunks) =
        apply stringHandler (Value.Text chunks)
    loop (Value.Scalar Null) =
        nullHandler
    loop (Value.List elements) =
        apply arrayHandler (Value.List (fmap loop elements))
    loop (Value.Record keyValues) =
        apply objectHandler (Value.List (Seq.fromList (map adapt (HashMap.toList keyValues))))
      where
        adapt (key, value) =
            Value.Record
                [("key", Value.Text (Value.Chunks key [])), ("value", loop value)]
    loop v =
        v
apply function argument =
    Value.Application function argument

countNames :: Text -> [Text] -> Int
countNames name = length . filter (== name)

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

        Value.Lambda (Closure name env body) ->
            Syntax.Lambda
                { nameBinding = Syntax.NameBinding
                    { nameLocation = location
                    , annotation = Nothing
                    , ..
                    }
                , body = withEnv
                , ..
                }
          where
            quoted = fmap (quote [] . snd) body

            newBody = Monad.join (first (\_ -> location) quoted)

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

        Value.Field record field ->
            Syntax.Field{ record = quote names record, fieldLocation = location, .. }

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

        Value.Text (Value.Chunks text₀ rest) ->
            Syntax.Text{ chunks = Syntax.Chunks text₀ (fmap onChunk rest), .. }
          where
            onChunk (interpolation, text) = (quote names interpolation, text)

        Value.Scalar scalar ->
            Syntax.Scalar{..}

        Value.Operator left operator right ->
            Syntax.Operator
                { left = quote names left
                , operatorLocation = location
                , right = quote names right
                , ..
                }

        Value.Builtin builtin ->
            Syntax.Builtin{..}
  where
    location = ()
