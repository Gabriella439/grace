{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE BlockArguments    #-}
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
    -> IO Value
    -- ^ Result, free of reducible sub-expressions
evaluate = loop
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

            Syntax.Embed{ embedded = (_, value) } ->
                return value

    {-| This is the function that implements function application, including
        evaluating anonymous functions and evaluating all built-in functions.
    -}
    apply :: Value -> Value -> IO Value
    apply (Value.Lambda (Closure name capturedEnv body)) argument =
        loop ((name, argument) : capturedEnv) body
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

        Value.Alternative name ->
            Syntax.Alternative{..}

        Value.Merge handlers ->
            Syntax.Merge{ handlers = quote names handlers, .. }

        Value.Text text ->
            Syntax.Text{ chunks = Syntax.Chunks text [], .. }

        Value.Scalar scalar ->
            Syntax.Scalar{..}

        Value.Builtin builtin ->
            Syntax.Builtin{..}
  where
    location = ()
