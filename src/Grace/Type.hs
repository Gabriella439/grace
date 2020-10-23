{-| This module contains the logic for inferring the type of an expression,
    and detecting any internal type errors within that expression
-}

module Grace.Type
    ( -- * Type-checking
      TypeError
    , typeOf
    ) where

import Data.Text (Text)
import Grace.Syntax (Syntax)
import Grace.Value (Closure(..), Value)

import qualified Grace.Normalize as Normalize
import qualified Grace.Syntax    as Syntax
import qualified Grace.Value     as Value

-- | Type used to encode failures in the type-checking algorithm
type TypeError = String -- TODO: Use a better error type

check
    :: [Text]
    -> [(Text, Value)]
    -> [(Text, Value)]
    -> Syntax
    -> Value
    -> Either TypeError ()
check names context environment term typeAnnotation = do
        inferredType <- infer names context environment term

        if equivalent names inferredType typeAnnotation
            then return ()
            else Left "Type mismatch"

infer
    :: [Text]
    -> [(Text, Value)]
    -> [(Text, Value)]
    -> Syntax
    -> Either TypeError Value
infer names context environment syntax =
    case syntax of
        Syntax.Variable name index -> do
            let value = Normalize.lookupVariable name index context

            case value of
                Value.Variable _ i | i < 0 -> Left "Unbound variable"
                _                          -> return value

        Syntax.Lambda name inputType body -> do
            let variable = Normalize.fresh name names

            let newNames = name : names

            _ <- infer names context environment inputType

            let evaluatedInputType = Normalize.evaluate environment inputType

            let newContext = (name, evaluatedInputType) : context

            let newEnvironment = (name, variable) : environment

            bodyType <- infer newNames newContext newEnvironment body

            let quotedBodyType = Normalize.quote names bodyType

            return (Value.Forall evaluatedInputType (Value.Closure environment name quotedBodyType))

        Syntax.Forall name inputType outputType -> do
            -- TODO: Generalize to pure type system
            check names context environment inputType Value.Type

            let newNames = name : names

            let evaluatedInputType = Normalize.evaluate environment inputType

            let newContext = (name, evaluatedInputType) : context

            let variable = Normalize.fresh name names

            let newEnvironment = (name, variable) : environment

            check newNames newContext newEnvironment outputType Value.Type

            return Value.Type

        Syntax.Application function argument -> do
            functionType <- infer names context environment function

            case functionType of
                Value.Forall inputType closure -> do
                    check names context environment argument inputType

                    let evaluatedArgument =
                            Normalize.evaluate environment argument

                    return (Normalize.instantiate closure evaluatedArgument)
                _ -> do
                    Left "Not a function type"

        Syntax.Let name maybeAnnotation assignment body -> do
            evaluatedAnnotation <- case maybeAnnotation of
                Nothing -> do
                    infer names context environment assignment
                Just annotation -> do
                    _ <- infer names context environment annotation

                    let evaluatedAnnotation = Normalize.evaluate environment annotation

                    check names context environment assignment evaluatedAnnotation

                    return evaluatedAnnotation

            let newNames = name : names

            let newContext = (name, evaluatedAnnotation) : context

            let evaluatedAssignment = Normalize.evaluate environment assignment

            let newEnvironment = (name, evaluatedAssignment) : environment

            infer newNames newContext newEnvironment body

        Syntax.Annotation body annotation -> do
            _ <- infer names context environment annotation

            let evaluatedAnnotation = Normalize.evaluate environment annotation

            check names context environment body evaluatedAnnotation

            return (Normalize.evaluate environment annotation)

        Syntax.If predicate ifTrue ifFalse -> do
            check names context environment predicate Value.Bool

            ifTrueType <- infer names context environment ifTrue

            ifFalseType <- infer names context environment ifFalse

            if equivalent names ifTrueType ifFalseType
                then return ()
                else Left "If expression type mismatch"

            return ifTrueType

        Syntax.And left right -> do
            check names context environment left Value.Bool
            check names context environment right Value.Bool
            return Value.Bool

        Syntax.Or left right -> do
            check names context environment left Value.Bool
            check names context environment right Value.Bool
            return Value.Bool

        Syntax.Bool -> do
            return Value.Type

        Syntax.True -> do
            return Value.Bool

        Syntax.False -> do
            return Value.Bool

        Syntax.Type -> do
            return Value.Kind

        Syntax.Kind -> do
            Left "Kind has no type"

{-| Infer the type of an expression or return a `TypeError` if there is an
    internal inconsistency
-}
typeOf :: Syntax -> Either TypeError Syntax
typeOf syntax = fmap (Normalize.quote []) (infer [] [] [] syntax)

equivalent :: [Text] -> Value -> Value -> Bool
equivalent _
    (Value.Variable nameL indexL)
    (Value.Variable nameR indexR) =
        nameL == nameR && indexL == indexR
equivalent names
    (Value.Lambda inputTypeL closureL@(Closure _ nameL _))
    (Value.Lambda inputTypeR closureR) =
            equivalent names inputTypeL inputTypeR
        &&  equivalent (nameL : names) valueL valueR
  where
    variable = Normalize.fresh nameL names

    valueL = Normalize.instantiate closureL variable
    valueR = Normalize.instantiate closureR variable
equivalent names
    (Value.Forall inputTypeL closureL@(Closure _ nameL _))
    (Value.Forall inputTypeR closureR) =
            equivalent names inputTypeL inputTypeR
        &&  equivalent (nameL : names) valueL valueR
  where
    variable = Normalize.fresh nameL names

    valueL = Normalize.instantiate closureL variable
    valueR = Normalize.instantiate closureR variable
equivalent names
    (Value.Application functionL argumentL)
    (Value.Application functionR argumentR) =
            equivalent names functionL functionR
        &&  equivalent names argumentL argumentR
equivalent names
    (Value.And leftL rightL)
    (Value.And leftR rightR) =
            equivalent names leftL  leftR
        &&  equivalent names rightL rightR
equivalent names
    (Value.Or leftL rightL)
    (Value.Or leftR rightR) =
            equivalent names leftL  leftR
        &&  equivalent names rightL rightR
equivalent _ Value.Bool Value.Bool = True
equivalent _ Value.True Value.True = True
equivalent _ Value.False Value.False = True
equivalent _ Value.Type Value.Type = True
equivalent _ Value.Kind Value.Kind = True
equivalent _ _ _ = False
