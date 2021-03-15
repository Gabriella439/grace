{-| This module contains the `Value` type used internally for efficient
    evaluation of expressions
-}
module Grace.Value
    ( -- * Value
      Closure(..)
    , Value(..)
    ) where

import Data.String (IsString(..))
import Data.Text (Text)
import Numeric.Natural (Natural)
import Grace.Syntax (Syntax)

{-| A `Closure` captures the current evaluation environment in order to defer
    evaluation until the value of some bound variable is known

    You can think of @Closure name env expression@ as essentially the same thing
    as @\\value -> evaluate ((name, value) : env) e@, except stored using a
    first-order representation.  In fact, you convert to the latter
    representation using `Grace.Normalize.instantiate`.

    This provides efficiency comparable to a higher-order abstract syntax
    tree, except using a first-order representation.
-}
data Closure = Closure Text [(Text, Value)] Syntax
    deriving (Show)

{-| This type represents a fully evaluated expression with no reducible
    sub-expressions

    There are two benefits to using a type separate from the surface syntax for
    this purpose:

    * To avoid wastefully reducing the same sub-expression multiple times

    * To use a more efficient representation for reduction purposes
-}
data Value
    = Variable Text Int
      -- ^ The `Text` field is the variable's name (i.e. \"x\")
      --
      -- The `Int` field disambiguates variables with the same name if there are
      -- multiple bound variables of the same name in scope.  Zero refers to the
      -- nearest bound variable and the index increases by one for each bound
      -- variable of the same name going outward.  The following diagram may
      -- help:
      --
      -- >              ┌──refers to──┐
      -- >              │             │
      -- >              v             │
      -- > \x -> \y -> \x     ->    x@0
      -- > 
      -- > ┌─────────refers to────────┐
      -- > │                          │
      -- > v                          │
      -- > \x -> \y -> \x     ->    x@1
      --
      -- This `Int` behaves like a De Bruijn index in the spcial case where all
      -- variables have the same name.
      --
      -- You can optionally omit the index if it is 0:
      --
      -- >              ┌─refers to─┐
      -- >              │           │
      -- >              v           │
      -- > \x -> \y -> \x     ->    x
      --
      -- Zero indices are omitted when pretty-printing variables and non-zero
      -- indices appear as a numeric suffix.
    | Lambda Closure
      -- The `Lambda` constructor captures the environment at the time it is
      -- evaluated, so that evaluation can be lazily deferred until the function
      -- input is known.  This is essentially the key optimization that powers
      -- the fast normalization-by-evaluation algorithm.
    | Application Value Value
    | List [Value]
    | Record [(Text, Value)]
    | Field Value Text
    | True
    | False
    | And Value Value
    | Or Value Value
    | If Value Value Value
    | Natural Natural
    | Times Value Value
    | Plus Value Value
    deriving (Show)

instance IsString Value where
    fromString string = Variable (fromString string) 0
