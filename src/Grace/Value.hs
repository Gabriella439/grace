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
    | Lambda Closure
    | If Value Value Value
    | Application Value Value
    | List [Value]
    | And Value Value
    | Or Value Value
    | True
    | False
    deriving (Show)

instance IsString Value where
    fromString string = Variable (fromString string) 0
