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

{-| A `Closure` captures the evaluation environment in order to defer
    substitution to a later point

    This provides efficiency comparable to a higher-order abstract syntax
    tree, except using a first-order representation.
-}
data Closure = Closure [(Text, Value)] Text Syntax
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
    | Lambda Value Closure
    | Forall Value Closure
    | Application Value Value
    | And Value Value
    | Or Value Value
    | True
    | False
    | Bool
    | Type
    | Kind
    deriving (Show)

instance IsString Value where
    fromString string = Variable (fromString string) 0
