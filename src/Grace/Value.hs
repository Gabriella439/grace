module Grace.Value where

import Data.String (IsString(..))
import Data.Text (Text)
import Grace.Syntax (Syntax)

data Closure = Closure [(Text, Value)] Text Syntax
    deriving (Show)

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
