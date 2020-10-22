module Grace.Syntax where

import Data.Text (Text)
import Data.String (IsString(..))

data Syntax
    = Variable Text Int
    | Lambda Text Syntax Syntax
    | Forall Text Syntax Syntax
    | Application Syntax Syntax
    | Let Text Syntax Syntax Syntax  -- TODO: Make annotation optional
    | Annotation Syntax Syntax
    | And Syntax Syntax
    | Or Syntax Syntax
    | True
    | False
    | Bool
    | Type
    | Kind
    deriving (Show)

instance IsString Syntax where
    fromString string = Variable (fromString string) 0
