{-| This module contains the syntax tree used for the surface syntax (i.e. the
    result of parsing), representing the code as the user wrote it.
-}

module Grace.Syntax
    ( -- * Syntax
      Syntax(..)
    ) where

import Data.Text (Text)
import Data.String (IsString(..))

-- | The surface syntax for the language
data Syntax
    = Variable Text Int
    | Lambda Text Syntax Syntax
    | Forall Text Syntax Syntax
    | Application Syntax Syntax
    | Let Text (Maybe Syntax) Syntax Syntax
    | If Syntax Syntax Syntax
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
