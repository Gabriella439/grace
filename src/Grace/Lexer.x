{
module Grace.Lexer where

import Data.Text (Text)

import qualified Data.Text as Text
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

token :-
  $white+                             ;
  "#".*                               ;
  \&\&                                { \_ -> And                 }
  \-\>                                { \_ -> Arrow               }
  @                                   { \_ -> At                  }
  Bool                                { \_ -> Bool                }
  \)                                  { \_ -> CloseParenthesis    }
  \:                                  { \_ -> Colon               }
  \=                                  { \_ -> Equals              }
  False                               { \_ -> Grace.Lexer.False   }
  forall                              { \_ -> Forall              }
  in                                  { \_ -> In                  }
  $digit+                             { \s -> Int (read s)        }
  Kind                                { \_ -> Kind                }
  \\                                  { \_ -> Lambda              }
  let                                 { \_ -> Let                 }
  \(                                  { \_ -> OpenParenthesis     }
  \(                                  { \_ -> Or                  }
  True                                { \_ -> Grace.Lexer.True    }
  Type                                { \_ -> Type                }
  [ $alpha \_ ] [ $alpha $digit \_ ]* { \s -> Label (Text.pack s) }

{
data Token
    = And
    | Arrow
    | At
    | Bool
    | CloseParenthesis
    | Colon
    | Equals
    | False
    | Forall
    | In
    | Int Int
    | Kind
    | Label Text
    | Lambda
    | Let
    | OpenParenthesis
    | Or
    | True
    | Type
    deriving (Show)
}
