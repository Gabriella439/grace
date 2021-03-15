{
{-| This module contains the logic for lexing Grace files using @alex@.  This
    uses a small variation on the @monad-bytestring@ wrapper to efficiently
    lex tokens while producing customizable error messages.

    The main reason for a separate lexing step using @alex@ is to take advantage
    of the lexical analyzer's support for the longest match rule, which
    makes the lexing logic easier to maintain.
-}

module Grace.Lexer
    ( -- * Lexer
      Alex
    , AlexPosn(..)
    , Token(..)
    , alexError
    , alexGetInput
    , monadScan
    , runAlex
    ) where

import Data.String.Interpolate (__i)
import Data.Text (Text)

import qualified Data.ByteString              as ByteString.Strict
import qualified Data.ByteString.Lazy         as ByteString.Lazy
import qualified Data.ByteString.Lex.Integral as ByteString.Integral
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as Text.Lazy
import qualified Data.Text.Lazy.Encoding      as Text.Lazy.Encoding
}

%wrapper "monad-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]

token :-
  $white+                             ;
  "#".*                               ;
  \&\&                                { emit And                            }
  \-\>                                { emit Arrow                          }
  @                                   { emit At                             }
  Bool                                { emit Grace.Lexer.Bool               }
  \}                                  { emit CloseBrace                     }
  \]                                  { emit CloseBracket                   }
  \)                                  { emit CloseParenthesis               }
  :                                   { emit Colon                          }
  \,                                  { emit Comma                          }
  \.                                  { emit Dot                            }
  \=                                  { emit Equals                         }
  else                                { emit Else                           }
  forall                              { emit Forall                         }
  False                               { emit Grace.Lexer.False              }
  if                                  { emit If                             }
  in                                  { emit In                             }
  $digit+                             { \i n -> fmap Int (captureInt i n)   }
  \\                                  { emit Lambda                         }
  let                                 { emit Let                            }
  \{                                  { emit OpenBrace                      }
  \[                                  { emit OpenBracket                    }
  \(                                  { emit OpenParenthesis                }
  \|\|                                { emit Or                             }
  \+                                  { emit Plus                           }
  then                                { emit Then                           }
  \*                                  { emit Times                          }
  True                                { emit Grace.Lexer.True               }
  [ $alpha \_ ] [ $alpha $digit \_ ]* { capture Label                       }

{
emit :: Token -> AlexAction Token
emit t _ _ = return t

getLazyBytes :: AlexAction ByteString.Lazy.ByteString
getLazyBytes (_, _, remainder, _) len =
    return (ByteString.Lazy.take len remainder)

getBytes :: AlexAction ByteString.Strict.ByteString
getBytes input len =
    fmap ByteString.Lazy.toStrict (getLazyBytes input len)

getText :: AlexAction Text
getText input len = do
    lazyBytes <- getLazyBytes input len

    lazyText <- case Text.Lazy.Encoding.decodeUtf8' lazyBytes of
        Left exception -> alexError (show exception)
        Right lazyText -> return lazyText

    return (Text.Lazy.toStrict lazyText)

capture :: (Text -> Token) -> AlexAction Token
capture f input len = fmap f (getText input len)

captureInt :: AlexAction Int
captureInt input len = do
    bytes <- getBytes input len

    case ByteString.Integral.readDecimal bytes of
        Nothing     -> alexError "Invalid integer"
        Just (n, _) -> return n

alexEOF :: Alex Token
alexEOF = return EndOfFile

-- | Same as `alexMonadScan`, except with a better error message
monadScan :: Alex Token
monadScan = do
  input@(_,_,_,n) <- alexGetInput

  sc <- alexGetStartCode

  case alexScan input sc of
    AlexEOF ->
        alexEOF

    AlexError ((AlexPn _ line column),char,_,_) -> do
        let message =
                [__i|
                Lexing failed

                #{show line}:#{show column}: Unexpected character #{show char}
                |]

        alexError (Text.unpack message)

    AlexSkip input' _ -> do
        alexSetInput input'

        monadScan

    AlexToken input'@(_,_,_,n') _ action -> do
        let len = n' - n

        alexSetInput input'

        action (ignorePendingBytes input) len

-- | Tokens produced by lexing
data Token
    = And
    | Arrow
    | At
    | Bool
    | CloseBrace
    | CloseBracket
    | CloseParenthesis
    | Colon
    | Comma
    | Dot
    | Else
    | Equals
    | False
    | Forall
    | If
    | In
    | Int Int
    | Label Text
    | Lambda
    | Let
    | OpenBrace
    | OpenBracket
    | OpenParenthesis
    | Or
    | Plus
    | Then
    | Times
    | True
    | EndOfFile
    deriving (Show)
}
