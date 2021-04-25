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
import qualified Data.ByteString.Lazy.Char8   as ByteString.Lazy.Char8
import qualified Data.ByteString.Lex.Integral as ByteString.Integral
import qualified Data.Char                    as Char
import qualified Data.Text                    as Text
import qualified Data.Text.Read               as Text.Read
import qualified Data.Text.Lazy               as Text.Lazy
import qualified Data.Text.Lazy.Encoding      as Text.Lazy.Encoding
}

%wrapper "monad-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]
$lower = [a-z]
$upper = [A-Z]
$pathchar = [\x21\x24-\x27\x2A-\x2B\x2D-\x2E\x30-\x3B\x3D\x40-\x5A\x5E-\x7A\x7C\x7E]
$stringchar = [\x20-\x21\x23-\x5b\x5d-\x10FFFF]
$hex = [a-fA-F0-9]

token :-
  $white+                             ;
  "#".*                               ;
  \&\&                                { emit And                            }
  \-\>                                { emit Arrow                          }
  @                                   { emit At                             }
  Bool                                { emit Grace.Lexer.Bool               }
  \>                                  { emit CloseAngle                     }
  \}                                  { emit CloseBrace                     }
  \]                                  { emit CloseBracket                   }
  \)                                  { emit CloseParenthesis               }
  :                                   { emit Colon                          }
  \,                                  { emit Comma                          }
  \.                                  { emit Dot                            }
  \=                                  { emit Equals                         }
  else                                { emit Else                           }
  forall                              { emit Forall                         }
  False                               { emit False_                         }
  if                                  { emit If                             }
  in                                  { emit In                             }
  \\                                  { emit Lambda                         }
  let                                 { emit Let                            }
  List                                { emit List                           }
  merge                               { emit Merge                          }
  Natural                             { emit Natural                        }
  Natural\/fold                       { emit NaturalFold                    }
  \<                                  { emit OpenAngle                      }
  \{                                  { emit OpenBrace                      }
  \[                                  { emit OpenBracket                    }
  \(                                  { emit OpenParenthesis                }
  \|\|                                { emit Or                             }
  \+                                  { emit Plus                           }
  \+\+                                { emit Append                         }
  then                                { emit Then                           }
  \*                                  { emit Times                          }
  Text                                { emit Text                           }
  True                                { emit True_                          }
  $digit+
    { \i n -> fmap Int (captureInt i n)   }
  (\.|\.\.|())(\/$pathchar+)+
    { capture (File . Text.unpack)        }
  \"( $stringchar | \\ ([\"\\\/bfnrt] | u ($hex{4})) )*\"
    { \i n -> fmap TextLiteral (captureText i n) }
  [ $lower \_ ] [ $alpha $digit \_ ]*
    { capture Label                       }
    $upper      [ $alpha $digit \_ ]*
    { capture Alternative                 }

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

captureText :: AlexAction Text
captureText input len = do
    text <- getText input len

    return (unescape text)

unescape :: Text -> Text
unescape =
      Text.intercalate "\\"
    . map unescapeInner
    . Text.splitOn "\\\\"
    . Text.init
    . Text.tail
  where
    unescapeInner =
          unescapeUnicode
        . Text.replace "\\b" "\b"
        . Text.replace "\\f" "\f"
        . Text.replace "\\n" "\n"
        . Text.replace "\\r" "\r"
        . Text.replace "\\t" "\t"

    unescapeUnicode text = Text.concat (x0 : ys)
      where
        x0 : xs = Text.splitOn "\\u" text

        ys = do
            x <- xs

            let (prefix, suffix) = Text.splitAt 4 x

            if Text.length prefix == 4
                then do
                    case Text.Read.hexadecimal prefix of
                        Right (n, "") -> do
                            return (Text.singleton (Char.chr n) <> suffix)
                        _             -> do
                            return ("\\u" <> x)
                else do
                    return ("\\u" <> x)

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

    AlexError ((AlexPn _ line column),_,current,_) -> do
        case ByteString.Lazy.Char8.uncons current of
            Just (char, _) -> do
                let message =
                        [__i|
                        Lexing failed

                        #{show line}:#{show column}: Unexpected character #{show char}
                        |]

                alexError (Text.unpack message)
            Nothing -> do
                let message =
                        [__i|
                        Lexing failed

                        #{show line}:#{show column}: Unexpected end of file
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
    | Append
    | At
    | Bool
    | CloseAngle
    | CloseBrace
    | CloseBracket
    | CloseParenthesis
    | Colon
    | Comma
    | Alternative Text
    | Dot
    | Else
    | Equals
    | False_
    | File FilePath
    | Forall
    | If
    | In
    | Int Int
    | Label Text
    | Lambda
    | Let
    | List
    | Merge
    | Natural
    | NaturalFold
    | OpenAngle
    | OpenBrace
    | OpenBracket
    | OpenParenthesis
    | Or
    | Plus
    | Text
    | TextLiteral Text
    | Then
    | Times
    | True_
    | EndOfFile
    deriving (Show)
}
