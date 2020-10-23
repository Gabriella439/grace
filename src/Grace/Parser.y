{
{-# LANGUAGE QuasiQuotes #-}

{-| This module contains the logic for parsing Grace files using @happy@.

    The main reason for not using @attoparsec@ or @megaparsec@ is because
    LR parser generators are easier to maintain due to not needing to
    left-factor the grammar.

    The main reason for not using @Earley@ is performance.
-}

module Grace.Parser
    ( -- * Parsing
      parseExpression
    ) where

import Grace.Lexer (Alex, AlexPosn(..), Token)
import Grace.Syntax (Syntax)

import qualified Data.Text         as Text
import qualified Grace.Lexer       as Lexer
import qualified Grace.Syntax      as Syntax
import qualified NeatInterpolation
}

%name parseExpression
%tokentype { Token }
%error { parseError }
%lexer { lexer } { Lexer.EndOfFile }
%monad { Alex }

%token
    '&&'    { Lexer.And              }
    '->'    { Lexer.Arrow            }
    '@'     { Lexer.At               }
    Bool    { Lexer.Bool             }
    ')'     { Lexer.CloseParenthesis }
    ':'     { Lexer.Colon            }
    '='     { Lexer.Equals           }
    else    { Lexer.Else             }
    False   { Lexer.False            }
    forall  { Lexer.Forall           }
    if      { Lexer.If               }
    in      { Lexer.In               }
    int     { Lexer.Int $$           }
    Kind    { Lexer.Kind             }
    '\\'    { Lexer.Lambda           }
    let     { Lexer.Let              }
    '('     { Lexer.OpenParenthesis  }
    '||'    { Lexer.Or               }
    then    { Lexer.Then             }
    True    { Lexer.True             }
    Type_   { Lexer.Type             }
    label   { Lexer.Label $$         }

%%

Expression
    : '\\' '(' label ':' Expression ')' '->' Expression
        { Syntax.Lambda $3 $5 $8 }
    | forall '(' label ':' Expression ')' '->' Expression
        { Syntax.Forall $3 $5 $8 }
    | ApplicationExpression '->' Expression
        { Syntax.Forall "_" $1 $3 }
    | let label ':' Expression '=' Expression in Expression
        { Syntax.Let $2 (Just $4) $6 $8 }
    | let label '=' Expression in Expression
        { Syntax.Let $2 Nothing $4 $6 }
    | if Expression then Expression else Expression
        { Syntax.If $2 $4 $6 }
    | AnnotationExpression
        { $1 }

AnnotationExpression
    : OrExpression ':' AnnotationExpression
        { Syntax.Annotation $1 $3 }
    | OrExpression
        { $1 }

OrExpression
    : OrExpression '||' AndExpression
        { Syntax.Or $1 $3 }
    | AndExpression
        { $1 }

AndExpression
    : AndExpression '&&' ApplicationExpression
        { Syntax.And $1 $3 }
    | ApplicationExpression
        { $1 }

ApplicationExpression
    : ApplicationExpression PrimitiveExpression
        { Syntax.Application $1 $2 }
    | PrimitiveExpression
        { $1 }

PrimitiveExpression
    : label
        { Syntax.Variable $1 0 }
    | label '@' int
        { Syntax.Variable $1 $3 }
    | Bool
        { Syntax.Bool }
    | True
        { Syntax.True }
    | False
        { Syntax.False }
    | Type_
        { Syntax.Type }
    | Kind
        { Syntax.Kind }
    | '(' Expression ')' 
       { $2 }

{
{-| Parse a complete expression

    For simplicity, this shares the same `Alex` monad used for lexing
-}
parseExpression :: Alex Syntax

lexer :: (Token -> Alex a) -> Alex a
lexer k = Lexer.monadScan >>= k

parseError :: Token -> Alex a
parseError token = do
    (AlexPn _ line column, _, _, _) <- Lexer.alexGetInput

    let l = Text.pack (show line)
    let c = Text.pack (show column)
    let t = Text.pack (show token)

    let message =
            [NeatInterpolation.text|
            Error: Parsing failed

            ${l}:${c}: Unexpected token - ${t}
            |]

    Lexer.alexError (Text.unpack message)
}
