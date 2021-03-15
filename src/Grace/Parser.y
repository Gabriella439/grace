{
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

import Data.String.Interpolate (__i)
import Grace.Lexer (Alex, AlexPosn(..), Token)
import Grace.Syntax (Syntax)

import qualified Data.Text         as Text
import qualified Grace.Lexer       as Lexer
import qualified Grace.Syntax      as Syntax
import qualified Grace.Type        as Type
}

%name parseExpression
%tokentype { Token }
%error { parseError }
%lexer { lexer } { Lexer.EndOfFile }
%monad { Alex }

%token
    '&&'           { Lexer.And              }
    '->'           { Lexer.Arrow            }
    '@'            { Lexer.At               }
    Bool           { Lexer.Bool             }
    '}'            { Lexer.CloseBrace       }
    ']'            { Lexer.CloseBracket     }
    ')'            { Lexer.CloseParenthesis }
    ':'            { Lexer.Colon            }
    ','            { Lexer.Comma            }
    '.'            { Lexer.Dot              }
    '='            { Lexer.Equals           }
    else           { Lexer.Else             }
    forall         { Lexer.Forall           }
    False          { Lexer.False_           }
    if             { Lexer.If               }
    in             { Lexer.In               }
    int            { Lexer.Int $$           }
    '\\'           { Lexer.Lambda           }
    let            { Lexer.Let              }
    Natural        { Lexer.Natural          }
    'Natural/fold' { Lexer.NaturalFold      }
    '{'            { Lexer.OpenBrace        }
    '['            { Lexer.OpenBracket      }
    '('            { Lexer.OpenParenthesis  }
    '||'           { Lexer.Or               }
    '+'            { Lexer.Plus             }
    then           { Lexer.Then             }
    '*'            { Lexer.Times            }
    True           { Lexer.True_            }
    label          { Lexer.Label $$         }

%%

Expression
    : '\\' label '->' Expression
        { Syntax.Lambda $2 $4 }
    | let label '=' Expression in Expression
        { Syntax.Let $2 Nothing $4 $6 }
    | let label ':' Type '=' Expression in Expression
        { Syntax.Let $2 (Just $4) $6 $8 }
    | if Expression then Expression else Expression
        { Syntax.If $2 $4 $6 }
    | TimesExpression ':' Type
        { Syntax.Annotation $1 $3 }
    | TimesExpression
        { $1 }

TimesExpression
    : TimesExpression '*' PlusExpression
        { Syntax.Times $1 $3 }
    | PlusExpression
        { $1 }

PlusExpression
    : PlusExpression '+' OrExpression
        { Syntax.Plus $1 $3 }
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
    : ApplicationExpression FieldExpression
        { Syntax.Application $1 $2 }
    | FieldExpression
        { $1 }

FieldExpression
    : FieldExpression '.' label
        { Syntax.Field $1 $3 }
    | PrimitiveExpression
        { $1 }

PrimitiveExpression
    : label
        { Syntax.Variable $1 0 }
    | label '@' int
        { Syntax.Variable $1 $3 }
    | '[' List ']'
        { Syntax.List $2 }
    | '{' Record '}'
        { Syntax.Record $2 }
    | True
        { Syntax.True }
    | False
        { Syntax.False }
    | int
        { Syntax.Natural (fromIntegral $1) }
    | 'Natural/fold'
        { Syntax.NaturalFold }
    | '(' Expression ')' 
       { $2 }

List
   : ReversedList
       { reverse $1 }
   | {- empty -}
       { [] }

ReversedList
    : ReversedList ',' Expression
       { $3 : $1 }
    | Expression
       { [ $1 ] }

Record
    : ReversedRecord
        { reverse $1 }
    | {- empty -}
        { [] }

ReversedRecord
    : ReversedRecord ',' label '=' Expression
        { ($3, $5) : $1 }
    | label '=' Expression
        { [ ($1, $3) ] }

Type
    : forall label '.' Type
        { Type.Forall $2 $4 }
    | FunctionType
        { $1 }

FunctionType
    : PrimitiveType '->' FunctionType
        { Type.Function $1 $3 }
    | PrimitiveType
        { $1 }

PrimitiveType
    : Bool
        { Type.Bool }
    | Natural
        { Type.Natural }
    | label
        { Type.Variable $1 }
    | '{' RecordType '}'
        { Type.Record (Type.Fields $2 Nothing) }
    | '(' Type ')'
        { $2 }

RecordType
    : ReversedRecordType
        { reverse $1 }
    | {- empty -}
        { [] }

ReversedRecordType
    : ReversedRecordType ',' label ':' Type
        { ($3, $5) : $1 }
    | label ':' Type
        { [ ($1, $3) ] }

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

    let message =
            [__i|
            Parsing failed

            #{show line}:#{show column}: Unexpected token - #{show token}
            |]

    Lexer.alexError (Text.unpack message)
}
