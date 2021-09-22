{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

{-| This module contains the logic for lexing Grace files.

    The main reason for a separate lexing step using is because we would like
    to use @Earley@ for LR parsing, but @Earley@ is not fast enough to handle
    character-by-character parsing.  Instead, we delegate lexing to a
    lower-level parsing library that supports efficient bulk parsing
    (@megaparsec@ in this case).

    The main reason for not using @alex@ is because it uses a separate code
    generation step, which leads to worse type errors and poor support for
    interactive type-checking.
-}

module Grace.Lexer
    ( -- * Lexer
      Token(..)
    , LocatedToken(..)
    , lex
    , reserved

      -- * Miscellaneous
    , validRecordLabel

      -- * Errors related to parsing
    , ParseError(..)
    ) where

import Control.Applicative (empty, (<|>))
import Control.Exception (Exception(..))
import Control.Monad.Combinators (many, manyTill, sepBy1)
import Data.HashSet (HashSet)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Scientific (Scientific)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Void (Void)
import Grace.Location (Location(..), Offset(..))
import Prelude hiding (lex)
import Text.Megaparsec (ParseErrorBundle(..), (<?>), try)

import qualified Control.Monad              as Monad
import qualified Control.Monad.Combinators  as Combinators
import qualified Data.Char                  as Char
import qualified Data.HashSet               as HashSet
import qualified Data.List                  as List
import qualified Data.Scientific            as Scientific
import qualified Data.Text                  as Text
import qualified Data.Text.Read             as Read
import qualified Grace.Location             as Location
import qualified Text.Megaparsec            as Megaparsec
import qualified Text.Megaparsec.Char       as Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Error      as Error

-- | Short-hand type synonym used by lexing utilities
type Parser = Megaparsec.Parsec Void Text

space :: Parser ()
space = Lexer.space Megaparsec.Char.space1 (Lexer.skipLineComment "#") empty

symbol :: Text -> Parser Text
symbol = Lexer.symbol space

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

parseToken :: Parser Token
parseToken =
    Combinators.choice
        [ -- `file` has to come before the lexer for `.` so that a file
          -- prefix of `.` or `..` is not lexed as a field access
          file
        , label

        , Combinators.choice
            [ Or     <$ symbol "||"
            , And    <$ symbol "&&"
            , Plus   <$ symbol "+"
            , Times  <$ symbol "*"
            ] <?> "operator"

        , Combinators.choice
            [ Exists       <$ symbol "exists"
            , Forall       <$ symbol "forall"
            , Let          <$ symbol "let"
            , In           <$ symbol "in"
            , If           <$ symbol "if"
            , Then         <$ symbol "then"
            , Else         <$ symbol "else"
            , Merge        <$ symbol "merge"
            , Type         <$ symbol "Type"
            , Fields       <$ symbol "Fields"
            , Alternatives <$ symbol "Alternatives"
            , Question     <$ symbol "?"
            ] <?> "keyword"

        , Combinators.choice
            [ RealEqual      <$ symbol "Real/equal"
            , RealLessThan   <$ symbol "Real/lessThan"
            , RealNegate     <$ symbol "Real/negate"
            , RealShow       <$ symbol "Real/show"
            , ListAny        <$ symbol "List/any"
            , ListDrop       <$ symbol "List/drop"
            , ListEqual      <$ symbol "List/equal"
            , ListFold       <$ symbol "List/fold"
            , ListHead       <$ symbol "List/head"
            , ListIndexed    <$ symbol "List/indexed"
            , ListLast       <$ symbol "List/last"
            , ListLength     <$ symbol "List/length"
            , ListMap        <$ symbol "List/map"
            , ListReverse    <$ symbol "List/reverse"
            , ListTake       <$ symbol "List/take"
            , IntegerAbs     <$ symbol "Integer/abs"
            , IntegerEven    <$ symbol "Integer/even"
            , IntegerNegate  <$ symbol "Integer/negate"
            , IntegerOdd     <$ symbol "Integer/odd"
            , JSONFold       <$ symbol "JSON/fold"
            , NaturalFold    <$ symbol "Natural/fold"
            , TextEqual      <$ symbol "Text/equal"
            , False_         <$ symbol "false"
            , True_          <$ symbol "true"
            , Null           <$ symbol "null"
            ] <?> "built-in value"

        , Combinators.choice
            [ List     <$ symbol "List"
            , Optional <$ symbol "Optional"
            , Real     <$ symbol "Real"
            , Integer  <$ symbol "Integer"
            , JSON     <$ symbol "JSON"
            , Natural  <$ symbol "Natural"
            , Bool     <$ symbol "Bool"
            , Text     <$ symbol "Text"
            ] <?> "built-in type"

        , OpenAngle        <$ symbol "<"
        , CloseAngle       <$ symbol ">"
        , OpenBrace        <$ symbol "{"
        , CloseBrace       <$ symbol "}"
        , OpenBracket      <$ symbol "["
        , CloseBracket     <$ symbol "]"
        , OpenParenthesis  <$ symbol "("
        , CloseParenthesis <$ symbol ")"

        , Arrow            <$ symbol "->"
        , At               <$ symbol "@"
        , Bar              <$ symbol "|"
        , Colon            <$ symbol ":"
        , Comma            <$ symbol ","
        , Dash             <$ symbol "-"
        , Dot              <$ symbol "."
        , Equals           <$ symbol "="
        , Lambda           <$ symbol "\\"

        , number
        , text
        , alternative
        ]

parseLocatedToken :: Parser LocatedToken
parseLocatedToken = do
    start <- fmap Offset (Megaparsec.getOffset)
    token <- parseToken
    return LocatedToken{..}

parseLocatedTokens :: Parser [LocatedToken]
parseLocatedTokens = do
    space
    manyTill parseLocatedToken Megaparsec.eof

-- | Lex a complete expression
lex :: String
    -- ^ Name of the input (used for error messages)
    -> Text
    -- ^ Source code
    -> Either ParseError [LocatedToken]
lex name code =
    case Megaparsec.parse parseLocatedTokens name code of
        Left ParseErrorBundle{..} -> do
            let bundleError :| _ = bundleErrors

            let offset = Offset (Error.errorOffset bundleError)

            Left (LexingFailed (Location{..}))
        Right tokens -> do
            return tokens

number :: Parser Token
number = do
    scientific <- lexeme Lexer.scientific

    case Scientific.toBoundedInteger scientific of
        Nothing  -> return (RealLiteral scientific)
        Just int -> return (Int int)

file :: Parser Token
file = lexeme do
    prefix <- ("../" <|> ("" <$ "./") <|> "/") <?> "path character"

    let isPath c =
                 '\x21' == c
            ||  ('\x24' <= c && c <= '\x27')
            ||  ('\x2A' <= c && c <= '\x2B')
            ||  ('\x2D' <= c && c <= '\x2E')
            ||  ('\x30' <= c && c <= '\x3B')
            ||   '\x3D' == c
            ||  ('\x40' <= c && c <= '\x5A')
            ||  ('\x5E' <= c && c <= '\x7A')
            ||  ('\x7C' == c)
            ||   '\x7E' == c

    let pathComponent = Megaparsec.takeWhileP (Just "path character") isPath

    suffix <- pathComponent `sepBy1` "/"

    return (File (concat (map Text.unpack (prefix : List.intersperse "/" suffix))))

text :: Parser Token
text = lexeme do
    "\""

    let isText c =
                ('\x20' <= c && c <=     '\x21')
            ||  ('\x23' <= c && c <=     '\x5b')
            ||  ('\x5d' <= c && c <= '\x10FFFF')

    let unescaped = Megaparsec.takeWhile1P (Just "text character") isText

    let unicodeEscape = do
            "\\u"

            codepoint <- Combinators.count 4 Megaparsec.Char.hexDigitChar

            case Read.hexadecimal (Text.pack codepoint) of
                Right (n, "") -> do
                    return (Text.singleton (Char.chr n))
                _             -> do
                    fail [__i|
                    Internal error - invalid unicode escape sequence
                    |]

    let escaped =
            Combinators.choice
                [ "\"" <$ "\\\""
                , "\\" <$ "\\\\"
                , "/"  <$ "\\/"
                , "\b" <$ "\\b"
                , "\f" <$ "\\f"
                , "\n" <$ "\\n"
                , "\r" <$ "\\r"
                , "\t" <$ "\\t"
                , unicodeEscape
                ] <?> "escape sequence"

    texts <- many (unescaped <|> escaped)

    "\""

    return (TextLiteral (Text.concat texts))

isLabel :: Char -> Bool
isLabel c = Char.isAlphaNum c || c == '_' || c == '-' || c == '/'

-- | Returns `True` if the given label is valid
validRecordLabel :: Text -> Bool
validRecordLabel text_  =
    case Text.uncons text_ of
        Nothing     -> False
        Just (h, t) ->
                (Char.isAlpha h || h == '_')
            &&  Text.all isLabel t
            &&  not (HashSet.member text_ reserved)

-- | Reserved tokens, which can't be used for labels unless they are quoted
reserved :: HashSet Text
reserved =
    HashSet.fromList
        [ "Alternatives"
        , "Bool"
        , "Real"
        , "Real/equal"
        , "Real/lessThan"
        , "Real/negate"
        , "Real/show"
        , "Fields"
        , "Integer"
        , "Integer/abs"
        , "Integer/even"
        , "Integer/negate"
        , "Integer/odd"
        , "JSON/fold"
        , "List"
        , "List/drop"
        , "List/equal"
        , "List/fold"
        , "List/indexed"
        , "List/last"
        , "List/length"
        , "List/map"
        , "List/reverse"
        , "List/take"
        , "Natural"
        , "Natural/fold"
        , "Optional"
        , "Text"
        , "Text/equal"
        , "Type"
        , "else"
        , "exists"
        , "false"
        , "forall"
        , "if"
        , "in"
        , "let"
        , "merge"
        , "null"
        , "then"
        , "true"
        ]

label :: Parser Token
label = (lexeme . try) do
    let isLabel0 c = Char.isLower c || c == '_'

    c0 <- Megaparsec.satisfy isLabel0 <?> "label character"

    cs <- Megaparsec.takeWhileP (Just "label character") isLabel

    let result = Text.cons c0 cs

    Monad.guard (not (HashSet.member result reserved))

    return (Label result)

alternative :: Parser Token
alternative = lexeme do
    c0 <- Megaparsec.satisfy Char.isUpper <?> "alternative character"

    cs <- Megaparsec.takeWhileP (Just "alternative character") isLabel

    return (Alternative (Text.cons c0 cs))

-- | Tokens produced by lexing
data Token
    = Alternative Text
    | Alternatives
    | And
    | Append
    | Arrow
    | At
    | Bar
    | Bool
    | CloseAngle
    | CloseBrace
    | CloseBracket
    | CloseParenthesis
    | Colon
    | Comma
    | Dash
    | Dot
    | Real
    | RealEqual
    | RealLessThan
    | RealLiteral Scientific
    | RealNegate
    | RealShow
    | Else
    | Equals
    | Exists
    | False_
    | Fields
    | File FilePath
    | Forall
    | If
    | In
    | Int Int
    | Integer
    | IntegerAbs
    | IntegerEven
    | IntegerNegate
    | IntegerOdd
    | JSON
    | JSONFold
    | Label Text
    | Lambda
    | Let
    | List
    | ListAny
    | ListDrop
    | ListEqual
    | ListFold
    | ListHead
    | ListIndexed
    | ListLast
    | ListLength
    | ListMap
    | ListReverse
    | ListTake
    | Merge
    | Natural
    | NaturalFold
    | Null
    | OpenAngle
    | OpenBrace
    | OpenBracket
    | OpenParenthesis
    | Optional
    | Or
    | Plus
    | Question
    | Text
    | TextEqual
    | TextLiteral Text
    | Then
    | Times
    | True_
    | Type
    deriving stock (Eq, Show)

{-| A token with offset information attached, used for reporting line and
    column numbers in error messages
-}
data LocatedToken = LocatedToken { token :: Token, start :: Offset }
    deriving (Show)

-- | Errors related to lexing and parsing
data ParseError
    = LexingFailed Location
    | ParsingFailed Location
    deriving (Eq, Show)

instance Exception ParseError where
    displayException (LexingFailed location) = Text.unpack
        (Location.renderError "Invalid input - Lexing failed" location)
    displayException (ParsingFailed location) = Text.unpack
        (Location.renderError "Invalid input - Parsing failed" location)
