{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo      #-}

{-| This module contains the logic for lexing and parsing Grace files

    The main reason for a separate lexing step using is because we would like
    to use @Earley@ for LR parsing, but @Earley@ is not fast enough to handle
    character-by-character parsing.  Instead, we delegate lexing to a
    lower-level parsing library that supports efficient bulk parsing
    (@megaparsec@ in this case).

    The main reason for not using @alex@ (for lexing) or @happy@ (for parsing)
    is because they use a separate code generation step, which leads to worse
    type errors and poor support for interactive type-checking.

    The main reason for not using @attoparsec@ or @megaparsec@ for everything
    is because LR parsers are easier to maintain due to not needing to
    left-factor the grammar.
-}

module Grace.Parser
    ( -- * Parsing
      parse
      -- * Errors related to parsing
    , ParseError(..)
      -- * Utilities
    , reserved
    , validLabel
    , validRecordLabel
    , validAlternativeLabel
    ) where

import Control.Applicative (empty, many, optional, some, (<|>))
import Control.Applicative.Combinators (endBy, sepBy)
import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Control.Exception.Safe (Exception(..))
import Control.Monad.Combinators (manyTill)
import Data.Functor (void)
import Data.Foldable (toList)
import Data.HashSet (HashSet)
import Data.List.NonEmpty (NonEmpty(..), some1)
import Data.Maybe (fromJust)
import Data.Scientific (Scientific)
import Data.Semigroup (sconcat)
import Data.Text (Text)
import Data.Void (Void)
import Grace.Input (Input(..), Mode(..))
import Grace.Location (Location(..), Offset(..))
import Grace.Type (Type(..))
import Numeric.Natural (Natural)
import Prelude hiding (lex, lines, unlines)
import Text.Earley (Grammar, Prod, Report(..), rule, (<?>))
import Text.Megaparsec (ParseErrorBundle(..), State(..), try)

import Grace.Syntax
    ( Assignment(..)
    , Binding(..)
    , BindMonad(..)
    , Chunks(..)
    , Field(..)
    , NameBinding(..)
    , Smaller(..)
    , Syntax(..)
    )

import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Data.Char as Char
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import qualified Grace.Domain as Domain
import qualified Grace.Location as Location
import qualified Grace.Monotype as Monotype
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type
import qualified Text.Earley as Earley
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Error as Error
import qualified Text.URI as URI

-- | Short-hand type synonym used by lexing utilities
type Lexer = Megaparsec.Parsec Void Text

space :: Lexer ()
space = Lexer.space Megaparsec.Char.space1 (Lexer.skipLineComment "#") empty

symbol :: Text -> Lexer Text
symbol = Lexer.symbol space

lexeme :: Lexer a -> Lexer a
lexeme = Lexer.lexeme space

lexToken :: Lexer Token
lexToken =
    Combinators.choice
        [ -- `file` has to come before the lexer for `.` so that a file
          -- prefix of `.` or `..` is not lexed as a field access
          lexFile
        , lexUri
        , lexLabel
        , lexNumber
        , lexDotNumber

        , Combinators.choice
            [ Grace.Parser.Or                 <$ symbol "||"
            , Grace.Parser.And                <$ symbol "&&"
            , Grace.Parser.Plus               <$ symbol "+"
            , Grace.Parser.Times              <$ symbol "*"
            , Grace.Parser.Modulus            <$ symbol "%"
            , Grace.Parser.ForwardSlash       <$ symbol "/"
            , Grace.Parser.DoubleEquals       <$ symbol "=="
            , Grace.Parser.NotEqual           <$ symbol "!="
            , Grace.Parser.LessThanOrEqual    <$ symbol "<="
            , Grace.Parser.GreaterThanOrEqual <$ symbol ">="
            ] Megaparsec.<?> "operator"

        , Combinators.choice
            [ Grace.Parser.Abs            <$ symbol "abs"
            , Grace.Parser.False_         <$ symbol "false"
            , Grace.Parser.Indexed        <$ symbol "indexed"
            , Grace.Parser.Length         <$ symbol "length"
            , Grace.Parser.Map            <$ symbol "map"
            , Grace.Parser.Null           <$ symbol "null"
            , Grace.Parser.Reveal         <$ symbol "reveal"
            , Grace.Parser.Show           <$ symbol "show"
            , Grace.Parser.Some           <$ symbol "some"
            , Grace.Parser.True_          <$ symbol "true"
            , Grace.Parser.YAML           <$ symbol "yaml"
            ] Megaparsec.<?> "built-in value"

        , Combinators.choice
            [ Grace.Parser.Else         <$ symbol "else"
            , Grace.Parser.Forall       <$ symbol "forall"
            , Grace.Parser.Fold         <$ symbol "fold"
            , Grace.Parser.For          <$ symbol "for"
            , Grace.Parser.GitHub       <$ symbol "github"
            , Grace.Parser.HTTP         <$ symbol "http"
            , Grace.Parser.Read         <$ symbol "read"
            , Grace.Parser.If           <$ symbol "if"
            , Grace.Parser.Import       <$ symbol "import"
            , Grace.Parser.In           <$ symbol "in"
            , Grace.Parser.Let          <$ symbol "let"
            , Grace.Parser.Of           <$ symbol "of"
            , Grace.Parser.Prompt       <$ symbol "prompt"
            , Grace.Parser.Then         <$ symbol "then"
            , Grace.Parser.Alternatives <$ symbol "Alternatives"
            , Grace.Parser.Fields       <$ symbol "Fields"
            , Grace.Parser.Type         <$ symbol "Type"
            ] Megaparsec.<?> "keyword"

        , Combinators.choice
            [ Grace.Parser.List     <$ symbol "List"
            , Grace.Parser.Optional <$ symbol "Optional"
            , Grace.Parser.Real     <$ symbol "Real"
            , Grace.Parser.Integer  <$ symbol "Integer"
            , Grace.Parser.JSON     <$ symbol "JSON"
            , Grace.Parser.Key      <$ symbol "Key"
            , Grace.Parser.Natural  <$ symbol "Natural"
            , Grace.Parser.Bool     <$ symbol "Bool"
            , Grace.Parser.Text     <$ symbol "Text"
            ] Megaparsec.<?> "built-in type"

        , Grace.Parser.OpenAngle        <$ symbol "<"
        , Grace.Parser.CloseAngle       <$ symbol ">"
        , Grace.Parser.OpenBrace        <$ symbol "{"
        , Grace.Parser.CloseBrace       <$ symbol "}"
        , Grace.Parser.OpenBracket      <$ symbol "["
        , Grace.Parser.CloseBracket     <$ symbol "]"
        , Grace.Parser.OpenParenthesis  <$ symbol "("
        , Grace.Parser.CloseParenthesis <$ symbol ")"

        , Grace.Parser.Arrow            <$ symbol "->"
        , Grace.Parser.At               <$ symbol "@"
        , Grace.Parser.Bar              <$ symbol "|"
        , Grace.Parser.Colon            <$ symbol ":"
        , Grace.Parser.Comma            <$ symbol ","
        , Grace.Parser.Dash             <$ symbol "-"
        , Grace.Parser.Dot              <$ symbol "."
        , Grace.Parser.Equals           <$ symbol "="
        , Grace.Parser.Lambda           <$ symbol "\\"

        , lexText
        , lexAlternative
        , lexQuotedAlternative
        ]

lexLocatedToken :: Lexer LocatedToken
lexLocatedToken = do
    state <- Megaparsec.getParserState
    token <- lexToken
    return LocatedToken{ token, state }

lexLocatedTokens :: Lexer [LocatedToken]
lexLocatedTokens = do
    space
    manyTill lexLocatedToken Megaparsec.eof

-- | Lex a complete expression
lex :: String
    -- ^ Name of the input (used for error messages)
    -> Text
    -- ^ Source code
    -> Either ParseError [LocatedToken]
lex name code =
    case Megaparsec.parse lexLocatedTokens name code of
        Left ParseErrorBundle{ bundleErrors } -> do
            let bundleError :| _ = bundleErrors

            let offset = Offset (Error.errorOffset bundleError)

            Left (LexingFailed (Location{ name, code, offset }))
        Right tokens -> do
            return tokens

lexSign :: Lexer Sign
lexSign = (Positive <$ "+") <|> (Negative <$ "-") <|> pure Unsigned

lexNumber :: Lexer Token
lexNumber = try lexInteger <|> try lexScientific
  where
    lexInteger = do
        sign <- lexSign
        n <- lexeme Lexer.decimal <* Megaparsec.notFollowedBy (Megaparsec.Char.char '.')
        return (Int sign n)

    lexScientific = do
        sign <- lexSign
        scientific <- lexeme Lexer.scientific
        return (RealLiteral sign scientific)

lexDotNumber :: Lexer Token
lexDotNumber = try do
    symbol "."

    sign <- lexSign

    n <- lexeme Lexer.decimal

    return case sign of
        Unsigned -> DotNumber (fromInteger n)
        Positive -> DotNumber (fromInteger n)
        Negative -> DotNumber (negate (fromInteger n))

lexFile :: Lexer Token
lexFile = (lexeme . try) do
    prefix <- ("../" <|> "./" <|> "/") Megaparsec.<?> "path character"

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

    let pathComponent = Megaparsec.takeWhile1P (Just "path character") isPath

    suffix <- pathComponent `sepBy1` "/"

    return (File (concatMap Text.unpack (prefix : List.intersperse "/" (toList suffix))))

lexUri :: Lexer Token
lexUri = (lexeme . try) do
    u <- URI.parser

    let schemes =
            map (fromJust . URI.mkScheme) [ "https", "http", "env", "file" ]

    if any (`elem` schemes) (URI.uriScheme u)
        then return (Grace.Parser.URI u)
        else fail "Unsupported Grace URI"

lines :: Chunks s a -> NonEmpty (Chunks s a)
lines = loop mempty
  where
    loop :: Chunks s a -> Chunks s a -> NonEmpty (Chunks s a)
    loop currentLine (Chunks text₀ rest)
        | Text.null suffix = case rest of
            [] -> (currentLine <> Chunks prefix []) :| []
            (interpolation, text₁) : est ->
                loop (currentLine <> Chunks prefix [(interpolation, "")])
                    (Chunks text₁ est)
        | otherwise =
            NonEmpty.cons
                (currentLine <> Chunks prefix [])
                (loop mempty (Chunks (Text.drop 1 suffix) rest))
      where
        (prefix, suffix) = Text.breakOn "\n" text₀

unlines :: NonEmpty (Chunks s a) -> Chunks s a
unlines ls = sconcat (NonEmpty.intersperse "\n" ls)

commonPrefix :: NonEmpty (Chunks s a) -> Text
commonPrefix ls = List.foldl' longestCommonPrefix t ts
  where
    t :| ts = fmap toPrefix (removeEmpty ls)

    toPrefix (Chunks text₀ _) = Text.takeWhile isPrefixCharacter text₀
      where
        isPrefixCharacter c = c == ' ' || c == '\t'

    longestCommonPrefix x y = case Text.commonPrefixes x y of
        Nothing             -> ""
        Just (prefix, _, _) -> prefix

removeEmpty :: NonEmpty (Chunks s a) -> NonEmpty (Chunks s a)
removeEmpty ls = prependList (filter present initLines) (pure lastLine)
  where
    initLines = NonEmpty.init ls
    lastLine  = NonEmpty.last ls

    present (Chunks "" []) = False
    present  _             = True

prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList      []        ys  = ys
prependList (x : xs) (y :| ys) = x :| (xs <> (y : ys))

dedent :: Chunks s a -> Chunks s a
dedent c = unlines (fmap stripPrefix ls)
  where
    ls = lines c

    prefix = commonPrefix ls

    stripPrefix (Chunks text₀ rest) =
        Chunks (Text.drop (Text.length prefix) text₀) rest

lexText :: Lexer Token
lexText = lexeme do
    "\""

    multiline <- (True <$ "\n") <|> pure False

    let isText c =
                ('\x09' <= c && c <=     '\x0A' && multiline)
            ||  ('\x20' <= c && c <=     '\x21')
            ||   '\x23' == c
            ||  ('\x25' <= c && c <=     '\x5b')
            ||  ('\x5d' <= c && c <= '\x10FFFF')

    let unescaped = do
            t <- Megaparsec.takeWhile1P (Just "text character") isText

            return (Chunks t [])

    let unicodeEscape = do
            "\\u"

            codepoint <- Combinators.count 4 Megaparsec.Char.hexDigitChar

            case Read.hexadecimal (Text.pack codepoint) of
                Right (n, "") -> do
                    return (Chunks (Text.singleton (Char.chr n)) [])
                _             -> do
                    fail "Internal error - invalid unicode escape sequence"

    let escaped =
            Combinators.choice
                (   (   if multiline
                        then []
                        else [ "\n" <$ "\\n", "\t" <$ "\\t" ]
                    )
                <>  [ "\"" <$ "\\\""
                    , "\\" <$ "\\\\"
                    , "/"  <$ "\\/"
                    , "\b" <$ "\\b"
                    , "\f" <$ "\\f"
                    , "\r" <$ "\\r"
                    , "$"  <$ "\\$"
                    , unicodeEscape
                    ]
                ) Megaparsec.<?> "escape sequence"

    let interpolated = do
            "${"

            originalState <- Megaparsec.getParserState

            let loop state = case result of
                    Left _ -> []
                    Right token -> token : loop newState
                  where
                    (newState, result) =
                        Megaparsec.runParser' lexLocatedToken state

            let locatedTokens = loop afterSpace
                  where
                    (afterSpace, _) = Megaparsec.runParser' space originalState

            (syntax, index) <- case Earley.allParses (Earley.parser (grammar True)) locatedTokens of
                ([], Report{ position }) -> do
                    case drop position locatedTokens of
                        [] ->
                            return ()
                        LocatedToken{ state } : _ ->
                            Megaparsec.setParserState state

                    empty Megaparsec.<?> "Incomplete string interpolation"

                (result : _, _) -> do
                    return result

            case drop (index - 1) locatedTokens of
                [] -> do
                    empty Megaparsec.<?> "Incomplete string literal"

                LocatedToken{ state } : _ -> do
                    Megaparsec.setParserState state

            "}"

            return (Chunks mempty [(syntax, mempty)])

    chunks <- many (unescaped <|> interpolated <|> escaped <|> ("$" <$ "$"))

    let chunk = mconcat chunks

    let dedented
            | multiline = dedent chunk
            | otherwise = chunk

    "\""

    return (TextLiteral dedented)

isLabel0 :: Char -> Bool
isLabel0 c = Char.isLower c || c == '_'

isLabel :: Char -> Bool
isLabel c = Char.isAlphaNum c || c == '_' || c == '-' || c == '/'

-- | Returns `True` if the given label is valid when unquoted
validLabel :: Text -> Bool
validLabel text_ =
    case Text.uncons text_ of
        Nothing ->
            False
        Just (h, t) ->
                isLabel0 h
            &&  Text.all isLabel t
            &&  not (HashSet.member text_ reserved)

-- | Returns `True` if the given record label is valid when unquoted
validRecordLabel :: Text -> Bool
validRecordLabel text_ =
    case Text.uncons text_ of
        Nothing ->
            False
        Just (h, t) ->
                (   isLabel0 h
                &&  Text.all isLabel t
                &&  not (HashSet.member text_ reserved)
                )
            ||  text_ == "null"
            ||  text_ == "some"
            ||  text_ == "true"
            ||  text_ == "false"

-- | Returns `True` if the given alternative label is valid when unquoted
validAlternativeLabel :: Text -> Bool
validAlternativeLabel text_ =
    case Text.uncons text_ of
        Nothing ->
            False
        Just (h, t) ->
                Char.isUpper h
            &&  Text.all isLabel t
            &&  not (HashSet.member text_ reserved)

-- | Reserved tokens, which can't be used for labels unless they are quoted
reserved :: HashSet Text
reserved =
    HashSet.fromList
        [ "Alternatives"
        , "Bool"
        , "Fields"
        , "Integer"
        , "List"
        , "List/equal"
        , "Natural"
        , "Optional"
        , "Real"
        , "Real/equal"
        , "Text"
        , "Text/equal"
        , "Type"
        , "abs"
        , "else"
        , "false"
        , "fold"
        , "for"
        , "forall"
        , "github"
        , "http"
        , "if"
        , "import"
        , "in"
        , "indexed"
        , "length"
        , "let"
        , "map"
        , "null"
        , "of"
        , "prompt"
        , "read"
        , "reveal"
        , "show"
        , "some"
        , "then"
        , "true"
        , "yaml"
        ]

lexLabel :: Lexer Token
lexLabel = lexeme (try lexUnquotedLabel <|> try lexQuotedLabel)

lexUnquotedLabel :: Lexer Token
lexUnquotedLabel = do
    c0 <- Megaparsec.satisfy isLabel0 Megaparsec.<?> "label character"

    cs <- Megaparsec.takeWhileP (Just "label character") isLabel

    let name = Text.cons c0 cs

    Monad.guard (not (HashSet.member name reserved))

    return (Label name)

lexQuotedLabel :: Lexer Token
lexQuotedLabel = do
    "."

    name <- lexSingleQuoted

    return (Label name)

lexAlternative :: Lexer Token
lexAlternative = lexeme do
    c0 <- Megaparsec.satisfy Char.isUpper Megaparsec.<?> "alternative character"

    cs <- Megaparsec.takeWhileP (Just "alternative character") isLabel

    return (Grace.Parser.Alternative (Text.cons c0 cs))

lexSingleQuoted :: Lexer Text
lexSingleQuoted = lexeme do
    "'"

    let isText c =
                ('\x20' <= c && c <=     '\x26')
            ||  ('\x28' <= c && c <=     '\x5c')
            ||  ('\x5d' <= c && c <= '\x10FFFF')

    let unescaped = Megaparsec.takeWhile1P (Just "alternative character") isText

    let unicodeEscape = do
            "\\u"

            codepoint <- Combinators.count 4 Megaparsec.Char.hexDigitChar

            case Read.hexadecimal (Text.pack codepoint) of
                Right (n, "") -> do
                    return (Text.singleton (Char.chr n))
                _             -> do
                    fail "Internal error - invalid unicode escape sequence"

    let escaped =
            Combinators.choice
                [ "'"  <$ "\\\'"
                , "\\" <$ "\\\\"
                , "/"  <$ "\\/"
                , "\b" <$ "\\b"
                , "\f" <$ "\\f"
                , "\n" <$ "\\n"
                , "\r" <$ "\\r"
                , "\t" <$ "\\t"
                , unicodeEscape
                ] Megaparsec.<?> "escape sequence"

    texts <- many (unescaped <|> escaped)

    "'"

    return (Text.concat texts)

lexQuotedAlternative :: Lexer Token
lexQuotedAlternative = do
    name <- lexSingleQuoted

    return (Grace.Parser.Alternative name)

-- | Tokens produced by lexing
data Token
    = Abs
    | Alternative Text
    | Alternatives
    | And
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
    | DotNumber Integer
    | DoubleEquals
    | Else
    | Equals
    | False_
    | Fields
    | File FilePath
    | Fold
    | For
    | Forall
    | ForwardSlash
    | GitHub
    | GreaterThanOrEqual
    | If
    | Import
    | In
    | Indexed
    | Int Sign Natural
    | Integer
    | JSON
    | Key
    | Label Text
    | Lambda
    | Length
    | LessThanOrEqual
    | Let
    | List
    | Map
    | Modulus
    | Natural
    | NotEqual
    | Null
    | Of
    | OpenAngle
    | OpenBrace
    | OpenBracket
    | OpenParenthesis
    | Optional
    | Or
    | Plus
    | HTTP
    | Prompt
    | Read
    | Real
    | RealLiteral Sign Scientific
    | Reveal
    | Show
    | Some
    | Text
    | TextLiteral (Chunks Offset Input)
    | Then
    | Times
    | True_
    | Type
    | URI URI.URI
    | YAML
    deriving stock (Eq, Show)

data Sign = Unsigned | Positive | Negative
    deriving stock (Eq, Show)

{-| A token with parsing state attached, used for reporting line and column
    numbers in error messages
-}
data LocatedToken = LocatedToken { token :: Token, state :: State Text Void }
    deriving stock (Show)

-- | Errors related to lexing and parsing
data ParseError
    = LexingFailed Location
    | ParsingFailed Location
    deriving stock (Eq, Show)

instance Exception ParseError where
    displayException (LexingFailed location) = Text.unpack
        (Location.renderError "Invalid input - Lexing failed" location)
    displayException (ParsingFailed location) = Text.unpack
        (Location.renderError "Invalid input - Parsing failed" location)

type Parser r = Prod r Text LocatedToken

matchLabel :: Token -> Maybe Text
matchLabel (Grace.Parser.Label l) = Just l
matchLabel  _                     = Nothing

matchReservedLabel :: Token -> Maybe Text
matchReservedLabel Grace.Parser.Some   = Just "some"
matchReservedLabel Grace.Parser.Null   = Just "null"
matchReservedLabel Grace.Parser.True_  = Just "true"
matchReservedLabel Grace.Parser.False_ = Just "false"
matchReservedLabel _                   = Nothing

matchAlternative :: Token -> Maybe Text
matchAlternative (Grace.Parser.Alternative a) = Just a
matchAlternative  _                           = Nothing

matchReal :: Token -> Maybe (Sign, Scientific)
matchReal (Grace.Parser.RealLiteral sign n) = Just (sign, n)
matchReal  _                                = Nothing

matchInt :: Token -> Maybe (Sign, Natural)
matchInt (Grace.Parser.Int sign n) = Just (sign, n)
matchInt  _                        = Nothing

matchDotNumber :: Token -> Maybe Integer
matchDotNumber (Grace.Parser.DotNumber n) = Just n
matchDotNumber  _                         = Nothing

matchChunks :: Token -> Maybe (Chunks Offset Input)
matchChunks (Grace.Parser.TextLiteral c) = Just c
matchChunks  _                           = Nothing

matchText :: Token -> Maybe Text
matchText (Grace.Parser.TextLiteral (Chunks t [])) = Just t
matchText  _                                       = Nothing

matchFile :: Token -> Maybe FilePath
matchFile (Grace.Parser.File f) = Just f
matchFile  _                    = Nothing

matchURI :: Token -> Maybe URI.URI
matchURI (Grace.Parser.URI t) = Just t
matchURI  _                   = Nothing

terminal :: (Token -> Maybe a) -> Parser r a
terminal match = Earley.terminal match'
  where
    match' locatedToken_ = match (Grace.Parser.token locatedToken_)

label :: Parser r Text
label = terminal matchLabel

reservedLabel :: Parser r Text
reservedLabel = terminal matchReservedLabel

alternative :: Parser r Text
alternative = terminal matchAlternative

int :: Parser r (Sign, Natural)
int = terminal matchInt

dotNumber :: Parser r Integer
dotNumber = terminal matchDotNumber

text :: Parser r Text
text = terminal matchText

parseToken :: Token -> Parser r ()
parseToken t = void (Earley.satisfy predicate <?> render t)
  where
    predicate locatedToken_ = token locatedToken_ == t

locatedTerminal :: (Token -> Maybe a) -> Parser r (Offset, a)
locatedTerminal match = Earley.terminal match'
  where
    match' locatedToken_@LocatedToken{ state }  = do
      a <- match (token locatedToken_)
      return (Offset (stateOffset state), a)

locatedLabel :: Parser r (Offset, Text)
locatedLabel = locatedTerminal matchLabel

locatedReservedLabel :: Parser r (Offset, Text)
locatedReservedLabel = locatedTerminal matchReservedLabel

locatedAlternative :: Parser r (Offset, Text)
locatedAlternative = locatedTerminal matchAlternative

locatedReal :: Parser r (Offset, (Sign, Scientific))
locatedReal = locatedTerminal matchReal

locatedInt :: Parser r (Offset, (Sign, Natural))
locatedInt = locatedTerminal matchInt

locatedChunks :: Parser r (Offset, Chunks Offset Input)
locatedChunks = locatedTerminal matchChunks

locatedText :: Parser r (Offset, Text)
locatedText = locatedTerminal matchText

locatedFile :: Parser r (Offset, FilePath)
locatedFile = locatedTerminal matchFile

locatedURI :: Parser r (Offset, URI.URI)
locatedURI = locatedTerminal matchURI

locatedToken :: Token -> Parser r Offset
locatedToken expectedToken =
    Earley.terminal capture <?> render expectedToken
  where
    capture LocatedToken{ token = actualToken, state }
        | expectedToken == actualToken = Just (Offset (stateOffset state))
        | otherwise                    = Nothing

-- | This render function is currently never used since `Location.renderError`
--   does not display expected tokens at all, but I maintain this anyway in
--   case someone wants to modify the code to display them.
render :: Token -> Text
render t = case t of
    Grace.Parser.Abs                -> "abs"
    Grace.Parser.Alternative _      -> "an alternative"
    Grace.Parser.Alternatives       -> "Alternatives"
    Grace.Parser.And                -> "&&"
    Grace.Parser.Arrow              -> "  ->"
    Grace.Parser.At                 -> "@"
    Grace.Parser.Bar                -> "|"
    Grace.Parser.Bool               -> "Bool"
    Grace.Parser.CloseAngle         -> ">"
    Grace.Parser.CloseBrace         -> "}"
    Grace.Parser.CloseBracket       -> "]"
    Grace.Parser.CloseParenthesis   -> ")"
    Grace.Parser.Colon              -> ":"
    Grace.Parser.Comma              -> ","
    Grace.Parser.Dash               -> "-"
    Grace.Parser.Dot                -> "."
    Grace.Parser.DotNumber _        -> ".n"
    Grace.Parser.DoubleEquals       -> "=="
    Grace.Parser.Else               -> "else"
    Grace.Parser.Equals             -> "="
    Grace.Parser.False_             -> "False"
    Grace.Parser.Fields             -> "Fields"
    Grace.Parser.File _             -> "a file"
    Grace.Parser.Fold               -> "fold"
    Grace.Parser.For                -> "for"
    Grace.Parser.Forall             -> "forall"
    Grace.Parser.ForwardSlash       -> "/"
    Grace.Parser.GitHub             -> "github"
    Grace.Parser.GreaterThanOrEqual -> ">="
    Grace.Parser.If                 -> "if"
    Grace.Parser.Import             -> "import"
    Grace.Parser.In                 -> "in"
    Grace.Parser.Indexed            -> "indexed"
    Grace.Parser.Int _ _            -> "an integer literal"
    Grace.Parser.Integer            -> "Integer"
    Grace.Parser.JSON               -> "JSON"
    Grace.Parser.Key                -> "Key"
    Grace.Parser.Label _            -> "a label"
    Grace.Parser.Lambda             -> "\\"
    Grace.Parser.Length             -> "length"
    Grace.Parser.LessThanOrEqual    -> "<="
    Grace.Parser.Let                -> "let"
    Grace.Parser.List               -> "list"
    Grace.Parser.Map                -> "map"
    Grace.Parser.Modulus            -> "%"
    Grace.Parser.Natural            -> "Natural"
    Grace.Parser.NotEqual           -> "!="
    Grace.Parser.Null               -> "null"
    Grace.Parser.Of                 -> "of"
    Grace.Parser.OpenAngle          -> "<"
    Grace.Parser.OpenBrace          -> "{"
    Grace.Parser.OpenBracket        -> "<"
    Grace.Parser.OpenParenthesis    -> "("
    Grace.Parser.Optional           -> "List"
    Grace.Parser.Or                 -> "||"
    Grace.Parser.Plus               -> "+"
    Grace.Parser.HTTP               -> "http"
    Grace.Parser.Prompt             -> "prompt"
    Grace.Parser.Read               -> "read"
    Grace.Parser.Real               -> "Real"
    Grace.Parser.RealLiteral _ _    -> "a real number literal"
    Grace.Parser.Reveal             -> "reveal"
    Grace.Parser.Show               -> "show"
    Grace.Parser.Some               -> "some"
    Grace.Parser.Text               -> "Text"
    Grace.Parser.TextLiteral _      -> "a text literal"
    Grace.Parser.Then               -> "then"
    Grace.Parser.Times              -> "*"
    Grace.Parser.True_              -> "True"
    Grace.Parser.Type               -> "Type"
    Grace.Parser.URI _              -> "a URI"
    Grace.Parser.YAML               -> "yaml"

grammar :: Bool -> Grammar r (Parser r (Syntax Offset Input))
grammar endsWithBrace = mdo
    parseBinding <- rule do
        let annotated = do
                parseToken Grace.Parser.OpenParenthesis

                ~(nameLocation, name) <- locatedLabel

                annotation <- optional do
                    parseToken Grace.Parser.Colon

                    r <- quantifiedType

                    pure r

                assignment <- optional do
                    parseToken Grace.Parser.Equals

                    r <- expression

                    pure r

                parseToken Grace.Parser.CloseParenthesis

                pure PlainBinding
                    { plain = NameBinding
                        { nameLocation
                        , name
                        , annotation
                        , assignment
                        }
                    }

        let unannotated = do
                ~(nameLocation, name) <- locatedLabel

                pure PlainBinding
                    { plain = NameBinding
                        { nameLocation
                        , name
                        , annotation = Nothing
                        , assignment = Nothing
                        }
                    }

        let fields = do
                let parseAnnotation = do
                        parseToken Grace.Parser.Colon

                        annotation <- quantifiedType

                        pure annotation

                let parseDefault = do
                        parseToken Grace.Parser.Equals

                        assignment <- expression

                        pure assignment

                let parseFieldName = do
                        ~(nameLocation, name) <- locatedRecordLabel

                        annotation <- optional parseAnnotation

                        assignment <- optional parseDefault

                        return NameBinding{ nameLocation, name, annotation, assignment }

                fieldNamesLocation <- locatedToken Grace.Parser.OpenBrace

                fieldNames <- parseFieldName `sepBy` parseToken Grace.Parser.Comma
                parseToken Grace.Parser.CloseBrace

                pure RecordBinding{ fieldNamesLocation, fieldNames }

        annotated <|> unannotated <|> fields

    expression <- rule
        (   do  location <- locatedToken Grace.Parser.Lambda

                bindings <- some1 parseBinding

                parseToken Grace.Parser.Arrow

                body0 <- expression

                return do
                    let cons binding body = Syntax.Lambda
                            { location
                            , binding
                            , body
                            }

                    foldr cons body0 bindings

        <|> do  assignments <- some1 parseAssignment

                parseToken Grace.Parser.In

                body <- expression

                return do
                    let location = case NonEmpty.head assignments of
                            Syntax.Define{ definition = Syntax.Definition{ nameLocation } } -> nameLocation
                            Syntax.Bind{ binding = Syntax.PlainBinding{ plain = Syntax.NameBinding{ nameLocation } } } -> nameLocation
                            Syntax.Bind{ binding = Syntax.RecordBinding{ fieldNamesLocation } } -> fieldNamesLocation
                    Syntax.Let{ location, assignments, body }

        <|> do  location <- locatedToken Grace.Parser.If

                predicate <- expression

                parseToken Grace.Parser.Then

                ifTrue <- expression

                parseToken Grace.Parser.Else

                ifFalse <- expression

                return Syntax.If{ location, predicate, ifTrue, ifFalse }

        <|> do  let annotatedFile = do
                        ~(location, file) <- locatedFile

                        return Syntax.Embed
                            { location
                            , embedded = Path file AsCode
                            }

                let annotatedURI = do
                        ~(location, uri) <- locatedURI

                        return Syntax.Embed
                            { location
                            , embedded = Grace.Input.URI uri AsText
                            }

                let adapt Syntax.Embed{ location, embedded = Path file AsCode } Type.Scalar{ scalar = Monotype.Text } =
                        Syntax.Embed{ location, embedded = Path file AsText }
                    adapt Syntax.Embed{ location, embedded = Path file AsCode } Type.Scalar{ scalar = Monotype.Key } =
                        Syntax.Embed{ location, embedded = Path file AsKey }
                    adapt Syntax.Embed{ location, embedded = Grace.Input.URI uri AsCode } Type.Scalar{ scalar = Monotype.Text } =
                        Syntax.Embed
                            { location
                            , embedded = Grace.Input.URI uri AsText
                            }
                    adapt Syntax.Embed{ location, embedded = Grace.Input.URI uri AsCode } Type.Scalar{ scalar = Monotype.Key } =
                        Syntax.Embed
                            { location
                            , embedded = Grace.Input.URI uri AsKey
                            }
                    adapt annotated annotation =
                        Syntax.Annotation
                            { location = Syntax.location annotated
                            , annotated
                            , annotation
                            }

                annotated <-
                    (   annotatedFile
                    <|> annotatedURI
                    <|> operatorExpression
                    )

                parseToken Grace.Parser.Colon

                annotation <- quantifiedType

                return (adapt annotated annotation)

        <|> do  operatorExpression
        )

    operatorExpression <- rule orExpression

    let op token_ operator subExpression = do
            let snoc left (operatorLocation, right) = Syntax.Operator
                    { location = Syntax.location left
                    , left
                    , operatorLocation
                    , operator
                    , right
                    }

            e0 <- subExpression

            ses <- many do
                s <- locatedToken token_

                e <- subExpression;

                return (s, e)

            return (foldl snoc e0 ses)

    orExpression <- rule (op Grace.Parser.Or Syntax.Or andExpression)

    andExpression <- rule (op Grace.Parser.And Syntax.And equalExpression)

    equalExpression <- rule (op Grace.Parser.DoubleEquals Syntax.Equal notEqualExpression)

    notEqualExpression <- rule (op Grace.Parser.NotEqual Syntax.NotEqual lessThanExpression)

    lessThanExpression <- rule (op Grace.Parser.OpenAngle Syntax.LessThan lessThanOrEqualExpression)

    lessThanOrEqualExpression <- rule (op Grace.Parser.LessThanOrEqual Syntax.LessThanOrEqual greaterThanExpression)

    greaterThanExpression <- rule (op Grace.Parser.CloseAngle Syntax.GreaterThan greaterThanOrEqualExpression)

    greaterThanOrEqualExpression <- rule (op Grace.Parser.GreaterThanOrEqual Syntax.GreaterThanOrEqual plusExpression)

    plusExpression <- rule (op Grace.Parser.Plus Syntax.Plus minusExpression)

    minusExpression <- rule (op Grace.Parser.Dash Syntax.Minus timesExpression)

    timesExpression <- rule (op Grace.Parser.Times Syntax.Times modulusExpression)

    modulusExpression <- rule (op Grace.Parser.Modulus Syntax.Modulus divideExpression)

    divideExpression <- rule (op Grace.Parser.ForwardSlash Syntax.Divide applicationExpression)

    let application function argument = Syntax.Application
            { location = Syntax.location function
            , function
            , argument
            }

    applicationExpression <- rule do
        e <-  (   do  i <- (True <$ locatedToken Grace.Parser.Import) <|> pure False
                      f <-  (   do  location <- locatedToken Grace.Parser.Prompt

                                    arguments <- projectExpression

                                    return \import_ -> Syntax.Prompt{ location, import_, arguments, schema = Nothing }

                            <|> do  location <- locatedToken Grace.Parser.HTTP

                                    arguments <- projectExpression

                                    return \import_ -> Syntax.HTTP{ location, import_, arguments, schema = Nothing }

                            <|> do  location <- locatedToken Grace.Parser.Read

                                    arguments <- projectExpression

                                    return \import_ -> Syntax.Read{ location, import_, arguments, schema = Nothing }

                            <|> do  location <- locatedToken Grace.Parser.GitHub

                                    arguments <- projectExpression

                                    return \import_ -> Syntax.GitHub{ location, import_, arguments, schema = Nothing }
                            )

                      pure (f i)

              <|> do  location <- locatedToken Grace.Parser.Fold

                      handlers <- projectExpression

                      return Syntax.Fold{ location, handlers }

              <|> do  projectExpression
              )

        es <- many projectExpression

        return (foldl application e es)

    projectExpression <- rule do
        let snoc location record f =
                f location record

        let parseField = do
                ~(fieldLocation, field) <- locatedRecordLabel

                return Syntax.Field{ fieldLocation, field }

        let parseSingle = do
                single <- parseField

                return \location larger ->
                    Syntax.Project{ location, larger, smaller = Syntax.Single{ single } }

        let parseMultiple = do
                multipleLocation <- locatedToken Grace.Parser.OpenBrace

                multiple <- parseField `sepBy` parseToken Grace.Parser.Comma

                parseToken Grace.Parser.CloseBrace

                return \location larger ->
                    Syntax.Project{ location, larger, smaller = Multiple{ multipleLocation, multiple } }

        let parseIndex = do
                index <- dotNumber

                return \location larger ->
                    Syntax.Project{ location, larger, smaller = Index { index } }

        let parseSlice = do
                let withSign (sign, n) = case sign of
                        Unsigned -> fromIntegral n
                        Positive -> fromIntegral n
                        Negative -> negate (fromIntegral n)

                parseToken Grace.Parser.OpenBracket

                begin <- fmap (fmap withSign) (optional int)

                parseToken Grace.Parser.Colon

                end <- fmap (fmap withSign) (optional int)

                parseToken Grace.Parser.CloseBracket

                return \location larger ->
                    Syntax.Project
                        { location
                        , larger
                        , smaller = Slice{ begin, end }
                        }

        let parseDotAccess = do
                smaller <- parseIndex <|> (parseToken Grace.Parser.Dot *> (parseSingle <|> parseMultiple))

                pure smaller

        record <- alternativeExpression

        projections <- many
            (do smaller <- parseSlice <|> parseDotAccess

                pure smaller
            )

        return (foldl (snoc (Syntax.location record)) record projections)

    alternativeExpression <- rule
        (   do  ~(location, name) <- locatedAlternative

                argument <- primitiveExpression

                return Syntax.Alternative{ location, name, argument }

        <|>     primitiveExpression
        )


    primitiveExpression <- rule
        (   do  ~(location, name) <- locatedLabel

                return Syntax.Variable{ location, name }

        <|> do  ~(location, name) <- locatedAlternative

                argument <- primitiveExpression

                return Syntax.Alternative{ location, name, argument }

        <|> do  location <- locatedToken Grace.Parser.OpenBracket

                optional (parseToken Grace.Parser.Comma)

                elements <- expression `sepBy` parseToken Grace.Parser.Comma

                optional (parseToken Grace.Parser.Comma)

                parseToken Grace.Parser.CloseBracket

                return Syntax.List{ location, elements = Seq.fromList elements }

        <|> do  location <- locatedToken Grace.Parser.OpenBrace

                optional (parseToken Grace.Parser.Comma)

                fieldValues <- fieldValue `sepBy` parseToken Grace.Parser.Comma

                optional (parseToken Grace.Parser.Comma)

                parseToken Grace.Parser.CloseBrace

                return Syntax.Record{ location, fieldValues }

        <|> do  location <- locatedToken Grace.Parser.True_

                return Syntax.Scalar{ location, scalar = Syntax.Bool True }

        <|> do  location <- locatedToken Grace.Parser.False_

                return Syntax.Scalar{ location, scalar = Syntax.Bool False }

        <|> do  location <- locatedToken Grace.Parser.Null

                return Syntax.Scalar{ location, scalar = Syntax.Null }

        <|> do  let withSign Unsigned n = Syntax.Real n
                    withSign Positive n = Syntax.Real n
                    withSign Negative n = Syntax.Real (negate n)

                ~(location, (sign, n)) <- locatedReal

                return Syntax.Scalar{ location, scalar = withSign sign n }

        <|> do  let withSign Unsigned n = Syntax.Natural (fromIntegral n)
                    withSign Positive n = Syntax.Integer (fromIntegral n)
                    withSign Negative n = Syntax.Integer (negate (fromIntegral n))

                ~(location, (sign, n)) <- locatedInt

                return Syntax.Scalar{ location, scalar = withSign sign n }

        <|> do  location <- locatedToken Grace.Parser.Some

                return Syntax.Builtin{ location, builtin = Syntax.Some }

        <|> do  location <- locatedToken Grace.Parser.Show

                return Syntax.Builtin{ location, builtin = Syntax.Show }

        <|> do  location <- locatedToken Grace.Parser.YAML

                return Syntax.Builtin{ location, builtin = Syntax.YAML }

        <|> do  location <- locatedToken Grace.Parser.Indexed

                return Syntax.Builtin{ location, builtin = Syntax.Indexed }

        <|> do  location <- locatedToken Grace.Parser.Length

                return Syntax.Builtin{ location, builtin = Syntax.Length }

        <|> do  location <- locatedToken Grace.Parser.Map

                return Syntax.Builtin{ location, builtin = Syntax.Map }

        <|> do  location <- locatedToken Grace.Parser.Abs

                return Syntax.Builtin{ location, builtin = Syntax.Abs }

        <|> do  location <- locatedToken Grace.Parser.Reveal

                return Syntax.Builtin{ location, builtin = Syntax.Reveal }

        <|> do  ~(location, chunks) <- locatedChunks

                return Syntax.Text{ location, chunks }

        <|> do  ~(location, file) <- locatedFile

                return Syntax.Embed{ location, embedded = Path file AsCode }

        <|> do  ~(location, uri) <- locatedURI

                return Syntax.Embed
                    { location
                    , embedded = Grace.Input.URI uri AsCode
                    }

        <|> do  parseToken Grace.Parser.OpenParenthesis

                e <- expression

                parseToken Grace.Parser.CloseParenthesis

                return e
        )

    parseAssignment <- rule do
        let parseLetAssignment = do
                let parseDefinition = do
                        ~(nameLocation, name) <- locatedLabel

                        bindings <- many parseBinding

                        annotation <- optional do
                            parseToken Grace.Parser.Colon

                            t <- quantifiedType

                            return t

                        parseToken Grace.Parser.Equals

                        assignment <- expression

                        return \assignmentLocation -> Syntax.Define
                            { assignmentLocation
                            , definition = Syntax.Definition
                                { nameLocation
                                , name
                                , bindings
                                , annotation
                                , assignment
                                }
                            }

                let parseBind = do
                        binding <- parseBinding

                        parseToken Grace.Parser.Equals

                        assignment <- expression

                        return \assignmentLocation -> Syntax.Bind
                            { assignmentLocation
                            , monad = NoMonad
                            , binding
                            , assignment
                            }

                assignmentLocation <- locatedToken Grace.Parser.Let

                f <- parseDefinition <|> parseBind

                return (f assignmentLocation)

        let parseForAssignment = do
                assignmentLocation <- locatedToken Grace.Parser.For

                binding <- parseBinding

                parseToken Grace.Parser.Of

                assignment <- expression

                return Syntax.Bind
                    { assignmentLocation
                    , monad = UnknownMonad
                    , binding
                    , assignment
                    }

        parseLetAssignment <|> parseForAssignment

    recordLabel <- rule (reservedLabel <|> label <|> alternative <|> text)

    locatedRecordLabel <- rule
        (   locatedReservedLabel
        <|> locatedLabel
        <|> locatedAlternative
        <|> locatedText
        )

    fieldValue <- rule do
        let setting = do
                ~(nameLocation, name) <- locatedRecordLabel

                bindings <- many parseBinding

                annotation <- optional do
                    parseToken Grace.Parser.Colon

                    t <- quantifiedType

                    return t

                parseToken Grace.Parser.Colon

                assignment <- expression

                return Syntax.Definition
                    { nameLocation
                    , name
                    , bindings
                    , annotation
                    , assignment
                    }

        let pun = do
                ~(nameLocation, name) <- locatedRecordLabel

                return Syntax.Definition
                    { nameLocation
                    , name
                    , bindings = [] -- TODO
                    , annotation = Nothing
                    , assignment = Syntax.Variable
                        { location = nameLocation
                        , name
                        }
                    }

        setting <|> pun

    domain <- rule
        (   do  parseToken Grace.Parser.Type

                return Domain.Type

        <|> do  parseToken Grace.Parser.Fields

                return Domain.Fields

        <|> do  parseToken Grace.Parser.Alternatives

                return Domain.Alternatives
        )

    quantifiedType <- rule do
        fss <- many
            (   do  location <- locatedToken Grace.Parser.Forall

                    fs <- some do
                        parseToken Grace.Parser.OpenParenthesis

                        ~(typeVariableOffset, typeVariable) <- locatedLabel

                        parseToken Grace.Parser.Colon

                        domain_ <- domain

                        parseToken Grace.Parser.CloseParenthesis

                        return \location_ type_ -> Type.Forall
                            { location = location_
                            , nameLocation = typeVariableOffset
                            , name = typeVariable
                            , domain = domain_
                            , type_
                            }

                    parseToken Grace.Parser.Dot

                    return (map ($ location) fs)
            )

        t <- functionType

        return (foldr ($) t (concat fss))

    functionType <- rule do
        let function input output =
                Type.Function{ location = Type.location input, input, output }

        ts <- applicationType `sepBy1` parseToken Grace.Parser.Arrow

        return (foldr function (NonEmpty.last ts) (NonEmpty.init ts))

    applicationType <- rule
        (   do  location <- locatedToken Grace.Parser.List

                type_ <- primitiveType

                return Type.List{ location, type_ }

        <|> do  location <- locatedToken Grace.Parser.Optional

                type_ <- primitiveType

                return Type.Optional{ location, type_ }

        <|> do  primitiveType
        )

    primitiveType <- rule
        (   do  location <- locatedToken Grace.Parser.Bool

                return Type.Scalar{ location, scalar = Monotype.Bool }

        <|> do  location <- locatedToken Grace.Parser.Real

                return Type.Scalar{ location, scalar = Monotype.Real }

        <|> do  location <- locatedToken Grace.Parser.Integer

                return Type.Scalar{ location, scalar = Monotype.Integer }

        <|> do  location <- locatedToken Grace.Parser.JSON

                return Type.Scalar{ location, scalar = Monotype.JSON }

        <|> do  location <- locatedToken Grace.Parser.Natural

                return Type.Scalar{ location, scalar = Monotype.Natural }

        <|> do  location <- locatedToken Grace.Parser.Text

                return Type.Scalar{ location, scalar = Monotype.Text }

        <|> do  location <- locatedToken Grace.Parser.Key

                return Type.Scalar{ location, scalar = Monotype.Key }

        <|> do  ~(location, name) <- locatedLabel

                return Type.VariableType{ location, name }

        <|> do  locatedOpenBrace <- locatedToken Grace.Parser.OpenBrace

                optional (parseToken Grace.Parser.Comma)

                fieldTypes <- fieldType `endBy` parseToken Grace.Parser.Comma

                toFields <-
                    (   do  text_ <- recordLabel

                            pure (\fs -> Type.Fields fs (Monotype.VariableFields text_))

                    <|> do  pure (\fs -> Type.Fields fs Monotype.EmptyFields)

                    <|> do  f <- fieldType

                            pure (\fs -> Type.Fields (fs <> [ f ]) Monotype.EmptyFields)
                    )

                optional (parseToken Grace.Parser.Comma)

                parseToken Grace.Parser.CloseBrace

                return Type.Record
                    { location = locatedOpenBrace
                    , fields = toFields fieldTypes
                    }

        <|> do  locatedOpenAngle <- locatedToken Grace.Parser.OpenAngle

                optional (parseToken Grace.Parser.Bar)

                alternativeTypes <- alternativeType `endBy` parseToken Grace.Parser.Bar

                toAlternatives <-
                    (   do  text_ <- label

                            return (\as -> Type.Alternatives as (Monotype.VariableAlternatives text_))

                    <|> do  pure (\as -> Type.Alternatives as Monotype.EmptyAlternatives)

                    <|> do  a <- alternativeType
                            return (\as -> Type.Alternatives (as <> [ a ]) Monotype.EmptyAlternatives)
                    )

                optional (parseToken Grace.Parser.Bar)

                parseToken Grace.Parser.CloseAngle

                return Type.Union
                    { location = locatedOpenAngle
                    , alternatives = toAlternatives alternativeTypes
                    }

        <|> do  parseToken Grace.Parser.OpenParenthesis

                t <- quantifiedType

                parseToken Grace.Parser.CloseParenthesis

                return t
        )

    fieldType <- rule do
        field <- recordLabel

        parseToken Grace.Parser.Colon

        t <- quantifiedType

        return (field, t)

    alternativeType <- rule do
        a <- alternative

        parseToken Grace.Parser.Colon

        t <- quantifiedType

        return (a, t)

    -- Used for parsing a string interpolation
    expressionEndingWithBrace <- rule do
        a <- expression

        parseToken Grace.Parser.CloseBrace

        return a

    return (if endsWithBrace then expressionEndingWithBrace else expression)

-- | Parse a complete expression
parse
    :: String
    -- ^ Name of the input (used for error messages)
    -> Text
    -- ^ Source code
    -> Either ParseError (Syntax Offset Input)
parse name code = do
    tokens <- lex name code

    case Earley.fullParses (Earley.parser (grammar False)) tokens of
        ([], Report{ unconsumed }) -> do
            let offset =
                    case unconsumed of
                        [] ->
                            Offset (Text.length code)
                        locatedToken_ : _ ->
                            Offset (stateOffset (state locatedToken_))

            Left (ParsingFailed Location{ name, code, offset })

        (result : _, _) -> do
            return result
