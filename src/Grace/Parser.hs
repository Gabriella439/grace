{-| This module contains the logic for parsing Grace files using @Earley@.

    The main reason for not using @attoparsec@ or @megaparsec@ is because
    LR parsers are easier to maintain due to not needing to left-factor the
    grammar.

    The main reason for not using @happy@ is because it uses a separate
    code generation step, which leads to worse error messages and poor
    support for interactive type-checking.
-}

module Grace.Parser
    ( -- * Parsing
      parse
    ) where

import Control.Applicative (many, (<|>))
import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Control.Applicative.Combinators (sepBy)
import Data.Functor (void)
import Data.List.NonEmpty (some1)
import Data.Text (Text)
import Grace.Lexer (Token)
import Grace.Syntax (Syntax)
import Text.Earley (Grammar, Prod, Report(..), rule, (<?>))

import qualified Data.List.NonEmpty     as NonEmpty
import qualified Grace.Lexer            as Lexer
import qualified Grace.Syntax           as Syntax
import qualified Grace.Type             as Type
import qualified Text.Earley            as Earley

type Parser r = Prod r Text Lexer.LocatedToken

terminal :: (Token -> Maybe a) -> Parser r a
terminal match = Earley.terminal match'
  where
    match' locatedToken = match (Lexer.token locatedToken)

label :: Parser r Text
label = terminal match
  where
    match (Lexer.Label l) = Just l
    match  _              = Nothing

alternative :: Parser r Text
alternative = terminal match
  where
    match (Lexer.Alternative a) = Just a
    match  _                    = Nothing

int :: Parser r Int
int = terminal match
  where
    match (Lexer.Int n) = Just n
    match  _            = Nothing

text :: Parser r Text
text = terminal match
  where
    match (Lexer.TextLiteral t) = Just t
    match  _                    = Nothing

file :: Parser r FilePath
file = terminal match
  where
    match (Lexer.File f) = Just f
    match  _             = Nothing

token :: Lexer.Token -> Parser r ()
token t = void (Earley.satisfy predicate <?> render t)
  where
    predicate locatedToken = Lexer.token locatedToken == t

render :: Lexer.Token -> Text
render t = case t of
    Lexer.Alternative _    -> "an alternative"
    Lexer.And              -> "&&"
    Lexer.Append           -> "++"
    Lexer.Arrow            -> "->"
    Lexer.At               -> "@"
    Lexer.Bool             -> "Bool"
    Lexer.CloseAngle       -> ">"
    Lexer.CloseBrace       -> "}"
    Lexer.CloseBracket     -> "]"
    Lexer.CloseParenthesis -> ")"
    Lexer.Colon            -> ":"
    Lexer.Comma            -> ","
    Lexer.Dot              -> "."
    Lexer.Else             -> "else"
    Lexer.Equals           -> "="
    Lexer.False_           -> "False"
    Lexer.File _           -> "a file"
    Lexer.Forall           -> "forall"
    Lexer.If               -> "if"
    Lexer.In               -> "in"
    Lexer.Int _            -> "an integer literal"
    Lexer.Label _          -> "a label"
    Lexer.Lambda           -> "\\"
    Lexer.Let              -> "let"
    Lexer.List             -> "list"
    Lexer.Merge            -> "merge"
    Lexer.Natural          -> "Natural"
    Lexer.NaturalFold      -> "Natural/fold"
    Lexer.OpenAngle        -> "<"
    Lexer.OpenBrace        -> "{"
    Lexer.OpenBracket      -> "<"
    Lexer.OpenParenthesis  -> "("
    Lexer.Or               -> "||"
    Lexer.Plus             -> "+"
    Lexer.Text             -> "Text"
    Lexer.TextLiteral _    -> "a text literal"
    Lexer.Then             -> "then"
    Lexer.Times            -> "*"
    Lexer.True_            -> "True"

grammar :: Grammar r (Parser r (Syntax FilePath))
grammar = mdo
    expression <- rule
        (   do  token Lexer.Lambda
                name <- label 
                token Lexer.Arrow
                body <- expression
                return (Syntax.Lambda name body)
        <|> do  bindings <- some1 binding
                token Lexer.In
                body <- expression
                return (Syntax.Let bindings body)
        <|> do  token Lexer.If
                predicate <- expression
                token Lexer.Then
                ifTrue <- expression
                token Lexer.Else
                ifFalse <- expression
                return (Syntax.If predicate ifTrue ifFalse)
        <|> do  annotated <- operatorExpression
                token Lexer.Colon
                annotation <- type_
                return (Syntax.Annotation annotated annotation)
        <|> do  operatorExpression
        )

    operatorExpression <- rule timesExpression

    let op token_ operator subExpression = do
            es <- subExpression `sepBy1` token token_
            return (foldl operator (NonEmpty.head es) (NonEmpty.tail es))

    timesExpression <- rule (op Lexer.Times Syntax.Times plusExpression)

    plusExpression <- rule (op Lexer.Plus Syntax.Plus orExpression)

    orExpression <- rule (op Lexer.Or Syntax.Or andExpression)

    andExpression <- rule (op Lexer.And Syntax.And appendExpression)

    appendExpression <- rule (op Lexer.Append Syntax.Append applicationExpression)

    applicationExpression <- rule
        (   do  es <- some1 fieldExpression
                return (foldl Syntax.Application (NonEmpty.head es) (NonEmpty.tail es))
        <|> do  token Lexer.Merge
                es <- some1 fieldExpression
                return (foldl Syntax.Application (Syntax.Merge (NonEmpty.head es)) (NonEmpty.tail es))
        )

    fieldExpression <- rule do
        record <- primitiveExpression
        fields <- many (do token Lexer.Dot; l <- label; return l)
        return (foldl Syntax.Field record fields)

    primitiveExpression <- rule
        (   do  name <- label
                return (Syntax.Variable name 0)
        <|> do  name <- label
                token Lexer.At
                index <- int
                return (Syntax.Variable name index)
        <|> do  a <- alternative
                return (Syntax.Alternative a)
        <|> do  token Lexer.OpenBracket
                elements <- expression `sepBy` token Lexer.Comma
                token Lexer.CloseBracket
                return (Syntax.List elements)
        <|> do  token Lexer.OpenBrace
                fieldValues <- fieldValue `sepBy` token Lexer.Comma
                token Lexer.CloseBrace
                return (Syntax.Record fieldValues)
        <|> do  token Lexer.True_
                return Syntax.True
        <|> do  token Lexer.False_
                return Syntax.False
        <|> do  n <- int
                return (Syntax.Natural (fromIntegral n))
        <|> do  token Lexer.NaturalFold
                return Syntax.NaturalFold
        <|> do  t <- text
                return (Syntax.Text t)
        <|> do  f <- file
                return (Syntax.Embed f)
        <|> do  token Lexer.OpenParenthesis
                e <- expression
                token Lexer.CloseParenthesis
                return e
        )

    binding <- rule
        (   do  token Lexer.Let
                name <- label
                token Lexer.Equals
                assignment <- expression
                return (Syntax.Binding name Nothing assignment)
        <|> do  token Lexer.Let
                name <- label
                token Lexer.Colon
                annotation <- type_
                token Lexer.Equals
                assignment <- expression
                return (Syntax.Binding name (Just annotation) assignment)
        )

    fieldValue <- rule do
        field <- label <|> alternative
        token Lexer.Equals
        value <- expression
        return (field, value)

    type_ <- rule do
        typeVariables <- many do
            token Lexer.Forall
            typeVariable <- label
            token Lexer.Dot
            return typeVariable
        t <- functionType
        return (foldr Type.Forall t typeVariables)

    functionType <- rule do
        ts <- applicationType `sepBy1` token Lexer.Arrow
        return (foldr Type.Function (NonEmpty.last ts) (NonEmpty.init ts))

    applicationType <- rule
        (   do  token Lexer.List
                t <- primitiveType
                return (Type.List t)
        <|> do  primitiveType
        )

    primitiveType <- rule
        (   do  token Lexer.Bool
                return Type.Bool
        <|> do  token Lexer.Natural
                return Type.Natural
        <|> do  token Lexer.Text
                return Type.Text
        <|> do  name <- label
                return (Type.Variable name)
        <|> do  token Lexer.OpenBrace
                fieldTypes <- fieldType `sepBy` token Lexer.Comma
                token Lexer.CloseBrace
                return (Type.Record (Type.Fields fieldTypes Nothing))
        <|> do  token Lexer.OpenAngle
                alternativeTypes <- alternativeType `sepBy` token Lexer.Comma
                token Lexer.CloseAngle
                return (Type.Union (Type.Alternatives alternativeTypes Nothing))
        <|> do  token Lexer.OpenParenthesis
                t <- type_
                token Lexer.CloseParenthesis
                return t
        )

    fieldType <- rule do
        field <- label
        token Lexer.Colon
        t <- type_
        return (field, t)

    alternativeType <- rule do
        a <- alternative
        token Lexer.Colon
        t <- type_
        return (a, t)

    return expression

-- | Parse a complete expression
parse
    :: String
    -- ^ Name of the input (used for error messages)
    -> Text
    -- ^ Tokens lexed from source code
    -> Either Text (Syntax FilePath)
parse inputName code = do
    tokens <- Lexer.lex inputName code

    case Earley.fullParses (Earley.parser grammar) tokens of
        ([], Report{..}) -> do
            let maybeOffset =
                    case unconsumed of
                        []               -> Nothing
                        locatedToken : _ -> Just (Lexer.start locatedToken)

            Left (Lexer.renderError inputName code maybeOffset)

        (result : _, _) -> do
            return result
