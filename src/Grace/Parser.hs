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
import Data.List.NonEmpty (NonEmpty(..), some1)
import Data.Text (Text)
import Grace.Lexer (LocatedToken(LocatedToken), Token)
import Grace.Syntax (Binding(..), Location(..), Offset, Syntax(..))
import Grace.Type (Type(..))
import Text.Earley (Grammar, Prod, Report(..), rule, (<?>))

import qualified Data.List.NonEmpty     as NonEmpty
import qualified Data.Text              as Text
import qualified Grace.Lexer            as Lexer
import qualified Grace.Syntax           as Syntax
import qualified Grace.Type             as Type
import qualified Text.Earley            as Earley

type Parser r = Prod r Text LocatedToken

matchLabel :: Token -> Maybe Text
matchLabel (Lexer.Label l) = Just l
matchLabel  _              = Nothing

matchAlternative :: Token -> Maybe Text
matchAlternative (Lexer.Alternative a) = Just a
matchAlternative  _                    = Nothing

matchInt :: Token -> Maybe Int
matchInt (Lexer.Int n) = Just n
matchInt  _            = Nothing

matchText :: Token -> Maybe Text
matchText (Lexer.TextLiteral t) = Just t
matchText  _                    = Nothing

matchFile :: Token -> Maybe FilePath
matchFile (Lexer.File f) = Just f
matchFile  _             = Nothing

terminal :: (Token -> Maybe a) -> Parser r a
terminal match = Earley.terminal match'
  where
    match' locatedToken_ = match (Lexer.token locatedToken_)

label :: Parser r Text
label = terminal matchLabel

alternative :: Parser r Text
alternative = terminal matchAlternative

int :: Parser r Int
int = terminal matchInt

token :: Token -> Parser r ()
token t = void (Earley.satisfy predicate <?> render t)
  where
    predicate locatedToken_ = Lexer.token locatedToken_ == t

locatedTerminal :: (Token -> Maybe a) -> Parser r (Offset, a)
locatedTerminal match = Earley.terminal match'
  where
    match' locatedToken_@LocatedToken{ start }  = do
      a <- match (Lexer.token locatedToken_)
      return (start, a)

locatedLabel :: Parser r (Offset, Text)
locatedLabel = locatedTerminal matchLabel

locatedAlternative :: Parser r (Offset, Text)
locatedAlternative = locatedTerminal matchAlternative

locatedInt :: Parser r (Offset, Int)
locatedInt = locatedTerminal matchInt

locatedText :: Parser r (Offset, Text)
locatedText = locatedTerminal matchText

locatedFile :: Parser r (Offset, FilePath)
locatedFile = locatedTerminal matchFile

locatedToken :: Token -> Parser r Offset
locatedToken expectedToken =
    Earley.terminal capture <?> render expectedToken
  where
    capture LocatedToken{ Lexer.token = actualToken, .. }
        | expectedToken == actualToken = Just start
        | otherwise                    = Nothing

render :: Token -> Text
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

grammar :: Grammar r (Parser r (Syntax Offset FilePath))
grammar = mdo
    expression <- rule
        (   do  let f location (nameLocation, name) body = Syntax{..}
                      where
                        node = Syntax.Lambda nameLocation name body

                lambdaOffset <- locatedToken Lexer.Lambda
                locatedName <- locatedLabel 
                token Lexer.Arrow
                body <- expression
                return (f lambdaOffset locatedName body)

        <|> do  let f bindings body = Syntax{..}
                      where
                        location = nameLocation (NonEmpty.head bindings)
                        node = Syntax.Let bindings body

                bindings <- some1 binding
                token Lexer.In
                body <- expression

                return (f bindings body)

        <|> do  let f location predicate ifTrue ifFalse = Syntax{..}
                      where
                        node = Syntax.If predicate ifTrue ifFalse

                ifOffset <- locatedToken Lexer.If
                predicate <- expression
                token Lexer.Then
                ifTrue <- expression
                token Lexer.Else
                ifFalse <- expression

                return (f ifOffset predicate ifTrue ifFalse)

        <|> do  let f annotated@Syntax{ location } annotation = Syntax{..}
                      where
                        node = Syntax.Annotation annotated annotation

                annotated <- operatorExpression
                token Lexer.Colon
                annotation <- quantifiedType

                return (f annotated annotation)

        <|> do  operatorExpression
        )

    operatorExpression <- rule timesExpression

    let op token_ operator subExpression = do
            let snoc l@Syntax{ location } (s, r) = Syntax{..}
                  where
                    node = operator l s r

            e0 <- subExpression

            ses <- many do
                s <- locatedToken token_
                e <- subExpression;
                return (s, e)

            return (foldl snoc e0 ses)

    timesExpression <- rule (op Lexer.Times Syntax.Times plusExpression)

    plusExpression <- rule (op Lexer.Plus Syntax.Plus orExpression)

    orExpression <- rule (op Lexer.Or Syntax.Or andExpression)

    andExpression <- rule (op Lexer.And Syntax.And appendExpression)

    appendExpression <- rule (op Lexer.Append Syntax.Append applicationExpression)

    let application left@Syntax{ location } right = Syntax{..}
          where
            node = Syntax.Application left right

    applicationExpression <- rule
        (   do  es <- some1 fieldExpression
                return (foldl application (NonEmpty.head es) (NonEmpty.tail es))
        <|> do  let f location (e :| es)  = foldl application nil es
                      where
                          nil = Syntax{ node = Syntax.Merge e, .. }

                locatedMerge <- locatedToken Lexer.Merge
                es <- some1 fieldExpression
                return (f locatedMerge es)
        )

    fieldExpression <- rule do
        let field Syntax{ location } l (fieldOffset, r) = Syntax{..}
              where
                node = Syntax.Field l fieldOffset r

        record <- primitiveExpression
        fields <- many (do token Lexer.Dot; l <- locatedLabel; return l)

        return (foldl (field record) record fields)

    primitiveExpression <- rule
        (   do  let f (location, name) = Syntax{..}
                      where
                        node = Syntax.Variable name 0

                locatedName <- locatedLabel

                return (f locatedName)

        <|> do  let f (location, name) index = Syntax{..}
                      where
                        node = Syntax.Variable name index

                locatedName <- locatedLabel
                token Lexer.At
                index <- int

                return (f locatedName index)

        <|> do  let f (location, alt) = Syntax{..}
                      where
                        node = Syntax.Alternative alt

                located <- locatedAlternative

                return (f located)

        <|> do  let f location elements = Syntax{..}
                      where
                        node = Syntax.List elements

                locatedOpenBracket <- locatedToken Lexer.OpenBracket
                elements <- expression `sepBy` token Lexer.Comma
                token Lexer.CloseBracket

                return (f locatedOpenBracket elements)

        <|> do  let f location fieldValues = Syntax{..}
                      where
                        node = Syntax.Record fieldValues

                locatedOpenBrace <- locatedToken Lexer.OpenBrace
                fieldValues <- fieldValue `sepBy` token Lexer.Comma
                token Lexer.CloseBrace

                return (f locatedOpenBrace fieldValues)

        <|> do  location <- locatedToken Lexer.True_

                return Syntax{ node = Syntax.True, .. }

        <|> do  location <- locatedToken Lexer.False_

                return Syntax{ node = Syntax.False, .. }

        <|> do  let f (location, n) = Syntax{..}
                      where
                        node = Syntax.Natural (fromIntegral n)

                located <- locatedInt

                return (f located)

        <|> do  location <- locatedToken Lexer.NaturalFold

                return Syntax{ node = Syntax.NaturalFold, .. }

        <|> do  let f (location, t) = Syntax{..}
                      where
                        node = Syntax.Text t

                located <- locatedText

                return (f located)

        <|> do  let f (location, file) = Syntax{..}
                      where
                        node = Syntax.Embed file

                located <- locatedFile

                return (f located)

        <|> do  token Lexer.OpenParenthesis
                e <- expression
                token Lexer.CloseParenthesis
                return e
        )

    binding <- rule
        (   do  let f location name assignment = Syntax.Binding{..}
                      where
                        nameLocation = location
                        annotation = Nothing
                        
                locatedLet <- locatedToken Lexer.Let
                name <- label
                token Lexer.Equals
                assignment <- expression

                return (f locatedLet name assignment)

        <|> do  let f location name annotation assignment = Syntax.Binding{..}
                      where
                        nameLocation = location

                locatedLet <- locatedToken Lexer.Let
                name <- label
                token Lexer.Colon
                annotation <- fmap Just quantifiedType
                token Lexer.Equals
                assignment <- expression
                return (f locatedLet name annotation assignment)
        )

    fieldValue <- rule do
        field <- label <|> alternative
        token Lexer.Equals
        value <- expression
        return (field, value)

    quantifiedType <- rule do
        let forall (location, (typeVariableOffset, typeVariable)) type_ =
                Type{..}
              where
                node = Type.Forall typeVariableOffset typeVariable type_

        locatedTypeVariables <- many do
            locatedForall <- locatedToken Lexer.Forall
            locatedTypeVariable <- locatedLabel
            token Lexer.Dot
            return (locatedForall, locatedTypeVariable)
        t <- functionType
        return (foldr forall t locatedTypeVariables)

    functionType <- rule do
        let function _A@Type{ location } _B = Type{..}
              where
                node = Type.Function _A _B

        ts <- applicationType `sepBy1` token Lexer.Arrow
        return (foldr function (NonEmpty.last ts) (NonEmpty.init ts))

    applicationType <- rule
        (   do  let f location t = Type{..}
                      where
                        node = Type.List t

                locatedList <- locatedToken Lexer.List
                t <- primitiveType
                return (f locatedList t)
        <|> do  primitiveType
        )

    primitiveType <- rule
        (   do  location <- locatedToken Lexer.Bool
                return Type{ node = Type.Bool, .. }
        <|> do  location <- locatedToken Lexer.Natural
                return Type{ node = Type.Natural, .. }
        <|> do  location <- locatedToken Lexer.Text
                return Type{ node = Type.Text, .. }
        <|> do  let variable (location, name) = Type{..}
                      where
                        node = Type.Variable name

                located <- locatedLabel
                return (variable located)
        <|> do  let record location fieldTypes = Type{..}
                      where
                        node = Type.Record (Type.Fields fieldTypes Nothing)

                locatedOpenBrace <- locatedToken Lexer.OpenBrace
                fieldTypes <- fieldType `sepBy` token Lexer.Comma
                token Lexer.CloseBrace
                return (record locatedOpenBrace fieldTypes)
        <|> do  let union location alternativeTypes = Type{..}
                      where
                        node =
                            Type.Union (Type.Alternatives alternativeTypes Nothing)

                locatedOpenAngle <- locatedToken Lexer.OpenAngle
                alternativeTypes <- alternativeType `sepBy` token Lexer.Comma
                token Lexer.CloseAngle
                return (union locatedOpenAngle alternativeTypes)
        <|> do  token Lexer.OpenParenthesis
                t <- quantifiedType
                token Lexer.CloseParenthesis
                return t
        )

    fieldType <- rule do
        field <- label
        token Lexer.Colon
        t <- quantifiedType
        return (field, t)

    alternativeType <- rule do
        a <- alternative
        token Lexer.Colon
        t <- quantifiedType
        return (a, t)

    return expression

-- | Parse a complete expression
parse
    :: String
    -- ^ Name of the input (used for error messages)
    -> Text
    -- ^ Tokens lexed from source code
    -> Either Text (Syntax Offset FilePath)
parse name code = do
    tokens <- Lexer.lex name code

    case Earley.fullParses (Earley.parser grammar) tokens of
        ([], Report{..}) -> do
            let offset =
                    case unconsumed of
                        []                -> Text.length code
                        locatedToken_ : _ -> Lexer.start locatedToken_

            Left (Syntax.renderError "Invalid input - Parsing failed" Location{..})

        (result : _, _) -> do
            return result
