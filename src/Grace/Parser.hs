{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecursiveDo       #-}

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
import Control.Applicative.Combinators (endBy, sepBy)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty(..), some1)
import Data.Text (Text)
import Grace.Lexer (LocatedToken(LocatedToken), Token)
import Grace.Location (Location(..), Offset(..))
import Grace.Syntax (Binding(..), Syntax(..))
import Grace.Type (Type(..))
import Text.Earley (Grammar, Prod, Report(..), rule, (<?>))

import qualified Data.List.NonEmpty     as NonEmpty
import qualified Data.Text              as Text
import qualified Grace.Domain           as Domain
import qualified Grace.Lexer            as Lexer
import qualified Grace.Location         as Location
import qualified Grace.Monotype         as Monotype
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

matchDouble :: Token -> Maybe Double
matchDouble (Lexer.DoubleLiteral n) = Just n
matchDouble  _                      = Nothing

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

text :: Parser r Text
text = terminal matchText

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

locatedDouble :: Parser r (Offset, Double)
locatedDouble = locatedTerminal matchDouble

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
    Lexer.Alternatives     -> "Alternatives"
    Lexer.And              -> "&&"
    Lexer.Append           -> "++"
    Lexer.Arrow            -> "->"
    Lexer.At               -> "@"
    Lexer.Bar              -> "|"
    Lexer.Bool             -> "Bool"
    Lexer.CloseAngle       -> ">"
    Lexer.CloseBrace       -> "}"
    Lexer.CloseBracket     -> "]"
    Lexer.CloseParenthesis -> ")"
    Lexer.Colon            -> ":"
    Lexer.Comma            -> ","
    Lexer.Dash             -> "-"
    Lexer.Dot              -> "."
    Lexer.Double           -> "Double"
    Lexer.DoubleLiteral _  -> "a double literal"
    Lexer.DoubleShow       -> "Double/show"
    Lexer.Else             -> "else"
    Lexer.Equals           -> "="
    Lexer.False_           -> "False"
    Lexer.Fields           -> "Fields"
    Lexer.File _           -> "a file"
    Lexer.Forall           -> "forall"
    Lexer.If               -> "if"
    Lexer.In               -> "in"
    Lexer.Int _            -> "an integer literal"
    Lexer.Integer          -> "Integer"
    Lexer.IntegerEven      -> "Integer/even"
    Lexer.IntegerOdd       -> "Integer/odd"
    Lexer.Label _          -> "a label"
    Lexer.Lambda           -> "\\"
    Lexer.Let              -> "let"
    Lexer.List             -> "list"
    Lexer.ListFold         -> "List/fold"
    Lexer.ListLength       -> "List/length"
    Lexer.ListMap          -> "List/map"
    Lexer.Merge            -> "merge"
    Lexer.Natural          -> "Natural"
    Lexer.NaturalFold      -> "Natural/fold"
    Lexer.Null             -> "null"
    Lexer.OpenAngle        -> "<"
    Lexer.OpenBrace        -> "{"
    Lexer.OpenBracket      -> "<"
    Lexer.OpenParenthesis  -> "("
    Lexer.Optional         -> "List"
    Lexer.Or               -> "||"
    Lexer.Plus             -> "+"
    Lexer.Text             -> "Text"
    Lexer.TextLiteral _    -> "a text literal"
    Lexer.Then             -> "then"
    Lexer.Type             -> "Type"
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
                    node = Syntax.Operator l s operator r

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
        fields <- many (do token Lexer.Dot; l <- locatedRecordLabel; return l)

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

                return Syntax{ node = Syntax.Scalar (Syntax.Bool True), .. }

        <|> do  location <- locatedToken Lexer.False_

                return Syntax{ node = Syntax.Scalar (Syntax.Bool False), .. }

        <|> do  location <- locatedToken Lexer.Null

                return Syntax{ node = Syntax.Scalar Syntax.Null, .. }

        <|> do  let f sign (location, n) = Syntax{..}
                      where
                        node = Syntax.Scalar (Syntax.Double (sign n))

                sign <- (token Lexer.Dash *> pure negate) <|> pure id

                located <- locatedDouble

                return (f sign located)

        <|> do  let f (location, n) = Syntax{..}
                      where
                        node =
                            Syntax.Scalar (Syntax.Integer (fromIntegral (negate n)))

                token Lexer.Dash

                located <- locatedInt

                return (f located)

        <|> do  let f (location, n) = Syntax{..}
                      where
                        node = Syntax.Scalar (Syntax.Natural (fromIntegral n))

                located <- locatedInt

                return (f located)

        <|> do  location <- locatedToken Lexer.DoubleShow

                return Syntax{ node = Syntax.Builtin Syntax.DoubleShow, .. }

        <|> do  location <- locatedToken Lexer.ListFold

                return Syntax{ node = Syntax.Builtin Syntax.ListFold, .. }

        <|> do  location <- locatedToken Lexer.ListLength

                return Syntax{ node = Syntax.Builtin Syntax.ListLength, .. }

        <|> do  location <- locatedToken Lexer.ListMap

                return Syntax{ node = Syntax.Builtin Syntax.ListMap, .. }

        <|> do  location <- locatedToken Lexer.IntegerEven

                return Syntax{ node = Syntax.Builtin Syntax.IntegerEven, .. }

        <|> do  location <- locatedToken Lexer.IntegerOdd

                return Syntax{ node = Syntax.Builtin Syntax.IntegerOdd, .. }

        <|> do  location <- locatedToken Lexer.NaturalFold

                return Syntax{ node = Syntax.Builtin Syntax.NaturalFold, .. }

        <|> do  let f (location, t) = Syntax{..}
                      where
                        node = Syntax.Scalar (Syntax.Text t)

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

    recordLabel <- rule (label <|> alternative <|> text)

    locatedRecordLabel <- rule (locatedLabel <|> locatedText)

    fieldValue <- rule do
        field <- recordLabel
        token Lexer.Colon
        value <- expression
        return (field, value)

    domain <- rule
        (   do  token Lexer.Type
                return Domain.Type
        <|> do  token Lexer.Fields
                return Domain.Fields
        <|> do  token Lexer.Alternatives
                return Domain.Alternatives
        )

    quantifiedType <- rule do
        let forall (location, (typeVariableOffset, typeVariable), domain_) type_ =
                Type{..}
              where
                node = Type.Forall typeVariableOffset typeVariable domain_ type_

        locatedTypeVariables <- many do
            locatedForall <- locatedToken Lexer.Forall
            token Lexer.OpenParenthesis
            locatedTypeVariable <- locatedLabel
            token Lexer.Colon
            domain_ <- domain
            token Lexer.CloseParenthesis
            token Lexer.Dot
            return (locatedForall, locatedTypeVariable, domain_)
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

        <|> do  let f location t = Type{..}
                      where
                        node = Type.Optional t

                locatedOptional <- locatedToken Lexer.Optional
                t <- primitiveType
                return (f locatedOptional t)

        <|> do  primitiveType
        )

    primitiveType <- rule
        (   do  location <- locatedToken Lexer.Bool
                return Type{ node = Type.Scalar Monotype.Bool, .. }
        <|> do  location <- locatedToken Lexer.Double
                return Type{ node = Type.Scalar Monotype.Double, .. }
        <|> do  location <- locatedToken Lexer.Integer
                return Type{ node = Type.Scalar Monotype.Integer, .. }
        <|> do  location <- locatedToken Lexer.Natural
                return Type{ node = Type.Scalar Monotype.Natural, .. }
        <|> do  location <- locatedToken Lexer.Text
                return Type{ node = Type.Scalar Monotype.Text, .. }
        <|> do  let variable (location, name) = Type{..}
                      where
                        node = Type.VariableType name

                located <- locatedLabel
                return (variable located)
        <|> do  let record location fields = Type{..}
                      where
                        node = Type.Record fields

                locatedOpenBrace <- locatedToken Lexer.OpenBrace

                fieldTypes <- fieldType `endBy` token Lexer.Comma
                
                toFields <-
                    (   do  text_ <- recordLabel
                            return (\fs -> Type.Fields fs (Monotype.VariableFields text_))
                    <|> do  pure (\fs -> Type.Fields fs Monotype.EmptyFields)
                    <|> do  f <- fieldType
                            return (\fs -> Type.Fields (fs <> [ f ]) Monotype.EmptyFields)
                    )

                token Lexer.CloseBrace

                return (record locatedOpenBrace (toFields fieldTypes))
        <|> do  let union location alternatives = Type{..}
                      where
                        node = Type.Union alternatives

                locatedOpenAngle <- locatedToken Lexer.OpenAngle

                alternativeTypes <- alternativeType `endBy` token Lexer.Bar

                toAlternatives <-
                    (   do  text_ <- label
                            return (\as -> Type.Alternatives as (Monotype.VariableAlternatives text_))
                    <|> do  pure (\as -> Type.Alternatives as Monotype.EmptyAlternatives)
                    <|> do  a <- alternativeType
                            return (\as -> Type.Alternatives (as <> [ a ]) Monotype.EmptyAlternatives)
                    )

                token Lexer.CloseAngle
                return (union locatedOpenAngle (toAlternatives alternativeTypes))
        <|> do  token Lexer.OpenParenthesis
                t <- quantifiedType
                token Lexer.CloseParenthesis
                return t
        )

    fieldType <- rule do
        field <- recordLabel
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
                        []                -> Offset (Text.length code)
                        locatedToken_ : _ -> Lexer.start locatedToken_

            Left (Location.renderError "Invalid input - Parsing failed" Location{..})

        (result : _, _) -> do
            return result
