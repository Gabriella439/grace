{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecursiveDo       #-}

{-| This module contains the logic for parsing Grace files using @Earley@.

    The main reason for not using @attoparsec@ or @megaparsec@ is because
    LR parsers are easier to maintain due to not needing to left-factor the
    grammar.

    The main reason for not using @happy@ is because it uses a separate code
    generation step, which leads to worse type errors and poor support for
    interactive type-checking.
-}

module Grace.Parser
    ( -- * Parsing
      parse
      -- * Errors related to parsing
    , ParseError(..)
    ) where

import Control.Applicative (many, optional, some, (<|>))
import Control.Applicative.Combinators (endBy, sepBy)
import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Data.Functor (void, ($>))
import Data.List.NonEmpty (NonEmpty(..), some1)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Grace.Input (Input(..))
import Grace.Lexer (LocatedToken(LocatedToken), ParseError(..), Token)
import Grace.Location (Location(..), Offset(..))
import Grace.Syntax (Binding(..), Syntax(..))
import Grace.Type (Type(..))
import Text.Earley (Grammar, Prod, Report(..), rule, (<?>))

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Grace.Domain as Domain
import qualified Grace.Lexer as Lexer
import qualified Grace.Monotype as Monotype
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type
import qualified Text.Earley as Earley
import qualified Text.URI as URI

type Parser r = Prod r Text LocatedToken

matchLabel :: Token -> Maybe Text
matchLabel (Lexer.Label l) = Just l
matchLabel  _              = Nothing

matchAlternative :: Token -> Maybe Text
matchAlternative (Lexer.Alternative a) = Just a
matchAlternative  _                    = Nothing

matchReal :: Token -> Maybe Scientific
matchReal (Lexer.RealLiteral n) = Just n
matchReal  _                    = Nothing

matchInt :: Token -> Maybe Int
matchInt (Lexer.Int n) = Just n
matchInt  _            = Nothing

matchText :: Token -> Maybe Text
matchText (Lexer.TextLiteral t) = Just t
matchText  _                    = Nothing

matchFile :: Token -> Maybe FilePath
matchFile (Lexer.File f) = Just f
matchFile  _             = Nothing

matchURI :: Token -> Maybe URI.URI
matchURI (Lexer.URI t) = Just t
matchURI  _            = Nothing

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

locatedReal :: Parser r (Offset, Scientific)
locatedReal = locatedTerminal matchReal

locatedInt :: Parser r (Offset, Int)
locatedInt = locatedTerminal matchInt

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
    capture LocatedToken{ Lexer.token = actualToken, .. }
        | expectedToken == actualToken = Just start
        | otherwise                    = Nothing

-- | This render function is currently never used since `Location.renderError`
--   does not display expected tokens at all, but I maintain this anyway in
--   case someone wants to modify the code to display them.
render :: Token -> Text
render t = case t of
    Lexer.Alternative _    -> "an alternative"
    Lexer.Alternatives     -> "Alternatives"
    Lexer.And              -> "&&"
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
    Lexer.Real             -> "Real"
    Lexer.RealLiteral _    -> "a real number literal"
    Lexer.RealEqual        -> "Real/equal"
    Lexer.RealLessThan     -> "Real/lessThan"
    Lexer.RealNegate       -> "Real/negate"
    Lexer.RealShow         -> "Real/show"
    Lexer.Else             -> "else"
    Lexer.Equals           -> "="
    Lexer.Exists           -> "exists"
    Lexer.False_           -> "False"
    Lexer.Fields           -> "Fields"
    Lexer.File _           -> "a file"
    Lexer.Forall           -> "forall"
    Lexer.If               -> "if"
    Lexer.In               -> "in"
    Lexer.Int _            -> "an integer literal"
    Lexer.Integer          -> "Integer"
    Lexer.IntegerAbs       -> "Integer/clamp"
    Lexer.IntegerEven      -> "Integer/even"
    Lexer.IntegerNegate    -> "Integer/negate"
    Lexer.IntegerOdd       -> "Integer/odd"
    Lexer.JSON             -> "JSON"
    Lexer.JSONFold         -> "JSON/fold"
    Lexer.Label _          -> "a label"
    Lexer.Lambda           -> "\\"
    Lexer.Let              -> "let"
    Lexer.List             -> "list"
    Lexer.ListDrop         -> "List/drop"
    Lexer.ListEqual        -> "List/equal"
    Lexer.ListFold         -> "List/fold"
    Lexer.ListHead         -> "List/head"
    Lexer.ListIndexed      -> "List/indexed"
    Lexer.ListLast         -> "List/last"
    Lexer.ListLength       -> "List/length"
    Lexer.ListMap          -> "List/map"
    Lexer.ListReverse      -> "List/reverse"
    Lexer.ListTake         -> "List/take"
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
    Lexer.Question         -> "?"
    Lexer.Text             -> "Text"
    Lexer.TextEqual        -> "Text/equal"
    Lexer.TextLiteral _    -> "a text literal"
    Lexer.Then             -> "then"
    Lexer.Type             -> "Type"
    Lexer.Times            -> "*"
    Lexer.True_            -> "True"
    Lexer.URI _            -> "a URI"

grammar :: Grammar r (Parser r (Syntax Offset Input))
grammar = mdo
    expression <- rule
        (   do  location <- locatedToken Lexer.Lambda
                locatedNames <- some1 locatedLabel
                token Lexer.Arrow
                body0 <- expression

                return do
                    let cons (nameLocation, name) body = Syntax{..}
                          where
                            node = Syntax.Lambda nameLocation name body
                    foldr cons body0 locatedNames

        <|> do  bindings <- some1 binding
                token Lexer.In
                body <- expression

                return do
                    let location = nameLocation (NonEmpty.head bindings)
                    let node = Syntax.Let bindings body
                    Syntax{..}

        <|> do  location <- locatedToken Lexer.If
                predicate <- expression
                token Lexer.Then
                ifTrue <- expression
                token Lexer.Else
                ifFalse <- expression

                return do
                    let node = Syntax.If predicate ifTrue ifFalse
                    Syntax{..}

        <|> do  ~annotated@Syntax{ location } <- operatorExpression
                token Lexer.Colon
                annotation <- quantifiedType

                return do
                    let node = Syntax.Annotation annotated annotation
                    Syntax{..}

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

    andExpression <- rule (op Lexer.And Syntax.And applicationExpression)

    let application left@Syntax{ location } right = Syntax{..}
          where
            node = Syntax.Application left right

    applicationExpression <- rule
        (   do  es <- some1 fieldExpression
                return (foldl application (NonEmpty.head es) (NonEmpty.tail es))
        <|> do  location <- locatedToken Lexer.Merge
                ~(e :| es) <- some1 fieldExpression

                return do
                    let nil = Syntax{ node = Syntax.Merge e, .. }
                    foldl application nil es
        )

    fieldExpression <- rule do
        let field Syntax{ location } l (fieldOffset, r) = Syntax{..}
              where
                node = Syntax.Field l fieldOffset r

        record <- primitiveExpression
        fields <- many (do token Lexer.Dot; l <- locatedRecordLabel; return l)

        return (foldl (field record) record fields)

    primitiveExpression <- rule
        (   do  ~(location, name) <- locatedLabel

                return do
                    let node = Syntax.Variable name 0
                    Syntax{..}

        <|> do  ~(location, name) <- locatedLabel
                token Lexer.At
                index <- int

                return do
                    let node = Syntax.Variable name index
                    Syntax{..}

        <|> do  ~(location, alt) <- locatedAlternative

                return do
                    let node = Syntax.Alternative alt
                    Syntax{..}

        <|> do  location <- locatedToken Lexer.OpenBracket
                optional (token Lexer.Comma)
                elements <- expression `sepBy` token Lexer.Comma
                optional (token Lexer.Comma)
                token Lexer.CloseBracket

                return do
                    let node = Syntax.List (Seq.fromList elements)
                    Syntax{..}

        <|> do  location <- locatedToken Lexer.OpenBrace
                optional (token Lexer.Comma)
                fieldValues <- fieldValue `sepBy` token Lexer.Comma
                optional (token Lexer.Comma)
                token Lexer.CloseBrace

                return do
                    let node = Syntax.Record fieldValues
                    Syntax{..}

        <|> do  location <- locatedToken Lexer.True_

                return Syntax{ node = Syntax.Scalar (Syntax.Bool True), .. }

        <|> do  location <- locatedToken Lexer.False_

                return Syntax{ node = Syntax.Scalar (Syntax.Bool False), .. }

        <|> do  location <- locatedToken Lexer.Null

                return Syntax{ node = Syntax.Scalar Syntax.Null, .. }

        <|> do  sign <- (token Lexer.Dash $> negate) <|> pure id

                ~(location, n) <- locatedReal

                return do
                    let node = Syntax.Scalar (Syntax.Real (sign n))
                    Syntax{..}

        <|> do  token Lexer.Dash

                ~(location, n) <- locatedInt

                return do
                    let node =
                            Syntax.Scalar (Syntax.Integer (fromIntegral (negate n)))
                    Syntax{..}

        <|> do  ~(location, n) <- locatedInt

                return do
                    let node = Syntax.Scalar (Syntax.Natural (fromIntegral n))
                    Syntax{..}

        <|> do  location <- locatedToken Lexer.RealEqual

                return Syntax{ node = Syntax.Builtin Syntax.RealEqual, .. }

        <|> do  location <- locatedToken Lexer.RealLessThan

                return Syntax{ node = Syntax.Builtin Syntax.RealLessThan, .. }

        <|> do  location <- locatedToken Lexer.RealNegate

                return Syntax{ node = Syntax.Builtin Syntax.RealNegate, .. }

        <|> do  location <- locatedToken Lexer.RealShow

                return Syntax{ node = Syntax.Builtin Syntax.RealShow, .. }

        <|> do  location <- locatedToken Lexer.ListDrop

                return Syntax{ node = Syntax.Builtin Syntax.ListDrop, .. }

        <|> do  location <- locatedToken Lexer.ListEqual

                return Syntax{ node = Syntax.Builtin Syntax.ListEqual, .. }

        <|> do  location <- locatedToken Lexer.ListFold

                return Syntax{ node = Syntax.Builtin Syntax.ListFold, .. }

        <|> do  location <- locatedToken Lexer.ListHead

                return Syntax{ node = Syntax.Builtin Syntax.ListHead, .. }

        <|> do  location <- locatedToken Lexer.ListIndexed

                return Syntax{ node = Syntax.Builtin Syntax.ListIndexed, .. }

        <|> do  location <- locatedToken Lexer.ListLast

                return Syntax{ node = Syntax.Builtin Syntax.ListLast, .. }

        <|> do  location <- locatedToken Lexer.ListLength

                return Syntax{ node = Syntax.Builtin Syntax.ListLength, .. }

        <|> do  location <- locatedToken Lexer.ListMap

                return Syntax{ node = Syntax.Builtin Syntax.ListMap, .. }

        <|> do  location <- locatedToken Lexer.ListReverse

                return Syntax{ node = Syntax.Builtin Syntax.ListReverse, .. }

        <|> do  location <- locatedToken Lexer.ListTake

                return Syntax{ node = Syntax.Builtin Syntax.ListTake, .. }

        <|> do  location <- locatedToken Lexer.IntegerAbs

                return Syntax{ node = Syntax.Builtin Syntax.IntegerAbs, .. }

        <|> do  location <- locatedToken Lexer.IntegerEven

                return Syntax{ node = Syntax.Builtin Syntax.IntegerEven, .. }

        <|> do  location <- locatedToken Lexer.IntegerNegate

                return Syntax{ node = Syntax.Builtin Syntax.IntegerNegate, .. }

        <|> do  location <- locatedToken Lexer.IntegerOdd

                return Syntax{ node = Syntax.Builtin Syntax.IntegerOdd, .. }

        <|> do  location <- locatedToken Lexer.JSONFold

                return Syntax{ node = Syntax.Builtin Syntax.JSONFold, .. }

        <|> do  location <- locatedToken Lexer.NaturalFold

                return Syntax{ node = Syntax.Builtin Syntax.NaturalFold, .. }

        <|> do  location <- locatedToken Lexer.TextEqual

                return Syntax{ node = Syntax.Builtin Syntax.TextEqual, .. }

        <|> do  ~(location, t) <- locatedText

                return do
                    let node = Syntax.Scalar (Syntax.Text t)
                    Syntax{..}

        <|> do  ~(location, file) <- locatedFile

                return do
                    let node = Syntax.Embed (Path file)
                    Syntax{..}

        <|> do  ~(location, uri) <- locatedURI

                return do
                    let node = Syntax.Embed (URI uri)
                    Syntax{..}

        <|> do  token Lexer.OpenParenthesis
                e <- expression
                token Lexer.CloseParenthesis
                return e
        )

    binding <- rule
        (   do  nameLocation <- locatedToken Lexer.Let
                name <- label
                token Lexer.Equals
                assignment <- expression

                return do
                    let annotation = Nothing
                    Syntax.Binding{..}

        <|> do  nameLocation <- locatedToken Lexer.Let
                name <- label
                token Lexer.Colon
                annotation <- fmap Just quantifiedType
                token Lexer.Equals
                assignment <- expression

                return Syntax.Binding{..}
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
        let forall (forallOrExists, location, (typeVariableOffset, typeVariable), domain_) type_ =
                Type{..}
              where
                node = forallOrExists typeVariableOffset typeVariable domain_ type_

        fss <- many
            (   do  location <- locatedToken Lexer.Forall
                    fs <- some do
                        token Lexer.OpenParenthesis
                        locatedTypeVariable <- locatedLabel
                        token Lexer.Colon
                        domain_ <- domain
                        token Lexer.CloseParenthesis
                        return \location_ -> forall (Type.Forall, location_, locatedTypeVariable, domain_)
                    token Lexer.Dot
                    return (map ($ location) fs)
            <|> do  location <- locatedToken Lexer.Exists
                    fs <- some do
                        token Lexer.OpenParenthesis
                        locatedTypeVariable <- locatedLabel
                        token Lexer.Colon
                        domain_ <- domain
                        token Lexer.CloseParenthesis

                        return \location_ -> forall (Type.Exists, location_, locatedTypeVariable, domain_)
                    token Lexer.Dot
                    return (map ($ location) fs)
            )
        t <- functionType
        return (foldr ($) t (concat fss))

    functionType <- rule do
        let function _A@Type{ location } _B = Type{..}
              where
                node = Type.Function _A _B

        ts <- applicationType `sepBy1` token Lexer.Arrow
        return (foldr function (NonEmpty.last ts) (NonEmpty.init ts))

    applicationType <- rule
        (   do  location <- locatedToken Lexer.List
                t <- primitiveType

                return do
                    let node = Type.List t
                    Type{..}

        <|> do  location <- locatedToken Lexer.Optional
                t <- primitiveType

                return do
                    let node = Type.Optional t
                    Type{..}

        <|> do  primitiveType
        )

    primitiveType <- rule
        (   do  location <- locatedToken Lexer.Question
                return Type{ node = Type.TypeHole, .. }
        <|> do  location <- locatedToken Lexer.Bool
                return Type{ node = Type.Scalar Monotype.Bool, .. }
        <|> do  location <- locatedToken Lexer.Real
                return Type{ node = Type.Scalar Monotype.Real, .. }
        <|> do  location <- locatedToken Lexer.Integer
                return Type{ node = Type.Scalar Monotype.Integer, .. }
        <|> do  location <- locatedToken Lexer.JSON
                return Type{ node = Type.Scalar Monotype.JSON, .. }
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

                optional (token Lexer.Comma)

                fieldTypes <- fieldType `endBy` token Lexer.Comma

                toFields <-
                    (   do  text_ <- recordLabel
                            pure (\fs -> Type.Fields fs (Monotype.VariableFields text_))
                    <|> do  token Lexer.Question
                            pure (\fs -> Type.Fields fs Monotype.HoleFields)
                    <|> do  pure (\fs -> Type.Fields fs Monotype.EmptyFields)
                    <|> do  f <- fieldType
                            pure (\fs -> Type.Fields (fs <> [ f ]) Monotype.EmptyFields)
                    )

                optional (token Lexer.Comma)

                token Lexer.CloseBrace

                return (record locatedOpenBrace (toFields fieldTypes))
        <|> do  let union location alternatives = Type{..}
                      where
                        node = Type.Union alternatives

                locatedOpenAngle <- locatedToken Lexer.OpenAngle

                optional (token Lexer.Bar)

                alternativeTypes <- alternativeType `endBy` token Lexer.Bar

                toAlternatives <-
                    (   do  text_ <- label
                            return (\as -> Type.Alternatives as (Monotype.VariableAlternatives text_))
                    <|> do  token Lexer.Question
                            pure (\as -> Type.Alternatives as Monotype.HoleAlternatives)
                    <|> do  pure (\as -> Type.Alternatives as Monotype.EmptyAlternatives)
                    <|> do  a <- alternativeType
                            return (\as -> Type.Alternatives (as <> [ a ]) Monotype.EmptyAlternatives)
                    )

                optional (token Lexer.Bar)

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
    -- ^ Source code
    -> Either ParseError (Syntax Offset Input)
parse name code = do
    tokens <- Lexer.lex name code

    case Earley.fullParses (Earley.parser grammar) tokens of
        ([], Report{..}) -> do
            let offset =
                    case unconsumed of
                        []                -> Offset (Text.length code)
                        locatedToken_ : _ -> Lexer.start locatedToken_

            Left (ParsingFailed (Location{..}))

        (result : _, _) -> do
            return result
