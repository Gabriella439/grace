{-| This module provides label-related logic shared by the parser and
    pretty-printer
-}
module Grace.Label
    ( -- * Reserved tokens
      reservedLabels
    , reservedRecordLabels

      -- * Character matching
    , isLabel0
    , isAlternativeLabel0
    , isLabel

      -- * Label quoting
    , validLabel
    , validRecordLabel
    , validAlternativeLabel
    ) where

import Data.HashSet (HashSet)
import Data.Text (Text)

import qualified Data.Char as Char
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

-- | Is this character valid as the first character in an unquoted label?
isLabel0 :: Char -> Bool
isLabel0 c = Char.isLower c || c == '_'

-- | Is this character valid as the first character in an unquoted alternative
-- label?
isAlternativeLabel0 :: Char -> Bool
isAlternativeLabel0 = Char.isUpper

-- | Is this character valid as a follow-up character in an unquoted label?
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
            &&  not (HashSet.member text_ reservedLabels)

-- | Returns `True` if the given record label is valid when unquoted
validRecordLabel :: Text -> Bool
validRecordLabel text_ =
    case Text.uncons text_ of
        Nothing     -> False
        Just (h, t) ->
                isLabel0 h
            &&  Text.all isLabel t
            &&  not (HashSet.member text_ reservedRecordLabels)

-- | Returns `True` if the given alternative label is valid when unquoted
validAlternativeLabel :: Text -> Bool
validAlternativeLabel text_ =
    case Text.uncons text_ of
        Nothing ->
            False
        Just (h, t) ->
                isAlternativeLabel0 h
            &&  Text.all isLabel t
            &&  not (HashSet.member text_ reservedLabels)

-- | Reserved tokens, which can't be used for labels unless they are quoted
reservedLabels :: HashSet Text
reservedLabels =
    HashSet.union
    (HashSet.fromList [ "some", "null", "true", "false" ])
    reservedRecordLabels

reservedRecordLabels :: HashSet Text
reservedRecordLabels = HashSet.fromList
    [ "Alternatives"
    , "Bool"
    , "Fields"
    , "Integer"
    , "List"
    , "Natural"
    , "Optional"
    , "Real"
    , "Text"
    , "Type"
    , "abs"
    , "else"
    , "export"
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
    , "of"
    , "prompt"
    , "read"
    , "reveal"
    , "show"
    , "then"
    , "yaml"
    ]
