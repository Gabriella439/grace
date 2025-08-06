{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-| This module stores the `Type` type representing polymorphic types and
    utilities for operating on `Type`s
-}
module Grace.Type
    ( -- * Types
      Type(..)
    , Record(..)
    , Union(..)

      -- * Utilities
    , solveType
    , solveFields
    , solveAlternatives
    , typeFreeIn
    , fieldsFreeIn
    , alternativesFreeIn
    , substituteType
    , substituteFields
    , substituteAlternatives
    , defaultTo

      -- * Pretty-printing
    , prettyRecordLabel
    , prettyAlternativeLabel
    , prettyTextLiteral
    , prettyTextBody
    ) where

import Control.Lens (Plated(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Generics.Product (the)
import Data.Generics.Sum (_As)
import Data.String (IsString(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Grace.Domain (Domain)
import Grace.Existential (Existential)
import Grace.Pretty (Pretty(..), builtin, keyword, label, operator, punctuation)
import Language.Haskell.TH.Syntax (Lift)
import Prelude hiding (lines)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)

import Grace.Monotype
    (Monotype, RemainingAlternatives(..), RemainingFields(..), Scalar(..))

import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Grace.Domain as Domain
import qualified Grace.Monotype as Monotype
import qualified Prettyprinter as Pretty

import {-# SOURCE #-} qualified Grace.Parser as Parser

{- $setup

   >>> :set -XOverloadedStrings
   >>> :set -XTypeApplications
-}

-- | A potentially polymorphic type
data Type s
    = VariableType { location :: s, name :: Text }
    -- ^ Type variable
    --
    -- >>> pretty @(Type ()) (VariableType () "a")
    -- a
    | UnsolvedType { location :: s, existential :: Existential Monotype }
    -- ^ A placeholder variable whose type has not yet been inferred
    --
    -- >>> pretty @(Type ()) (UnsolvedType () 0)
    -- a?
    | Forall { location :: s, nameLocation :: s, name :: Text, domain :: Domain, type_ :: Type s }
    -- ^ Universally quantified type
    --
    -- >>> pretty @(Type ()) (Forall () () "a" Domain.Type "a")
    -- forall (a : Type) . a
    | Function { location :: s, input :: Type s, output :: Type s }
    -- ^ Function type
    --
    -- >>> pretty @(Type ()) (Function () "a" "b")
    -- a -> b
    | Optional { location :: s, type_ :: Type s }
    -- ^ Optional type
    --
    -- >>> pretty @(Type ()) (Optional () "a")
    -- Optional a
    | List { location :: s, type_ :: Type s }
    -- ^ List type
    --
    -- >>> pretty @(Type ()) (List () "a")
    -- List a
    | Record { location :: s, fields :: Record s }
    -- ^ Record type
    --
    -- >>> pretty @(Type ()) (Record () (Fields [("x", "X"), ("y", "Y")] Monotype.EmptyFields))
    -- { x: X, y: Y }
    -- >>> pretty @(Type ()) (Record () (Fields [("x", "X"), ("y", "Y")] (Monotype.UnsolvedFields 0)))
    -- { x: X, y: Y, a? }
    | Union { location :: s, alternatives :: Union s }
    -- ^ Union type
    --
    -- >>> pretty @(Type ()) (Union () (Alternatives [("X", "X"), ("Y", "Y")] Monotype.EmptyAlternatives))
    -- < X: X | Y: Y >
    -- >>> pretty @(Type ()) (Union () (Alternatives [("X", "X"), ("Y", "Y")] (Monotype.UnsolvedAlternatives 0)))
    -- < X: X | Y: Y | a? >
    | Scalar { location :: s, scalar :: Scalar }
    deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance IsString (Type ()) where
    fromString string = VariableType{ name = fromString string, location = () }

instance Pretty (Type s) where
    pretty = prettyQuantifiedType

instance Plated (Type s) where
    plate onType type_ =
        case type_ of
            VariableType{..} -> do
                pure VariableType{..}
            UnsolvedType{..} -> do
                pure UnsolvedType{..}
            Forall{ type_ = oldType, .. } -> do
                newType <- onType oldType
                return Forall{ type_ = newType, .. }
            Function{ input = oldInput, output = oldOutput, .. } -> do
                newInput <- onType oldInput
                newOutput <- onType oldOutput
                return Function{ input = newInput, output = newOutput, .. }
            Optional{ type_ = oldType, .. } -> do
                newType <- onType oldType
                return Optional{ type_ = newType, .. }
            List{ type_ = oldType, .. } -> do
                newType <- onType oldType
                return List{ type_ = newType, .. }
            Record{ fields = Fields oldFieldTypes remainingFields, .. } -> do
                let onPair (field, oldType) = do
                        newType <- onType oldType
                        return (field, newType)
                newFieldTypes <- traverse onPair oldFieldTypes
                return Record{ fields = Fields newFieldTypes remainingFields, .. }
            Union{ alternatives = Alternatives oldAlternativeTypes remainingAlternatives, .. } -> do
                let onPair (alternative, oldType) = do
                        newType <- onType oldType
                        return (alternative, newType)
                newAlternativeTypes <- traverse onPair oldAlternativeTypes
                return Union{ alternatives = Alternatives newAlternativeTypes remainingAlternatives, .. }
            Scalar{..} -> do
                pure Scalar{..}

-- | A potentially polymorphic record type
data Record s = Fields [(Text, Type s)] RemainingFields
    deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

instance Pretty (Record s) where
    pretty = prettyRecordType

-- | A potentially polymorphic union type
data Union s = Alternatives [(Text, Type s)] RemainingAlternatives
    deriving stock (Eq, Functor, Foldable, Generic, Lift, Show, Traversable)

instance Pretty (Union s) where
    pretty = prettyUnionType

{-| This function should not be exported or generally used because it does not
    handle the `location` field correctly.  It is only really safe to use within
    one of the @solve*@ functions
-}
fromMonotype :: Monotype -> Type ()
fromMonotype monotype =
    case monotype of
        Monotype.VariableType name ->
            VariableType{..}
        Monotype.UnsolvedType existential ->
            UnsolvedType{..}
        Monotype.Function input output ->
            Function{ input = fromMonotype input, output = fromMonotype output, .. }
        Monotype.Optional type_ ->
            Optional{ type_ = fromMonotype type_, .. }
        Monotype.List type_ ->
            List{ type_ = fromMonotype type_, .. }
        Monotype.Record (Monotype.Fields kτs ρ) ->
            Record{ fields = Fields (map (second fromMonotype) kτs) ρ, .. }
        Monotype.Union (Monotype.Alternatives kτs ρ) ->
            Union{ alternatives = Alternatives (map (second fromMonotype) kτs) ρ, .. }
        Monotype.Scalar scalar ->
            Scalar{..}
  where
    location = ()

instance Pretty Monotype where
    pretty = pretty . fromMonotype

{-| Substitute a `Type` by replacing all occurrences of the given unsolved
    variable with a `Monotype`
-}
solveType :: Existential Monotype -> Monotype -> Type s -> Type s
solveType unsolved monotype = Lens.transform transformType
  where
    transformType UnsolvedType{..}
        | unsolved == existential =
            fmap (\_ -> location) (fromMonotype monotype)

    transformType type_ =
        type_

{-| Substitute a `Type` by replacing all occurrences of the given unsolved
    fields variable with a t`Monotype.Record`
-}
solveFields
    :: Existential Monotype.Record -> Monotype.Record -> Type s -> Type s
solveFields unsolved (Monotype.Fields fieldMonotypes fields) =
    Lens.transform transformType
  where
    transformType Record{ fields = Fields fieldTypes (UnsolvedFields existential), .. }
        | unsolved == existential =
            Record{ fields = Fields fieldTypes' fields, .. }
      where
        fieldTypes' =
            fieldTypes <> map transformPair fieldMonotypes

        transformPair (field, monotype) =
            (field, fmap (\_ -> location) (fromMonotype monotype))

    transformType type_ =
        type_

{-| Substitute a `Type` by replacing all occurrences of the given unsolved
    alternatives variable with a t`Monotype.Union`
-}
solveAlternatives
    :: Existential Monotype.Union -> Monotype.Union -> Type s -> Type s
solveAlternatives unsolved (Monotype.Alternatives alternativeMonotypes alternatives) =
    Lens.transform transformType
  where
    transformType Union{ alternatives = Alternatives alternativeTypes (UnsolvedAlternatives existential), .. }
        | unsolved == existential =
            Union{ alternatives = Alternatives alternativeTypes' alternatives, .. }
      where
        alternativeTypes' =
            alternativeTypes <> map transformPair alternativeMonotypes

        transformPair (alternative, monotype) =
            (alternative, fmap (\_ -> location) (fromMonotype monotype))

    transformType type_ =
        type_

{-| Replace all occurrences of a variable within one `Type` with another `Type`,
    given the variable's label and index
-}
substituteType :: Text -> Int -> Type s -> Type s -> Type s
substituteType a n _A type_ =
    case type_ of
        VariableType{..}
            | a == name && n == 0 -> _A
            | otherwise           -> VariableType{..}

        UnsolvedType{..} ->
            UnsolvedType{..}

        Forall{ type_ = oldType, .. } -> Forall{ type_ = newType, .. }
          where
            newType = substituteType a n' _A oldType

            n'  | a == name && domain == Domain.Type = n + 1
                | otherwise                          = n

        Function{ input = oldInput, output = oldOutput, .. } ->
            Function{ input = newInput, output = newOutput, .. }
          where
            newInput = substituteType a n _A oldInput

            newOutput = substituteType a n _A oldOutput

        Optional{ type_ = oldType, .. } -> Optional{ type_ = newType, .. }
          where
            newType = substituteType a n _A oldType

        List{ type_ = oldType, .. } -> List{ type_ = newType, .. }
          where
            newType = substituteType a n _A oldType

        Record{ fields = Fields kAs ρ, .. } ->
            Record{ fields = Fields (map (second (substituteType a n _A)) kAs) ρ, .. }

        Union{ alternatives = Alternatives kAs ρ, .. } ->
            Union{ alternatives = Alternatives (map (second (substituteType a n _A)) kAs) ρ, .. }

        Scalar{..} ->
            Scalar{..}

{-| Replace all occurrences of a variable within one `Type` with another `Type`,
    given the variable's label and index
-}
substituteFields :: Text -> Int -> Record s -> Type s -> Type s
substituteFields ρ0 n r@(Fields kτs ρ1) type_ =
    case type_ of
        VariableType{..} ->
            VariableType{..}

        UnsolvedType{..} ->
            UnsolvedType{..}

        Forall{ type_ = oldType, .. } -> Forall{ type_ = newType, .. }
          where
            newType = substituteFields ρ0 n' r oldType

            n'  | ρ0 == name && domain == Domain.Fields = n + 1
                | otherwise                             = n

        Function{ input = oldInput, output = oldOutput, .. } ->
            Function{ input = newInput, output = newOutput, .. }
          where
            newInput = substituteFields ρ0 n r oldInput

            newOutput = substituteFields ρ0 n r oldOutput

        Optional{ type_ = oldType, .. } -> Optional{ type_ = newType, .. }
          where
            newType = substituteFields ρ0 n r oldType

        List{ type_ = oldType, .. } -> List{ type_ = newType, .. }
          where
            newType = substituteFields ρ0 n r oldType

        Record{ fields = Fields kAs0 ρ, .. }
            | VariableFields ρ0 == ρ && n == 0 ->
                Record{ fields = Fields (map (second (substituteFields ρ0 n r)) kAs1) ρ1, .. }
            | otherwise ->
                Record{ fields = Fields (map (second (substituteFields ρ0 n r)) kAs0) ρ, .. }
          where
            kAs1 = kAs0 <> map (second (fmap (\_ -> location))) kτs

        Union{ alternatives = Alternatives kAs ρ, .. } ->
            Union{ alternatives = Alternatives (map (second (substituteFields ρ0 n r)) kAs) ρ, .. }

        Scalar{..} ->
            Scalar{..}

{-| Replace all occurrences of a variable within one `Type` with another `Type`,
    given the variable's label and index
-}
substituteAlternatives :: Text -> Int -> Union s -> Type s -> Type s
substituteAlternatives ρ0 n r@(Alternatives kτs ρ1) type_ =
    case type_ of
        VariableType{..} ->
            VariableType{..}

        UnsolvedType{..} ->
            UnsolvedType{..}

        Forall{ type_ = oldType, .. } -> Forall{ type_ = newType, .. }
          where
            newType = substituteAlternatives ρ0 n' r oldType

            n'  | ρ0 == name && domain == Domain.Alternatives = n + 1
                | otherwise                                   = n

        Function{ input = oldInput, output = oldOutput, .. } ->
            Function{ input = newInput, output = newOutput, .. }
          where
            newInput = substituteAlternatives ρ0 n r oldInput

            newOutput = substituteAlternatives ρ0 n r oldOutput

        Optional{ type_ = oldType, .. } -> Optional{ type_ = newType, .. }
         where
            newType = substituteAlternatives ρ0 n r oldType

        List{ type_ = oldType, .. } -> List{ type_ = newType, .. }
         where
            newType = substituteAlternatives ρ0 n r oldType

        Record{ fields = Fields kAs ρ, .. } ->
            Record{ fields = Fields (map (second (substituteAlternatives ρ0 n r)) kAs) ρ, .. }

        Union{ alternatives = Alternatives kAs0 ρ, .. }
            | Monotype.VariableAlternatives ρ0 == ρ && n == 0 ->
                Union{ alternatives = Alternatives (map (second (substituteAlternatives ρ0 n r)) kAs1) ρ1, .. }
            | otherwise ->
                Union{ alternatives = Alternatives (map (second (substituteAlternatives ρ0 n r)) kAs0) ρ, .. }
          where
            kAs1 = kAs0 <> map (second (fmap (\_ -> location))) kτs

        Scalar{..} ->
            Scalar{..}

{-| Count how many times the given `Existential` `Type` variable appears within
    a `Type`
-}
typeFreeIn :: Existential Monotype -> Type s -> Bool
typeFreeIn unsolved =
    Lens.has
        ( Lens.cosmos
        . _As @"UnsolvedType"
        . the @2
        . Lens.only unsolved
        )

{-| Count how many times the given `Existential` t`Monotype.Record` variable
    appears within a `Type`
-}
fieldsFreeIn :: Existential Monotype.Record -> Type s -> Bool
fieldsFreeIn unsolved =
    Lens.has
        ( Lens.cosmos
        . _As @"Record"
        . the @2
        . the @2
        . _As @"UnsolvedFields"
        . Lens.only unsolved
        )

{-| Count how many times the given `Existential` t`Monotype.Union` variable
    appears within a `Type`
-}
alternativesFreeIn :: Existential Monotype.Union -> Type s -> Bool
alternativesFreeIn unsolved =
    Lens.has
        ( Lens.cosmos
        . _As @"Union"
        . the @2
        . the @2
        . _As @"UnsolvedAlternatives"
        . Lens.only unsolved
        )

-- | Default universally quantified variables
defaultTo :: Type s -> Type s -> Type s
defaultTo def Forall{ name, domain = Domain.Type, type_ } =
    substituteType name 0 def type_
defaultTo _ Forall{ name, domain = Domain.Fields, type_ } =
    substituteFields name 0 (Fields [] Monotype.EmptyFields) type_
defaultTo _ Forall{ name, domain = Domain.Alternatives, type_ } =
    substituteAlternatives name 0 (Alternatives [] Monotype.EmptyAlternatives) type_
defaultTo _ type_ = type_

prettyQuantifiedType :: Type s -> Doc AnsiStyle
prettyQuantifiedType type0@Forall{} =
    Pretty.group (Pretty.flatAlt long short)
  where
    short = prettyShort (keyword "forall" <> " ") type0

    long = prettyLong type0

    prettyShort prefix Forall{..} =
            prefix
        <>  punctuation "("
        <>  label (pretty name)
        <>  " "
        <>  punctuation ":"
        <>  " "
        <>  pretty domain
        <>  punctuation ")"
        <>  " "
        <>  prettyShort "" type_

    prettyShort _prefix _A =
        punctuation "." <> " " <> prettyFunctionType _A

    prettyLong Forall{..} =
            keyword "forall"
        <>  " "
        <>  punctuation "("
        <>  label (pretty name)
        <>  " "
        <>  punctuation ":"
        <>  " "
        <>  pretty domain
        <>  punctuation ")"
        <>  " "
        <>  punctuation "."
        <>  Pretty.hardline
        <>  prettyLong type_
    prettyLong _A =
        "  " <> Pretty.nest 2 (prettyFunctionType _A)
prettyQuantifiedType other = prettyFunctionType other

prettyFunctionType :: Type s -> Doc AnsiStyle
prettyFunctionType  type_@Function{} = Pretty.group (Pretty.flatAlt long short)
  where
    long = prettyLong type_

    short = prettyShort type_

    prettyShort Function{..} =
            prettyApplicationType input
        <>  " "
        <>  punctuation "->"
        <>  " "
        <>  prettyShort output
    prettyShort _A =
        prettyApplicationType _A

    prettyLong Function{..} =
            prettyApplicationType input
        <>  " "
        <>  punctuation "->"
        <>  Pretty.hardline
        <>  prettyLong output
    prettyLong _A =
        "  " <> Pretty.nest 2 (prettyApplicationType _A)
prettyFunctionType other =
    prettyApplicationType other

prettyApplicationType :: Type s -> Doc AnsiStyle
prettyApplicationType Optional{..} = Pretty.group (Pretty.flatAlt long short)
  where
    short = builtin "Optional" <> " " <> prettyPrimitiveType type_

    long =  builtin "Optional"
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.nest 2 (prettyPrimitiveType type_)
prettyApplicationType List{..} = Pretty.group (Pretty.flatAlt long short)
  where
    short = builtin "List" <> " " <> prettyPrimitiveType type_

    long =  builtin "List"
        <>  Pretty.hardline
        <>  "  "
        <>  Pretty.nest 2 (prettyPrimitiveType type_)
prettyApplicationType other =
    prettyPrimitiveType other

prettyPrimitiveType :: Type s -> Doc AnsiStyle
prettyPrimitiveType VariableType{..} =
    label (pretty name)
prettyPrimitiveType UnsolvedType{..} =
    label (pretty existential <> "?")
prettyPrimitiveType Record{..} =
    prettyRecordType fields
prettyPrimitiveType Union{..} =
    prettyUnionType alternatives
prettyPrimitiveType Scalar{..} =
    pretty scalar
prettyPrimitiveType other =
    Pretty.group (Pretty.flatAlt long short)
  where
    short = punctuation "(" <> prettyQuantifiedType other <> punctuation ")"

    long =  punctuation "("
        <>  " "
        <>  Pretty.nest 2 (prettyQuantifiedType other)
        <>  Pretty.hardline
        <>  punctuation ")"

prettyRecordType :: Record s -> Doc AnsiStyle
prettyRecordType (Fields [] fields) =
        punctuation "{"
    <>  (case fields of
            EmptyFields      -> " "
            UnsolvedFields ρ -> " " <> label (pretty ρ <> "?") <> " "
            VariableFields ρ -> " " <> label (pretty ρ) <> " "
        )
    <>  punctuation "}"
prettyRecordType (Fields (keyType : keyTypes) fields) =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
            punctuation "{"
        <>  " "
        <>  prettyShortFieldType keyType
        <>  foldMap (\ft -> punctuation "," <> " " <> prettyShortFieldType ft) keyTypes
        <>  (case fields of
                EmptyFields ->
                    mempty
                UnsolvedFields ρ ->
                    punctuation "," <> " " <> label (pretty ρ <> "?")
                VariableFields ρ ->
                    punctuation "," <> " " <> label (pretty ρ)
            )
        <>  " "
        <>  punctuation "}"

    long =  punctuation "{"
        <>  " "
        <>  prettyLongFieldType keyType
        <>  foldMap (\ft -> punctuation "," <> " " <> prettyLongFieldType ft) keyTypes
        <>  case fields of
                EmptyFields ->
                    punctuation "}"
                UnsolvedFields ρ ->
                        punctuation ","
                    <>  " "
                    <>  label (pretty ρ <> "?")
                    <>  Pretty.hardline
                    <>  punctuation "}"
                VariableFields ρ ->
                        punctuation ","
                    <>  " "
                    <>  label (pretty ρ)
                    <>  Pretty.hardline
                    <>  punctuation "}"

    prettyShortFieldType :: (Text, Type s) -> Doc AnsiStyle
    prettyShortFieldType (key, type_) =
            prettyRecordLabel False key
        <>  operator ":"
        <>  " "
        <>  prettyQuantifiedType type_

    prettyLongFieldType :: (Text, Type s) -> Doc AnsiStyle
    prettyLongFieldType (key, type_) =
            prettyRecordLabel False key
        <>  operator ":"
        <>  Pretty.hardline
        <>  "    "
        <>  Pretty.nest 4 (prettyQuantifiedType type_)
        <>  Pretty.hardline

prettyUnionType :: Union s -> Doc AnsiStyle
prettyUnionType (Alternatives [] alternatives) =
        punctuation "<"
    <>  (case alternatives of
            EmptyAlternatives      -> " "
            UnsolvedAlternatives ρ -> " " <> label (pretty ρ <> "?") <> " "
            VariableAlternatives ρ -> " " <> label (pretty ρ) <> " "
        )
    <> punctuation ">"
prettyUnionType (Alternatives (keyType : keyTypes) alternatives) =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
            punctuation "<"
        <>  " "
        <>  prettyShortAlternativeType keyType
        <>  foldMap (\kt -> " " <> punctuation "|" <> " " <> prettyShortAlternativeType kt) keyTypes
        <>  (case alternatives of
                EmptyAlternatives ->
                    mempty
                UnsolvedAlternatives ρ ->
                        " "
                    <>  punctuation "|"
                    <>  " "
                    <>  label (pretty ρ <> "?")
                VariableAlternatives ρ ->
                        " "
                    <>  punctuation "|"
                    <>  " "
                    <>  label (pretty ρ)
            )
        <>  " "
        <>  punctuation ">"

    long  = punctuation "<"
        <>  " "
        <>  prettyLongAlternativeType keyType
        <>  foldMap (\kt -> punctuation "|" <> " " <> prettyLongAlternativeType kt) keyTypes
        <>  case alternatives of
                EmptyAlternatives ->
                    punctuation ">"
                UnsolvedAlternatives ρ ->
                        punctuation "|"
                    <>  " "
                    <>  label (pretty ρ <> "?")
                    <>  Pretty.hardline
                    <>  punctuation ">"
                VariableAlternatives ρ ->
                        punctuation "|"
                    <>  " "
                    <>  label (pretty ρ)
                    <>  Pretty.hardline
                    <>  punctuation ">"

    prettyShortAlternativeType (key, type_) =
            prettyAlternativeLabel key
        <>  operator ":"
        <>  " "
        <>  prettyQuantifiedType type_

    prettyLongAlternativeType (key, type_) =
            prettyAlternativeLabel key
        <>  operator ":"
        <>  Pretty.hardline
        <>  "    "
        <>  Pretty.nest 4 (prettyQuantifiedType type_)
        <>  Pretty.hardline

-- | Pretty-print a @Text@ literal
prettyTextLiteral :: Text -> Doc AnsiStyle
prettyTextLiteral text = "\"" <> prettyTextBody False text <> "\""

-- | Pretty-print the body of a @Text@ literal
prettyTextBody
    :: Bool
    -- ^ Is this a multiline string literal
    -> Text
    -> Doc AnsiStyle
prettyTextBody False text =
    ( pretty
    . Text.replace "\"" "\\\""
    . Text.replace "\b" "\\b"
    . Text.replace "\f" "\\f"
    . Text.replace "\n" "\\n"
    . Text.replace "\r" "\\r"
    . Text.replace "\t" "\\t"
    . Text.replace "\\" "\\\\"
    ) text
prettyTextBody True text =
    mconcat
        (List.intersperse
            Pretty.hardline
            (map
                ( pretty
                . Text.replace "\"" "\\\""
                . Text.replace "\b" "\\b"
                . Text.replace "\f" "\\f"
                . Text.replace "\r" "\\r"
                . Text.replace "\\" "\\\\"
                )
                lines
            )
        )
  where
    lines = Text.splitOn "\n" text

-- | Pretty-print a quoted alternative
prettyQuotedAlternative :: Text -> Doc AnsiStyle
prettyQuotedAlternative text =
        "'"
    <>  ( pretty
        . Text.replace "'" "\\\'"
        . Text.replace "\b" "\\b"
        . Text.replace "\f" "\\f"
        . Text.replace "\n" "\\n"
        . Text.replace "\r" "\\r"
        . Text.replace "\t" "\\t"
        . Text.replace "\\" "\\\\"
        ) text
    <>  "'"

-- | Pretty-print a record label
prettyRecordLabel
    :: Bool
    -- ^ Always quote the label if `True`
    --
    -- This is mainly set to `True` when pretty-printing records so that the
    -- output is valid JSON
    -> Text
    -> Doc AnsiStyle
prettyRecordLabel alwaysQuote field
    | Parser.validRecordLabel field && not alwaysQuote =
        label (pretty field)
    | otherwise =
        label (prettyTextLiteral field)

-- | Pretty-print an alternative label
prettyAlternativeLabel
    :: Text
    -> Doc AnsiStyle
prettyAlternativeLabel alternative
    | Parser.validAlternativeLabel alternative =
        label (pretty alternative)
    | otherwise =
        label (prettyQuotedAlternative alternative)
