{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

{-| This module stores the `Type` type representing polymorphic types and
    utilities for operating on `Type`s
-}
module Grace.Type
    ( -- * Types
      Type(..)
    , Node(..)
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

    -- * Miscellaneous
    , prettyRecordLabel
    , prettyTextLiteral
    ) where

import Control.Lens (Plated(..))
import Data.Generics.Product (the)
import Data.Generics.Sum (_As)
import Data.String (IsString(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Grace.Domain (Domain)
import Grace.Existential (Existential)
import Prettyprinter (Doc, Pretty(..))

import Grace.Monotype
    ( Monotype
    , RemainingAlternatives(..)
    , RemainingFields(..)
    , Scalar(..)
    )

import qualified Control.Lens   as Lens
import qualified Data.Text      as Text
import qualified Grace.Domain   as Domain
import qualified Grace.Lexer    as Lexer
import qualified Grace.Monotype as Monotype
import qualified Prettyprinter  as Pretty

{- $setup

   >>> :set -XOverloadedStrings
   >>> :set -XTypeApplications
-}

-- | A potentially polymorphic type
data Type s = Type { location :: s, node :: Node s }
    deriving stock (Eq, Functor, Generic, Show)

instance IsString (Type ()) where
    fromString string = Type{ location = (), node = fromString string }

instance Pretty (Type s) where
    pretty = prettyType prettyQuantifiedType

instance Plated (Type s) where
    plate onType Type{ node = oldNode, .. } = do
        newNode <- case oldNode of
            VariableType a -> do
                pure (VariableType a)
            UnsolvedType a -> do
                pure (UnsolvedType a)
            Exists s a domain oldA -> do
                newA <- onType oldA
                return (Exists s a domain newA)
            Forall s a domain oldA -> do
                newA <- onType oldA
                return (Forall s a domain newA)
            Function oldA oldB -> do
                newA <- onType oldA
                newB <- onType oldB
                return (Function newA newB)
            Optional oldA -> do
                newA <- onType oldA
                return (Optional newA)
            List oldA -> do
                newA <- onType oldA
                return (List newA)
            Record (Fields oldFieldTypes fields) -> do
                let onPair (field, oldType) = do
                        newType <- onType oldType
                        return (field, newType)
                newFieldTypes <- traverse onPair oldFieldTypes
                return (Record (Fields newFieldTypes fields))
            Union (Alternatives oldAlternativeTypes alternatives) -> do
                let onPair (alternative, oldType) = do
                        newType <- onType oldType
                        return (alternative, newType)
                newAlternativeTypes <- traverse onPair oldAlternativeTypes
                return (Union (Alternatives newAlternativeTypes alternatives))
            Scalar scalar -> do
                pure (Scalar scalar)
        return Type{ node = newNode, .. }

prettyType :: (Node s -> Doc b) -> Type s -> Doc b
prettyType prettyNode Type{ node } = prettyNode node

-- | The constructors for `Type`
data Node s
    = VariableType Text
    -- ^ Type variable
    --
    -- >>> pretty @(Node ()) (VariableType "a")
    -- a
    | UnsolvedType (Existential Monotype)
    -- ^ A placeholder variable whose type has not yet been inferred
    --
    -- >>> pretty @(Node ()) (UnsolvedType 0)
    -- a?
    | Exists s Text Domain (Type s)
    -- ^ Existentially quantified type
    --
    -- >>> pretty @(Node ()) (Exists () "a" Domain.Type "a")
    -- exists (a : Type) . a
    | Forall s Text Domain (Type s)
    -- ^ Universally quantified type
    --
    -- >>> pretty @(Node ()) (Forall () "a" Domain.Type "a")
    -- forall (a : Type) . a
    | Function (Type s) (Type s)
    -- ^ Function type
    --
    -- >>> pretty @(Node ()) (Function "a" "b")
    -- a -> b
    | Optional (Type s)
    -- ^ Optional type
    --
    -- >>> pretty @(Node ()) (Optional "a")
    -- Optional a
    | List (Type s)
    -- ^ List type
    --
    -- >>> pretty @(Node ()) (List "a")
    -- List a
    | Record (Record s)
    -- ^ Record type
    --
    -- >>> pretty @(Node ()) (Record (Fields [("x", "X"), ("y", "Y")] Monotype.EmptyFields))
    -- { x : X, y : Y }
    -- >>> pretty @(Node ()) (Record (Fields [("x", "X"), ("y", "Y")] (Monotype.UnsolvedFields 0)))
    -- { x : X, y : Y, a? }
    | Union (Union s)
    -- ^ Union type
    --
    -- >>> pretty @(Node ()) (Union (Alternatives [("x", "X"), ("y", "Y")] Monotype.EmptyAlternatives))
    -- < x : X | y : Y >
    -- >>> pretty @(Node ()) (Union (Alternatives [("x", "X"), ("y", "Y")] (Monotype.UnsolvedAlternatives 0)))
    -- < x : X | y : Y | a? >
    | Scalar Scalar
    deriving stock (Eq, Functor, Generic, Show)

instance IsString (Node s) where
    fromString string = VariableType (fromString string)

instance Pretty (Node s) where
    pretty = prettyQuantifiedType

-- | A potentially polymorphic record type
data Record s = Fields [(Text, Type s)] RemainingFields
    deriving stock (Eq, Functor, Generic, Show)

instance Pretty (Record s) where
    pretty = prettyRecordType

-- | A potentially polymorphic union type
data Union s = Alternatives [(Text, Type s)] RemainingAlternatives
    deriving stock (Eq, Functor, Generic, Show)

instance Pretty (Union s) where
    pretty = prettyUnionType

{-| This function should not be exported or generally used because it does not
    handle the `location` field correctly.  It is only really safe to use within
    one of the @solve*@ functions
-}
fromMonotype :: Monotype -> Type ()
fromMonotype monotype = Type{ location = (), node }
  where
    node = case monotype of
        Monotype.VariableType α ->
            VariableType α
        Monotype.UnsolvedType α ->
            UnsolvedType α
        Monotype.Function τ σ ->
            Function (fromMonotype τ) (fromMonotype σ)
        Monotype.Optional τ ->
            Optional (fromMonotype τ)
        Monotype.List τ ->
            List (fromMonotype τ)
        Monotype.Record (Monotype.Fields kτs ρ) ->
            Record (Fields (map (\(k, τ) -> (k, fromMonotype τ)) kτs) ρ)
        Monotype.Union (Monotype.Alternatives kτs ρ) ->
            Union (Alternatives (map (\(k, τ) -> (k, fromMonotype τ)) kτs) ρ)
        Monotype.Scalar scalar ->
            Scalar scalar

{-| Substitute a `Type` by replacing all occurrences of the given unsolved
    variable with a `Monotype`
-}
solveType :: Existential Monotype -> Monotype -> Type s -> Type s
solveType unsolved monotype = Lens.transform transformType
  where
    transformType type_ = Lens.over (the @"node") transformNode type_
      where
        transformNode (UnsolvedType unsolved')
            | unsolved == unsolved' =
                node (fmap (\_ -> location type_) (fromMonotype monotype))

        transformNode node =
            node

{-| Substitute a `Type` by replacing all occurrences of the given unsolved
    fields variable with a t`Monotype.Record`
-}
solveFields
    :: Existential Monotype.Record -> Monotype.Record -> Type s -> Type s
solveFields unsolved (Monotype.Fields fieldMonotypes fields) =
    Lens.transform transformType
  where
    transformType type_ = Lens.over (the @"node") transformNode type_
      where
        transformNode (Record (Fields fieldTypes (UnsolvedFields unsolved')))
            | unsolved == unsolved' = Record (Fields fieldTypes' fields)
          where
            fieldTypes' =
                fieldTypes <> map transformPair fieldMonotypes

            transformPair (field, monotype) =
                (field, fmap (\_ -> location type_) (fromMonotype monotype))

        transformNode node =
            node

{-| Substitute a `Type` by replacing all occurrences of the given unsolved
    alternatives variable with a t`Monotype.Union`
-}
solveAlternatives
    :: Existential Monotype.Union -> Monotype.Union -> Type s -> Type s
solveAlternatives unsolved (Monotype.Alternatives alternativeMonotypes alternatives) =
    Lens.transform transformType
  where
    transformType type_ = Lens.over (the @"node") transformNode type_
      where
        transformNode (Union (Alternatives alternativeTypes (UnsolvedAlternatives unsolved')))
            | unsolved == unsolved' =
                Union (Alternatives alternativeTypes' alternatives)
          where
            alternativeTypes' =
                alternativeTypes <> map transformPair alternativeMonotypes

            transformPair (alternative, monotype) =
                (alternative, fmap (\_ -> location type_) (fromMonotype monotype))

        transformNode node =
            node

{-| Replace all occurrences of a variable within one `Type` with another `Type`,
    given the variable's label and index
-}
substituteType :: Text -> Int -> Type s -> Type s -> Type s
substituteType α₀ n _A₀ Type{ node = old, .. } = Type{ node = new, .. }
  where
    new = case old of
        VariableType α₁
            | α₀ == α₁ && n == 0 -> node _A₀
            | otherwise          -> VariableType α₁
        UnsolvedType α ->
            UnsolvedType α
        Exists s α₁ domain _A₁ ->
            if α₀ == α₁ && domain == Domain.Type
            then Exists s α₁ domain (substituteType α₀ (n + 1) _A₀ _A₁)
            else Exists s α₁ domain (substituteType α₀  n      _A₀ _A₁)
        Forall s α₁ domain _A₁ ->
            if α₀ == α₁ && domain == Domain.Type
            then Forall s α₁ domain (substituteType α₀ (n + 1) _A₀ _A₁)
            else Forall s α₁ domain (substituteType α₀  n      _A₀ _A₁)
        Function _A₁ _B ->
            Function (substituteType α₀ n _A₀ _A₁) (substituteType α₀ n _A₀ _B)
        Optional _A₁ ->
            Optional (substituteType α₀ n _A₀ _A₁)
        List _A₁ ->
            List (substituteType α₀ n _A₀ _A₁)
        Record (Fields kAs ρ) ->
            Record (Fields (map (\(k, _A₁) -> (k, substituteType α₀ n _A₀ _A₁)) kAs) ρ)
        Union (Alternatives kAs ρ) ->
            Union (Alternatives (map (\(k, _A₁) -> (k, substituteType α₀ n _A₀ _A₁)) kAs) ρ)
        Scalar scalar ->
            Scalar scalar

{-| Replace all occurrences of a variable within one `Type` with another `Type`,
    given the variable's label and index
-}
substituteFields :: Text -> Int -> Record s -> Type s -> Type s
substituteFields ρ₀ n r@(Fields kτs ρ₁) Type{ node = old, .. } =
    Type{ node = new, .. }
  where
    new = case old of
        VariableType α ->
            VariableType α
        UnsolvedType α ->
            UnsolvedType α
        Exists s α₁ domain _A ->
            if ρ₀ == α₁ && domain == Domain.Fields
            then Exists s α₁ domain (substituteFields ρ₀ (n + 1) r _A)
            else Exists s α₁ domain (substituteFields ρ₀  n      r _A)
        Forall s α₁ domain _A ->
            if ρ₀ == α₁ && domain == Domain.Fields
            then Forall s α₁ domain (substituteFields ρ₀ (n + 1) r _A)
            else Forall s α₁ domain (substituteFields ρ₀  n      r _A)
        Function _A _B ->
            Function (substituteFields ρ₀ n r _A) (substituteFields ρ₀ n r _B)
        Optional _A ->
            Optional (substituteFields ρ₀ n r _A)
        List _A ->
            List (substituteFields ρ₀ n r _A)
        Record (Fields kAs₀ ρ)
            | VariableFields ρ₀ == ρ && n == 0 ->
                Record (Fields (map (\(k, _A) -> (k, substituteFields ρ₀ n r _A)) kAs₁) ρ₁)
            | otherwise ->
                Record (Fields (map (\(k, _A) -> (k, substituteFields ρ₀ n r _A)) kAs₀) ρ)
          where
            kAs₁ = kAs₀ <> map (\(k, τ) -> (k, fmap (\_ -> location) τ)) kτs
        Union (Alternatives kAs ρ) ->
            Union (Alternatives (map (\(k, _A) -> (k, substituteFields ρ₀ n r _A)) kAs) ρ)
        Scalar scalar ->
            Scalar scalar

{-| Replace all occurrences of a variable within one `Type` with another `Type`,
    given the variable's label and index
-}
substituteAlternatives :: Text -> Int -> Union s -> Type s -> Type s
substituteAlternatives ρ₀ n r@(Alternatives kτs ρ₁) Type{ node = old, .. } =
    Type{ node = new, .. }
  where
    new = case old of
        VariableType α ->
            VariableType α
        UnsolvedType α ->
            UnsolvedType α
        Exists s α₁ domain _A ->
            if ρ₀ == α₁ && domain == Domain.Alternatives
            then Exists s α₁ domain (substituteAlternatives ρ₀ (n + 1) r _A)
            else Exists s α₁ domain (substituteAlternatives ρ₀  n      r _A)
        Forall s α₁ domain _A ->
            if ρ₀ == α₁ && domain == Domain.Alternatives
            then Forall s α₁ domain (substituteAlternatives ρ₀ (n + 1) r _A)
            else Forall s α₁ domain (substituteAlternatives ρ₀  n      r _A)
        Function _A _B ->
            Function (substituteAlternatives ρ₀ n r _A) (substituteAlternatives ρ₀ n r _B)
        Optional _A ->
            Optional (substituteAlternatives ρ₀ n r _A)
        List _A ->
            List (substituteAlternatives ρ₀ n r _A)
        Record (Fields kAs ρ) ->
            Record (Fields (map (\(k, _A) -> (k, substituteAlternatives ρ₀ n r _A)) kAs) ρ)
        Union (Alternatives kAs₀ ρ)
            | Monotype.VariableAlternatives ρ₀ == ρ && n == 0 ->
                Union (Alternatives (map (\(k, _A) -> (k, substituteAlternatives ρ₀ n r _A)) kAs₁) ρ₁)
            | otherwise ->
                Union (Alternatives (map (\(k, _A) -> (k, substituteAlternatives ρ₀ n r _A)) kAs₀) ρ)
          where
            kAs₁ = kAs₀ <> map (\(k, τ) -> (k, fmap (\_ -> location) τ)) kτs
        Scalar scalar ->
            Scalar scalar

{-| Count how many times the given `Existential` `Type` variable appears within
    a `Type`
-}
typeFreeIn :: Existential Monotype -> Type s -> Bool
typeFreeIn unsolved =
    Lens.has
        ( Lens.cosmos
        . the @"node"
        . _As @"UnsolvedType"
        . Lens.only unsolved
        )

{-| Count how many times the given `Existential` t`Monotype.Record` variable
    appears within a `Type`
-}
fieldsFreeIn :: Existential Monotype.Record -> Type s -> Bool
fieldsFreeIn unsolved =
    Lens.has
        ( Lens.cosmos
        . the @"node"
        . _As @"Record"
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
        . the @"node"
        . _As @"Union"
        . the @2
        . _As @"UnsolvedAlternatives"
        . Lens.only unsolved
        )

prettyQuantifiedType :: Node s -> Doc a
prettyQuantifiedType type_
    | isQuantified type_ = Pretty.group (Pretty.flatAlt long short)
    | otherwise          = prettyFunctionType type_
  where
    isQuantified Forall{} = True
    isQuantified Exists{} = True
    isQuantified _        = False

    short = prettyShort type_

    long = Pretty.align (prettyLong type_)

    prettyShort (Forall _ a domain _A) =
            "forall ("
        <>  pretty a
        <>  " : "
        <>  pretty domain
        <>  ") . "
        <>  prettyType prettyShort _A
    prettyShort (Exists _ a domain _A) =
            "exists ("
        <>  pretty a
        <>  " : "
        <>  pretty domain
        <>  ") . "
        <>  prettyType prettyShort _A
    prettyShort _A =
        prettyFunctionType _A

    prettyLong (Forall _ a domain _A) =
            "forall ("
        <>  pretty a
        <>  " : "
        <>  pretty domain
        <>  ") ."
        <>  Pretty.hardline
        <>  prettyType prettyLong _A
    prettyLong (Exists _ a domain _A) =
            "exists ("
        <>  pretty a
        <>  " : "
        <>  pretty domain
        <>  ") ."
        <>  Pretty.hardline
        <>  prettyType prettyLong _A
    prettyLong _A =
        "  " <> prettyFunctionType _A

prettyFunctionType :: Node s -> Doc a
prettyFunctionType  type_@Function{} = Pretty.group (Pretty.flatAlt long short)
  where
    long = Pretty.align (prettyLong type_)

    short = prettyShort type_

    prettyShort (Function _A _B) =
            prettyType prettyApplicationType _A
        <>  " -> "
        <>  prettyType prettyShort _B
    prettyShort _A =
        prettyApplicationType _A

    prettyLong (Function _A _B) =
            prettyType prettyApplicationType _A
        <>  " ->"
        <>  Pretty.hardline
        <>  prettyType prettyLong _B
    prettyLong _A =
        "  " <> prettyApplicationType _A
prettyFunctionType other =
    prettyApplicationType other

prettyApplicationType :: Node s -> Doc a
prettyApplicationType (Optional _A) = Pretty.group (Pretty.flatAlt long short)
  where
    short = "Optional " <> prettyType prettyPrimitiveType _A

    long =
        Pretty.align
            (   "Optional"
            <>  Pretty.hardline
            <>  "  "
            <>  prettyType prettyPrimitiveType _A
            )
prettyApplicationType (List _A) = Pretty.group (Pretty.flatAlt long short)
  where
    short = "List " <> prettyType prettyPrimitiveType _A

    long =
        Pretty.align
            (   "List"
            <>  Pretty.hardline
            <>  "  "
            <>  prettyType prettyPrimitiveType _A
            )
prettyApplicationType other =
    prettyPrimitiveType other

prettyPrimitiveType :: Node s -> Doc a
prettyPrimitiveType (VariableType α) = pretty α
prettyPrimitiveType (UnsolvedType α) = pretty α <> "?"
prettyPrimitiveType (Record r)       = pretty r
prettyPrimitiveType (Union u)        = pretty u
prettyPrimitiveType (Scalar scalar)  = pretty scalar
prettyPrimitiveType  other           = Pretty.group (Pretty.flatAlt long short)
  where
    short = "(" <> prettyQuantifiedType other <> ")"

    long =
        Pretty.align
            (   "( "
            <>  prettyQuantifiedType other
            <>  Pretty.hardline
            <>  ")"
            )

prettyRecordType :: Record s -> Doc a
prettyRecordType (Fields [] EmptyFields) =
    "{ }"
prettyRecordType (Fields [] (UnsolvedFields ρ)) =
    "{ " <> pretty ρ <> "? }"
prettyRecordType (Fields [] (VariableFields ρ)) =
    "{ " <> pretty ρ <> " }"
prettyRecordType (Fields ((key₀, type₀) : keyTypes) fields) =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
            "{ "
        <>  prettyShortFieldType (key₀, type₀)
        <>  foldMap (\ft -> ", " <> prettyShortFieldType ft) keyTypes
        <>  case fields of
                EmptyFields      -> " }"
                UnsolvedFields ρ -> ", " <> pretty ρ <> "? }"
                VariableFields ρ -> ", " <> pretty ρ <> " }"

    long =
        Pretty.align
            (   "{ "
            <>  prettyLongFieldType (key₀, type₀)
            <>  foldMap (\ft -> ", " <> prettyLongFieldType ft) keyTypes
            <>  case fields of
                    EmptyFields ->
                        "}"
                    UnsolvedFields ρ ->
                        ", " <> pretty ρ <> "?" <> Pretty.hardline <> "}"
                    VariableFields ρ ->
                        ", " <> pretty ρ <> Pretty.hardline <> "}"
            )

    prettyShortFieldType :: (Text, Type s) -> Doc a
    prettyShortFieldType (key, type_) =
            prettyRecordLabel False key
        <>  ": "
        <>  prettyType prettyQuantifiedType type_

    prettyLongFieldType :: (Text, Type s) -> Doc a
    prettyLongFieldType (key, type_) =
            prettyRecordLabel False key
        <>  ":"
        <>  Pretty.group (Pretty.flatAlt (Pretty.hardline <> "    ") " ")
        <>  prettyType prettyQuantifiedType type_
        <>  Pretty.hardline

prettyUnionType :: Union s -> Doc a
prettyUnionType (Alternatives [] EmptyAlternatives) =
    "< >"
prettyUnionType (Alternatives [] (UnsolvedAlternatives ρ)) =
    "< " <> pretty ρ <> "? >"
prettyUnionType (Alternatives [] (VariableAlternatives ρ)) =
    "< " <> pretty ρ <> " >"
prettyUnionType (Alternatives ((key₀, type₀) : keyTypes) alternatives) =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
            "< "
        <>  prettyShortAlternativeType (key₀, type₀)
        <>  foldMap (\kt -> " | " <> prettyShortAlternativeType kt) keyTypes
        <>  case alternatives of
                EmptyAlternatives      -> " >"
                UnsolvedAlternatives ρ -> " | " <> pretty ρ <> "? >"
                VariableAlternatives ρ -> " | " <> pretty ρ <> " >"

    long  =
        Pretty.align
            (   "< "
            <>  prettyLongAlternativeType (key₀, type₀)
            <>  foldMap (\kt -> "| " <> prettyLongAlternativeType kt) keyTypes
            <>  case alternatives of
                    EmptyAlternatives ->
                        ">"
                    UnsolvedAlternatives ρ ->
                        "| " <> pretty ρ <> "?" <> Pretty.hardline <> ">"
                    VariableAlternatives ρ ->
                        "| " <> pretty ρ <> Pretty.hardline <> ">"
            )

    prettyShortAlternativeType (key, type_) =
            pretty key
        <>  ": "
        <>  prettyType prettyQuantifiedType type_

    prettyLongAlternativeType (key, type_) =
            pretty key
        <>  ":"
        <>  Pretty.group (Pretty.flatAlt (Pretty.hardline <> "    ") " ")
        <>  prettyType prettyQuantifiedType type_
        <>  Pretty.hardline

-- | Pretty-print a @Text@ literal
prettyTextLiteral :: Text -> Doc b
prettyTextLiteral text =
        "\""
    <>  ( pretty
        . Text.replace "\"" "\\\""
        . Text.replace "\b" "\\b"
        . Text.replace "\f" "\\f"
        . Text.replace "\n" "\\n"
        . Text.replace "\r" "\\r"
        . Text.replace "\t" "\\t"
        . Text.replace "\\" "\\\\"
        ) text
    <>  "\""

-- | Pretty-print a record label
prettyRecordLabel
    :: Bool
    -- ^ Always quote the label if `True`
    --
    -- This is mainly set to `True` when pretty-printing records so that the
    -- output is valid JSON
    -> Text
    -> Doc b
prettyRecordLabel alwaysQuote label
    | Lexer.validRecordLabel label && not alwaysQuote = pretty label
    | otherwise                                       = prettyTextLiteral label
