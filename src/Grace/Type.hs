{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

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

import Data.String (IsString(..))
import Data.Text (Text)
import Prettyprinter (Doc, Pretty(..))
import Grace.Domain (Domain)
import Grace.Existential (Existential)

import Grace.Monotype
    ( Monotype
    , RemainingAlternatives(..)
    , RemainingFields(..)
    , Scalar(..)
    )

import qualified Data.Text      as Text
import qualified Grace.Lexer    as Lexer
import qualified Grace.Domain   as Domain
import qualified Grace.Monotype as Monotype

{- $setup

   >>> :set -XOverloadedStrings
   >>> :set -XTypeApplications
-}

-- | A potentially polymorphic type
data Type s = Type { location :: s, node :: Node s }
    deriving stock (Eq, Functor, Show)

instance IsString (Type ()) where
    fromString string = Type{ location = (), node = fromString string }

instance Pretty (Type s) where
    pretty = prettyType prettyQuantifiedType

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
    deriving stock (Eq, Functor, Show)

instance IsString (Node s) where
    fromString string = VariableType (fromString string)

instance Pretty (Node s) where
    pretty = prettyQuantifiedType

-- | A potentially polymorphic record type
data Record s = Fields [(Text, Type s)] RemainingFields
    deriving stock (Eq, Functor, Show)

instance Pretty (Record s) where
    pretty = prettyRecordType

-- | A potentially polymorphic union type
data Union s = Alternatives [(Text, Type s)] RemainingAlternatives
    deriving stock (Eq, Functor, Show)

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
solveType α₀ τ Type{ node = old, .. } = Type{ node = new, .. }
  where
    new = case old of
        VariableType α ->
            VariableType α
        UnsolvedType α₁
            | α₀ == α₁ -> node (fmap (\_ -> location) (fromMonotype τ))
            | otherwise -> UnsolvedType α₁
        Forall s α₁ domain _A ->
            Forall s α₁ domain (solveType α₀ τ _A)
        Function _A _B ->
            Function (solveType α₀ τ _A) (solveType α₀ τ _B)
        List _A ->
            List (solveType α₀ τ _A)
        Record (Fields kAs ρ) ->
            Record (Fields (map (\(k, _A) -> (k, solveType α₀ τ _A)) kAs) ρ)
        Union (Alternatives kAs ρ) ->
            Union (Alternatives (map (\(k, _A) -> (k, solveType α₀ τ _A)) kAs) ρ)
        Scalar scalar ->
            Scalar scalar

{-| Substitute a `Type` by replacing all occurrences of the given unsolved
    fields variable with a t`Monotype.Record`
-}
solveFields
    :: Existential Monotype.Record -> Monotype.Record -> Type s -> Type s
solveFields ρ₀ r@(Monotype.Fields kτs ρ₁) Type{ node = old, .. } =
    Type{ node = new, .. }
  where
    new = case old of
        VariableType α ->
            VariableType α
        UnsolvedType α ->
            UnsolvedType α
        Forall s α domain _A ->
            Forall s α domain (solveFields ρ₀ r _A)
        Function _A _B ->
            Function (solveFields ρ₀ r _A) (solveFields ρ₀ r _B)
        List _A ->
            List (solveFields ρ₀ r _A)
        Record (Fields kAs₀ ρ)
            | UnsolvedFields ρ₀ == ρ ->
                Record (Fields (map (\(k, _A) -> (k, solveFields ρ₀ r _A)) kAs₁) ρ₁)
            | otherwise ->
                Record (Fields (map (\(k, _A) -> (k, solveFields ρ₀ r _A)) kAs₀) ρ)
          where
            kAs₁ = kAs₀ <> map (\(k, τ) -> (k, fmap (\_ -> location) (fromMonotype τ))) kτs
        Union (Alternatives kAs ρ) ->
            Union (Alternatives (map adapt kAs) ρ)
          where
            adapt (k, _A) = (k, solveFields ρ₀ r _A)
        Scalar scalar ->
            Scalar scalar

{-| Substitute a `Type` by replacing all occurrences of the given unsolved
    alternatives variable with a t`Monotype.Union`
-}
solveAlternatives
    :: Existential Monotype.Union -> Monotype.Union -> Type s -> Type s
solveAlternatives ρ₀ r@(Monotype.Alternatives kτs ρ₁) Type{ node = old, .. } =
    Type{ node = new, .. }
  where
    new = case old of
        VariableType α ->
            VariableType α
        UnsolvedType α ->
            UnsolvedType α
        Forall s α domain _A ->
            Forall s α domain (solveAlternatives ρ₀ r _A)
        Function _A _B ->
            Function (solveAlternatives ρ₀ r _A) (solveAlternatives ρ₀ r _B)
        List _A ->
            List (solveAlternatives ρ₀ r _A)
        Record (Fields kAs ρ) ->
            Record (Fields (map adapt kAs) ρ)
          where
            adapt (k, _A) = (k, solveAlternatives ρ₀ r _A)
        Union (Alternatives kAs₀ ρ)
            | UnsolvedAlternatives ρ₀ == ρ ->
                Union (Alternatives (map (\(k, _A) -> (k, solveAlternatives ρ₀ r _A)) kAs₁) ρ₁)
            | otherwise ->
                Union (Alternatives (map (\(k, _A) -> (k, solveAlternatives ρ₀ r _A)) kAs₀) ρ)
          where
            kAs₁ = kAs₀ <> map (\(k, τ) -> (k, fmap (\_ -> location) (fromMonotype τ))) kτs
        Scalar scalar ->
            Scalar scalar

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
        Forall s α₁ domain _A₁ ->
            if α₀ == α₁ && domain == Domain.Type
            then Forall s α₁ domain (substituteType α₀ (n + 1) _A₀ _A₁)
            else Forall s α₁ domain (substituteType α₀  n      _A₀ _A₁)
        Function _A₁ _B ->
            Function (substituteType α₀ n _A₀ _A₁) (substituteType α₀ n _A₀ _B)
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
        Forall s α₁ domain _A ->
            if ρ₀ == α₁ && domain == Domain.Fields
            then Forall s α₁ domain (substituteFields ρ₀ (n + 1) r _A)
            else Forall s α₁ domain (substituteFields ρ₀  n      r _A)
        Function _A _B ->
            Function (substituteFields ρ₀ n r _A) (substituteFields ρ₀ n r _B)
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
        Forall s α₁ domain _A ->
            if ρ₀ == α₁ && domain == Domain.Alternatives
            then Forall s α₁ domain (substituteAlternatives ρ₀ (n + 1) r _A)
            else Forall s α₁ domain (substituteAlternatives ρ₀  n      r _A)
        Function _A _B ->
            Function (substituteAlternatives ρ₀ n r _A) (substituteAlternatives ρ₀ n r _B)
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
typeFreeIn :: Existential Monotype -> Type s-> Bool
α₀ `typeFreeIn` Type{ node } =
    case node of
        VariableType _ ->
            False
        UnsolvedType α₁ ->
            α₀ == α₁
        Forall _ _ _ _A ->
            α₀ `typeFreeIn` _A
        Function _A _B ->
            α₀ `typeFreeIn` _A || α₀ `typeFreeIn` _B
        List _A ->
            α₀ `typeFreeIn` _A
        Scalar _->
            False
        Record (Fields kAs _) ->
            any (\(_, _A) -> α₀ `typeFreeIn` _A) kAs
        Union (Alternatives kAs _) ->
            any (\(_, _A) -> α₀ `typeFreeIn` _A) kAs

{-| Count how many times the given `Existential` t`Monotype.Record` variable
    appears within a `Type`
-}
fieldsFreeIn :: Existential Monotype.Record -> Type s -> Bool
ρ₀ `fieldsFreeIn` Type{ node } =
    case node of
        VariableType _ ->
            False
        UnsolvedType _ ->
            False
        Forall _ _ _ _A ->
            ρ₀ `fieldsFreeIn` _A
        Function _A _B ->
            ρ₀ `fieldsFreeIn` _A || ρ₀ `fieldsFreeIn` _B
        List _A ->
            ρ₀ `fieldsFreeIn` _A
        Scalar _ ->
            False
        Record (Fields kAs ρ₁) ->
                UnsolvedFields ρ₀ == ρ₁
            ||  any (\(_, _A) -> ρ₀ `fieldsFreeIn` _A) kAs
        Union (Alternatives kAs _) ->
            any (\(_, _A) -> ρ₀ `fieldsFreeIn` _A) kAs

{-| Count how many times the given `Existential` t`Monotype.Union` variable
    appears within a `Type`
-}
alternativesFreeIn :: Existential Monotype.Union -> Type s -> Bool
ρ₀ `alternativesFreeIn` Type{ node } =
    case node of
        VariableType _ ->
            False
        UnsolvedType _ ->
            False
        Forall _ _ _ _A ->
            ρ₀ `alternativesFreeIn` _A
        Function _A _B ->
            ρ₀ `alternativesFreeIn` _A || ρ₀ `alternativesFreeIn` _B
        List _A ->
            ρ₀ `alternativesFreeIn` _A
        Scalar _ ->
            False
        Record (Fields kAs _) ->
            any (\(_, _A) -> ρ₀ `alternativesFreeIn` _A) kAs
        Union (Alternatives kAs ρ₁) ->
                UnsolvedAlternatives ρ₀ == ρ₁
            ||  any (\(_, _A) -> ρ₀ `alternativesFreeIn` _A) kAs

prettyQuantifiedType :: Node s -> Doc a
prettyQuantifiedType (Forall _ α Domain.Type _A) =
    "forall (" <> pretty α <> " : Type) . " <> prettyType prettyQuantifiedType _A
prettyQuantifiedType (Forall _ α Domain.Fields _A) =
    "forall (" <> pretty α <> " : Fields) . " <> prettyType prettyQuantifiedType _A
prettyQuantifiedType (Forall _ α Domain.Alternatives _A) =
    "forall (" <> pretty α <> " : Alternatives) . " <> prettyType prettyQuantifiedType _A
prettyQuantifiedType other = prettyFunctionType other

prettyFunctionType :: Node s -> Doc a
prettyFunctionType (Function _A _B) =
        prettyType prettyApplicationType _A
    <>  " -> "
    <>  prettyType prettyFunctionType _B
prettyFunctionType other =
    prettyApplicationType other

prettyApplicationType :: Node s -> Doc a
prettyApplicationType (List _A) = "List " <> prettyType prettyPrimitiveType _A
prettyApplicationType  other    = prettyPrimitiveType other

prettyPrimitiveType :: Node s -> Doc a
prettyPrimitiveType (VariableType α) = pretty α
prettyPrimitiveType (UnsolvedType α) = pretty α <> "?"
prettyPrimitiveType (Record r)       = pretty r
prettyPrimitiveType (Union u)        = pretty u
prettyPrimitiveType (Scalar scalar)  = pretty scalar
prettyPrimitiveType  other           = "(" <> prettyQuantifiedType other <> ")"

prettyRecordType :: Record s -> Doc a
prettyRecordType (Fields [] EmptyFields) =
    "{ }"
prettyRecordType (Fields [] (UnsolvedFields ρ)) =
    "{ " <> pretty ρ <> "? }"
prettyRecordType (Fields [] (VariableFields ρ)) =
    "{ " <> pretty ρ <> " }"
prettyRecordType (Fields ((key₀, type₀) : keyTypes) fields) =
        "{ "
    <>  prettyRecordLabel key₀
    <>  " : "
    <>  prettyType prettyQuantifiedType type₀
    <>  foldMap prettyFieldType keyTypes
    <>  case fields of
            EmptyFields      -> " }"
            UnsolvedFields ρ -> ", " <> pretty ρ <> "? }"
            VariableFields ρ -> ", " <> pretty ρ <> " }"

prettyUnionType :: Union s -> Doc a
prettyUnionType (Alternatives [] EmptyAlternatives) =
    "< >"
prettyUnionType (Alternatives [] (UnsolvedAlternatives ρ)) =
    "< " <> pretty ρ <> "? >"
prettyUnionType (Alternatives [] (VariableAlternatives ρ)) =
    "< " <> pretty ρ <> " >"
prettyUnionType (Alternatives ((key₀, type₀) : keyTypes) alternatives) =
        "< "
    <>  pretty key₀
    <>  " : "
    <>  prettyType prettyQuantifiedType type₀
    <>  foldMap prettyAlternativeType keyTypes
    <>  case alternatives of
            EmptyAlternatives      -> " >"
            UnsolvedAlternatives ρ -> " | " <> pretty ρ <> "? >"
            VariableAlternatives ρ -> " | " <> pretty ρ <> " >"

prettyFieldType :: (Text, Type s) -> Doc a
prettyFieldType (key, type_) =
        ", "
    <>  prettyRecordLabel key
    <>  " : "
    <> prettyType prettyQuantifiedType type_

prettyAlternativeType :: (Text, Type s) -> Doc a
prettyAlternativeType (key, type_) =
    " | " <> pretty key <> " : " <> prettyType prettyQuantifiedType type_

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

-- | Pretty-print a record label (with quotes if necessary)
prettyRecordLabel :: Text -> Doc b
prettyRecordLabel label
    | Lexer.validRecordLabel label = pretty label
    | otherwise                    = prettyTextLiteral label
