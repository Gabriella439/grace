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
    , Node(..)
    , Record(..)
    , Union(..)
    , Hole(..)
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
      -- * Pretty-printing
    , prettyRecordLabel
    , prettyTextLiteral
    ) where

import Control.Lens (Plated(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Generics.Product (the)
import Data.Generics.Sum (_As)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Grace.Domain (Domain)
import Grace.Existential (Existential)
import Grace.Pretty (Pretty(..), builtin, keyword, label, operator, punctuation)
import Language.Haskell.TH.Syntax (Lift)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)

import Grace.Monotype
    ( Monotype
    , RemainingAlternatives(..)
    , RemainingFields(..)
    , Scalar(..)
    )

import qualified Control.Lens as Lens
import qualified Data.Text as Text
import qualified Grace.Domain as Domain
import qualified Grace.Lexer as Lexer
import qualified Grace.Monotype as Monotype
import qualified Prettyprinter as Pretty

{- $setup

   >>> :set -XOverloadedStrings
   >>> :set -XTypeApplications
-}

-- | A potentially polymorphic type
data Type s h = Type { location :: s, node :: Node s h}
    deriving stock (Eq, Functor, Generic, Lift, Show)

instance Bifunctor Type where
    first f Type{..} = Type{ location = f location, node = first f node, .. }

    second = fmap

instance IsString (Type () a) where
    fromString string = Type{ location = (), node = fromString string }

instance Pretty h => Pretty (Type s h) where
    pretty = liftType prettyQuantifiedType

instance Plated (Type s h) where
    plate onType Type{ node = oldNode, .. } = do
        newNode <- case oldNode of
            VariableType a -> do
                pure (VariableType a)
            UnsolvedType a -> do
                pure (UnsolvedType a)
            TypeHole a -> do
                pure (TypeHole a)
            Exists s h domain oldA -> do
                newA <- onType oldA
                return (Exists s h domain newA)
            Forall s h domain oldA -> do
                newA <- onType oldA
                return (Forall s h domain newA)
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

liftType :: (Node s h -> Doc b) -> Type s h -> Doc b
liftType pretty_ Type{ node } = pretty_ node

-- | The constructors for `Type`
data Node s h
    = VariableType Text
    -- ^ Type variable
    --
    -- >>> pretty @(Node () Hole) (VariableType "a")
    -- a
    | UnsolvedType (Existential Monotype)
    -- ^ A placeholder variable whose type has not yet been inferred
    --
    -- >>> pretty @(Node () Hole) (UnsolvedType 0)
    -- a?
    | TypeHole h
    -- ^ A type hole inserted by the user
    --
    -- >>> pretty @(Node () Hole) (TypeHole Hole)
    -- ?
    | Exists s Text Domain (Type s h)
    -- ^ Existentially quantified type
    --
    -- >>> pretty @(Node () Hole) (Exists () "a" Domain.Type "a")
    -- exists (a : Type) . a
    | Forall s Text Domain (Type s h)
    -- ^ Universally quantified type
    --
    -- >>> pretty @(Node () Hole) (Forall () "a" Domain.Type "a")
    -- forall (a : Type) . a
    | Function (Type s h) (Type s h)
    -- ^ Function type
    --
    -- >>> pretty @(Node () Hole) (Function "a" "b")
    -- a -> b
    | Optional (Type s h)
    -- ^ Optional type
    --
    -- >>> pretty @(Node () Hole) (Optional "a")
    -- Optional a
    | List (Type s h)
    -- ^ List type
    --
    -- >>> pretty @(Node () Hole) (List "a")
    -- List a
    | Record (Record s h)
    -- ^ Record type
    --
    -- >>> pretty @(Node () Hole) (Record (Fields [("x", "X"), ("y", "Y")] Monotype.EmptyFields))
    -- { x: X, y: Y }
    -- >>> pretty @(Node () Hole) (Record (Fields [("x", "X"), ("y", "Y")] (Monotype.UnsolvedFields 0)))
    -- { x: X, y: Y, a? }
    | Union (Union s h)
    -- ^ Union type
    --
    -- >>> pretty @(Node () Hole) (Union (Alternatives [("x", "X"), ("y", "Y")] Monotype.EmptyAlternatives))
    -- < x: X | y: Y >
    -- >>> pretty @(Node () Hole) (Union (Alternatives [("x", "X"), ("y", "Y")] (Monotype.UnsolvedAlternatives 0)))
    -- < x: X | y: Y | a? >
    | Scalar Scalar
    deriving stock (Eq, Functor, Generic, Lift, Show)

instance Bifunctor Node where
    first _ (VariableType a) = VariableType a
    first _ (UnsolvedType a) = UnsolvedType a
    first _ (TypeHole a    ) = TypeHole a
    first f (Exists a b c d) = Exists (f a) b c (first f d)
    first f (Forall a b c d) = Forall (f a) b c (first f d)
    first f (Function a b  ) = Function (first f a) (first f b)
    first f (Optional a    ) = Optional (first f a)
    first f (List a        ) = List (first f a)
    first f (Record a      ) = Record (first f a)
    first f (Union a      ) = Union (first f a)
    first _ (Scalar a     ) = Scalar a

    second = fmap

instance IsString (Node s h) where
    fromString string = VariableType (fromString string)

instance Pretty h => Pretty (Node s h) where
    pretty = prettyQuantifiedType

-- | A potentially polymorphic record type
data Record s h = Fields [(Text, Type s h)] RemainingFields
    deriving stock (Eq, Functor, Generic, Lift, Show)

instance Bifunctor Record where
    first f (Fields as b) = Fields (map (second (first f)) as) b

    second = fmap

instance Pretty h => Pretty (Record s h) where
    pretty = prettyRecordType

-- | A potentially polymorphic union type
data Union s h = Alternatives [(Text, Type s h)] RemainingAlternatives
    deriving stock (Eq, Functor, Generic, Lift, Show)

instance Bifunctor Union where
    first f (Alternatives as b) = Alternatives (map (second (first f)) as) b

    second = fmap

instance Pretty h => Pretty (Union s h) where
    pretty = prettyUnionType

data Hole = Hole
    deriving (Eq, Lift, Show)

instance Pretty Hole where
    pretty Hole = keyword "?"

{-| This function should not be exported or generally used because it does not
    handle the `location` field correctly.  It is only really safe to use within
    one of the @solve*@ functions
-}
fromMonotype :: Monotype -> Type () a
fromMonotype monotype = Type{ location = (), node }
  where
    node = case monotype of
        Monotype.VariableType a ->
            VariableType a
        Monotype.UnsolvedType a ->
            UnsolvedType a
        Monotype.Function τ σ ->
            Function (fromMonotype τ) (fromMonotype σ)
        Monotype.Optional τ ->
            Optional (fromMonotype τ)
        Monotype.List τ ->
            List (fromMonotype τ)
        Monotype.Record (Monotype.Fields kτs ρ) ->
            Record (Fields (map (second fromMonotype) kτs) ρ)
        Monotype.Union (Monotype.Alternatives kτs ρ) ->
            Union (Alternatives (map (second fromMonotype) kτs) ρ)
        Monotype.Scalar scalar ->
            Scalar scalar

instance Pretty Monotype where
    pretty = pretty . fromMonotype @Void

{-| Substitute a `Type` by replacing all occurrences of the given unsolved
    variable with a `Monotype`
-}
solveType :: Existential Monotype -> Monotype -> Type s h -> Type s h
solveType unsolved monotype = Lens.transform transformType
  where
    transformType type_ = Lens.over (the @"node") transformNode type_
      where
        transformNode (UnsolvedType unsolved')
            | unsolved == unsolved' =
                node (first (\_ -> location type_) (fromMonotype monotype))

        transformNode node =
            node

{-| Substitute a `Type` by replacing all occurrences of the given unsolved
    fields variable with a t`Monotype.Record`
-}
solveFields
    :: Existential Monotype.Record -> Monotype.Record -> Type s h -> Type s h
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
                (field, first (\_ -> location type_) (fromMonotype monotype))

        transformNode node =
            node

{-| Substitute a `Type` by replacing all occurrences of the given unsolved
    alternatives variable with a t`Monotype.Union`
-}
solveAlternatives
    :: Existential Monotype.Union -> Monotype.Union -> Type s h -> Type s h
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
                (alternative, first (\_ -> location type_) (fromMonotype monotype))

        transformNode node =
            node

{-| Replace all occurrences of a variable within one `Type` with another `Type`,
    given the variable's label and index
-}
substituteType :: Text -> Int -> Type s h -> Type s h -> Type s h
substituteType a0 n _A0 Type{ node = old, .. } = Type{ node = new, .. }
  where
    new = case old of
        VariableType a1
            | a0 == a1 && n == 0 -> node _A0
            | otherwise          -> VariableType a1
        UnsolvedType a ->
            UnsolvedType a
        TypeHole h ->
            TypeHole h
        Exists s a1 domain _A1 ->
            if a0 == a1 && domain == Domain.Type
            then Exists s a1 domain (substituteType a0 (n + 1) _A0 _A1)
            else Exists s a1 domain (substituteType a0  n      _A0 _A1)
        Forall s a1 domain _A1 ->
            if a0 == a1 && domain == Domain.Type
            then Forall s a1 domain (substituteType a0 (n + 1) _A0 _A1)
            else Forall s a1 domain (substituteType a0  n      _A0 _A1)
        Function _A1 _B ->
            Function (substituteType a0 n _A0 _A1) (substituteType a0 n _A0 _B)
        Optional _A1 ->
            Optional (substituteType a0 n _A0 _A1)
        List _A1 ->
            List (substituteType a0 n _A0 _A1)
        Record (Fields kAs ρ) ->
            Record (Fields (map (second (substituteType a0 n _A0)) kAs) ρ)
        Union (Alternatives kAs ρ) ->
            Union (Alternatives (map (second (substituteType a0 n _A0)) kAs) ρ)
        Scalar scalar ->
            Scalar scalar

{-| Replace all occurrences of a variable within one `Type` with another `Type`,
    given the variable's label and index
-}
substituteFields :: Text -> Int -> Record s h -> Type s h -> Type s h
substituteFields ρ0 n r@(Fields kτs ρ1) Type{ node = old, .. } =
    Type{ node = new, .. }
  where
    new = case old of
        VariableType a ->
            VariableType a
        UnsolvedType a ->
            UnsolvedType a
        TypeHole h ->
            TypeHole h
        Exists s a1 domain _A ->
            if ρ0 == a1 && domain == Domain.Fields
            then Exists s a1 domain (substituteFields ρ0 (n + 1) r _A)
            else Exists s a1 domain (substituteFields ρ0  n      r _A)
        Forall s a1 domain _A ->
            if ρ0 == a1 && domain == Domain.Fields
            then Forall s a1 domain (substituteFields ρ0 (n + 1) r _A)
            else Forall s a1 domain (substituteFields ρ0  n      r _A)
        Function _A _B ->
            Function (substituteFields ρ0 n r _A) (substituteFields ρ0 n r _B)
        Optional _A ->
            Optional (substituteFields ρ0 n r _A)
        List _A ->
            List (substituteFields ρ0 n r _A)
        Record (Fields kAs0 ρ)
            | VariableFields ρ0 == ρ && n == 0 ->
                Record (Fields (map (second (substituteFields ρ0 n r)) kAs1) ρ1)
            | otherwise ->
                Record (Fields (map (second (substituteFields ρ0 n r)) kAs0) ρ)
          where
            kAs1 = kAs0 <> map (second (first (\_ -> location))) kτs
        Union (Alternatives kAs ρ) ->
            Union (Alternatives (map (second (substituteFields ρ0 n r)) kAs) ρ)
        Scalar scalar ->
            Scalar scalar

{-| Replace all occurrences of a variable within one `Type` with another `Type`,
    given the variable's label and index
-}
substituteAlternatives :: Text -> Int -> Union s h -> Type s h -> Type s h
substituteAlternatives ρ0 n r@(Alternatives kτs ρ1) Type{ node = old, .. } =
    Type{ node = new, .. }
  where
    new = case old of
        VariableType a ->
            VariableType a
        UnsolvedType a ->
            UnsolvedType a
        TypeHole h ->
            TypeHole h
        Exists s a1 domain _A ->
            if ρ0 == a1 && domain == Domain.Alternatives
            then Exists s a1 domain (substituteAlternatives ρ0 (n + 1) r _A)
            else Exists s a1 domain (substituteAlternatives ρ0  n      r _A)
        Forall s a1 domain _A ->
            if ρ0 == a1 && domain == Domain.Alternatives
            then Forall s a1 domain (substituteAlternatives ρ0 (n + 1) r _A)
            else Forall s a1 domain (substituteAlternatives ρ0  n      r _A)
        Function _A _B ->
            Function (substituteAlternatives ρ0 n r _A) (substituteAlternatives ρ0 n r _B)
        Optional _A ->
            Optional (substituteAlternatives ρ0 n r _A)
        List _A ->
            List (substituteAlternatives ρ0 n r _A)
        Record (Fields kAs ρ) ->
            Record (Fields (map (second (substituteAlternatives ρ0 n r)) kAs) ρ)
        Union (Alternatives kAs0 ρ)
            | Monotype.VariableAlternatives ρ0 == ρ && n == 0 ->
                Union (Alternatives (map (second (substituteAlternatives ρ0 n r)) kAs1) ρ1)
            | otherwise ->
                Union (Alternatives (map (second (substituteAlternatives ρ0 n r)) kAs0) ρ)
          where
            kAs1 = kAs0 <> map (second (first (\_ -> location))) kτs
        Scalar scalar ->
            Scalar scalar

{-| Count how many times the given `Existential` `Type` variable appears within
    a `Type`
-}
typeFreeIn :: Existential Monotype -> Type s h -> Bool
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
fieldsFreeIn :: Existential Monotype.Record -> Type s h -> Bool
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
alternativesFreeIn :: Existential Monotype.Union -> Type s h -> Bool
alternativesFreeIn unsolved =
    Lens.has
        ( Lens.cosmos
        . the @"node"
        . _As @"Union"
        . the @2
        . _As @"UnsolvedAlternatives"
        . Lens.only unsolved
        )

prettyQuantifiedType :: Pretty h => Node s h -> Doc AnsiStyle
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
            keyword "forall"
        <>  " "
        <>  punctuation "("
        <>  label (pretty a)
        <>  " "
        <>  punctuation ":"
        <>  " "
        <>  pretty domain
        <>  punctuation ")"
        <>  " "
        <>  punctuation "."
        <>  " "
        <>  liftType prettyShort _A
    prettyShort (Exists _ a domain _A) =
            keyword "exists"
        <>  " "
        <>  punctuation "("
        <>  label (pretty a)
        <>  " "
        <>  punctuation ":"
        <>  " "
        <>  pretty domain
        <>  punctuation ")"
        <>  " "
        <>  punctuation "."
        <>  " "
        <>  liftType prettyShort _A
    prettyShort _A =
        prettyFunctionType _A

    prettyLong (Forall _ a domain _A) =
            keyword "forall"
        <>  " "
        <>  punctuation "("
        <>  label (pretty a)
        <>  " "
        <>  punctuation ":"
        <>  " "
        <>  pretty domain
        <>  punctuation ")"
        <>  " "
        <>  punctuation "."
        <>  Pretty.hardline
        <>  liftType prettyLong _A
    prettyLong (Exists _ a domain _A) =
            keyword "exists"
        <>  " "
        <>  punctuation "("
        <>  label (pretty a)
        <>  " "
        <>  punctuation ":"
        <>  " "
        <>  pretty domain
        <>  punctuation ")"
        <>  " "
        <>  punctuation "."
        <>  Pretty.hardline
        <>  liftType prettyLong _A
    prettyLong _A =
        "  " <> prettyFunctionType _A

prettyFunctionType :: Pretty h => Node s h -> Doc AnsiStyle
prettyFunctionType  type_@Function{} = Pretty.group (Pretty.flatAlt long short)
  where
    long = Pretty.align (prettyLong type_)

    short = prettyShort type_

    prettyShort (Function _A _B) =
            liftType prettyApplicationType _A
        <>  " "
        <>  punctuation "->"
        <>  " "
        <>  liftType prettyShort _B
    prettyShort _A =
        prettyApplicationType _A

    prettyLong (Function _A _B) =
            liftType prettyApplicationType _A
        <>  " "
        <>  punctuation "->"
        <>  Pretty.hardline
        <>  liftType prettyLong _B
    prettyLong _A =
        "  " <> prettyApplicationType _A
prettyFunctionType other =
    prettyApplicationType other

prettyApplicationType :: Pretty h => Node s h -> Doc AnsiStyle
prettyApplicationType (Optional _A) = Pretty.group (Pretty.flatAlt long short)
  where
    short = builtin "Optional" <> " " <> liftType prettyPrimitiveType _A

    long =
        Pretty.align
            (   builtin "Optional"
            <>  Pretty.hardline
            <>  "  "
            <>  liftType prettyPrimitiveType _A
            )
prettyApplicationType (List _A) = Pretty.group (Pretty.flatAlt long short)
  where
    short = builtin "List" <> " " <> liftType prettyPrimitiveType _A

    long =
        Pretty.align
            (   builtin "List"
            <>  Pretty.hardline
            <>  "  "
            <>  liftType prettyPrimitiveType _A
            )
prettyApplicationType other =
    prettyPrimitiveType other

prettyPrimitiveType :: Pretty h => Node s h -> Doc AnsiStyle
prettyPrimitiveType (VariableType a) =
    label (pretty a)
prettyPrimitiveType (UnsolvedType a) =
    label (pretty a <> "?")
prettyPrimitiveType (TypeHole h) =
    pretty h
prettyPrimitiveType (Record r) =
    prettyRecordType r
prettyPrimitiveType (Union u) =
    prettyUnionType u
prettyPrimitiveType (Scalar scalar) =
    pretty scalar
prettyPrimitiveType other =
    Pretty.group (Pretty.flatAlt long short)
  where
    short = punctuation "(" <> prettyQuantifiedType other <> punctuation ")"

    long =
        Pretty.align
            (   punctuation "("
            <>  " "
            <>  prettyQuantifiedType other
            <>  Pretty.hardline
            <>  punctuation ")"
            )

prettyRecordType :: Pretty h => Record s h -> Doc AnsiStyle
prettyRecordType (Fields [] EmptyFields) =
    punctuation "{" <> " " <> punctuation "}"
prettyRecordType (Fields [] (UnsolvedFields ρ)) =
        punctuation "{"
    <>  " "
    <>  label (pretty ρ <> "?")
    <>  " "
    <>  punctuation "}"
prettyRecordType (Fields [] HoleFields) =
        punctuation "{"
    <>  " "
    <>  keyword "?"
    <>  " "
    <>  punctuation "}"
prettyRecordType (Fields [] (VariableFields ρ)) =
    punctuation "{" <> " " <> label (pretty ρ) <> " " <> punctuation "}"
prettyRecordType (Fields (keyType : keyTypes) fields) =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
            punctuation "{"
        <>  " "
        <>  prettyShortFieldType keyType
        <>  foldMap (\ft -> punctuation "," <> " " <> prettyShortFieldType ft) keyTypes
        <>  case fields of
                EmptyFields ->
                        " "
                    <>  punctuation "}"
                UnsolvedFields ρ ->
                        punctuation ","
                    <>  " "
                    <>  label (pretty ρ <> "?")
                    <>  " "
                    <>  punctuation "}"
                HoleFields ->
                        punctuation ","
                    <>  " "
                    <>  keyword "?"
                    <>  " "
                    <>  punctuation "}"
                VariableFields ρ ->
                        punctuation ","
                    <>  " "
                    <>  label (pretty ρ)
                    <>  " "
                    <>  punctuation "}"

    long =
        Pretty.align
            (   punctuation "{"
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
                    HoleFields ->
                            punctuation ","
                        <>  " "
                        <>  keyword "?"
                        <>  Pretty.hardline
                        <>  punctuation "}"
                    VariableFields ρ ->
                            punctuation ","
                        <>  " "
                        <>  label (pretty ρ)
                        <>  Pretty.hardline
                        <>  punctuation "}"
            )

    prettyShortFieldType :: Pretty h => (Text, Type s h) -> Doc AnsiStyle
    prettyShortFieldType (key, type_) =
            prettyRecordLabel False key
        <>  operator ":"
        <>  " "
        <>  liftType prettyQuantifiedType type_

    prettyLongFieldType :: Pretty h => (Text, Type s h) -> Doc AnsiStyle
    prettyLongFieldType (key, type_) =
            prettyRecordLabel False key
        <>  operator ":"
        <>  Pretty.group (Pretty.flatAlt (Pretty.hardline <> "    ") " ")
        <>  liftType prettyQuantifiedType type_
        <>  Pretty.hardline

prettyUnionType :: Pretty h => Union s h -> Doc AnsiStyle
prettyUnionType (Alternatives [] EmptyAlternatives) =
    punctuation "<" <> " " <> punctuation ">"
prettyUnionType (Alternatives [] (UnsolvedAlternatives ρ)) =
        punctuation "<"
    <>  " "
    <>  label (pretty ρ <> "?")
    <>  " "
    <>  punctuation ">"
prettyUnionType (Alternatives [] HoleAlternatives) =
        punctuation "<"
    <>  " "
    <>  keyword "?"
    <>  " "
    <>  punctuation ">"
prettyUnionType (Alternatives [] (VariableAlternatives ρ)) =
    punctuation "<" <> " " <> label (pretty ρ) <> " " <> punctuation ">"
prettyUnionType (Alternatives (keyType : keyTypes) alternatives) =
    Pretty.group (Pretty.flatAlt long short)
  where
    short =
            punctuation "<"
        <>  " "
        <>  prettyShortAlternativeType keyType
        <>  foldMap (\kt -> " " <> punctuation "|" <> " " <> prettyShortAlternativeType kt) keyTypes
        <>  case alternatives of
                EmptyAlternatives ->
                        " "
                    <>  punctuation ">"
                UnsolvedAlternatives ρ ->
                        " "
                    <>  punctuation "|"
                    <>  " "
                    <>  label (pretty ρ <> "?")
                    <>  " "
                    <>  punctuation ">"
                HoleAlternatives ->
                        " "
                    <>  punctuation "|"
                    <>  " "
                    <>  keyword "?"
                    <>  " "
                    <>  punctuation ">"
                VariableAlternatives ρ ->
                        " "
                    <>  punctuation "|"
                    <>  " "
                    <>  label (pretty ρ)
                    <>  " "
                    <>  punctuation ">"

    long  =
        Pretty.align
            (   punctuation "<"
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
                    HoleAlternatives ->
                            punctuation "|"
                        <>  " "
                        <>  keyword "?"
                        <>  Pretty.hardline
                        <>  punctuation ">"
                    VariableAlternatives ρ ->
                            punctuation "|"
                        <>  " "
                        <>  label (pretty ρ)
                        <>  Pretty.hardline
                        <>  punctuation ">"
            )

    prettyShortAlternativeType (key, type_) =
            label (pretty key)
        <>  operator ":"
        <>  " "
        <>  liftType prettyQuantifiedType type_

    prettyLongAlternativeType (key, type_) =
            label (pretty key)
        <>  operator ":"
        <>  Pretty.group (Pretty.flatAlt (Pretty.hardline <> "    ") " ")
        <>  liftType prettyQuantifiedType type_
        <>  Pretty.hardline

-- | Pretty-print a @Text@ literal
prettyTextLiteral :: Text -> Doc AnsiStyle
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
    -> Doc AnsiStyle
prettyRecordLabel alwaysQuote field
    | Lexer.validRecordLabel field && not alwaysQuote =
        label (pretty field)
    | otherwise =
        label (prettyTextLiteral field)
