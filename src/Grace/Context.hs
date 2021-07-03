{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-| A `Context` is an ordered list of `Entry`s used as the state for the
    bidirectional type-checking algorithm
-}
module Grace.Context
    ( -- * Types
      Entry(..)
    , Context

      -- * Utilities
    , lookup
    , splitOnUnsolvedType
    , splitOnUnsolvedFields
    , splitOnUnsolvedAlternatives
    , discardUpTo
    , solveType
    , solveRecord
    , solveUnion
    , complete
    ) where

import Data.Text (Text)
import Grace.Domain (Domain)
import Grace.Existential (Existential)
import Grace.Monotype (Monotype)
import Grace.Pretty (Pretty(..), label, operator, punctuation)
import Grace.Type (Type(..))
import Prelude hiding (lookup)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)

import qualified Control.Monad              as Monad
import qualified Control.Monad.State.Strict as State
import qualified Grace.Domain               as Domain
import qualified Grace.Existential          as Existential
import qualified Grace.Monotype             as Monotype
import qualified Grace.Type                 as Type

{- $setup

   >>> :set -XOverloadedStrings
   >>> :set -XTypeApplications
-}

-- | An element of the `Context` list
data Entry s
    = Variable Domain Text
    -- ^ Universally quantified variable
    --
    -- >>> pretty @(Entry ()) (Variable Domain.Type "a")
    -- a: Type
    | Annotation Text (Type s)
    -- ^ A bound variable whose type is known
    --
    -- >>> pretty @(Entry ()) (Annotation "x" "a")
    -- x: a
    | UnsolvedType (Existential Monotype)
    -- ^ A placeholder type variable whose type has not yet been inferred
    --
    -- >>> pretty @(Entry ()) (UnsolvedType 0)
    -- a?
    | UnsolvedFields (Existential Monotype.Record)
    -- ^ A placeholder fields variable whose type has not yet been inferred
    --
    -- >>> pretty @(Entry ()) (UnsolvedFields 0)
    -- a?
    | UnsolvedAlternatives (Existential Monotype.Union)
    -- ^ A placeholder alternatives variable whose type has not yet been
    -- inferred
    --
    -- >>> pretty @(Entry ()) (UnsolvedAlternatives 0)
    -- a?
    | SolvedType (Existential Monotype) Monotype
    -- ^ A placeholder type variable whose type has been (at least partially)
    --   inferred
    --
    -- >>> pretty @(Entry ()) (SolvedType 0 (Monotype.Scalar Monotype.Bool))
    -- a = Bool
    | SolvedFields (Existential Monotype.Record) Monotype.Record
    -- ^ A placeholder fields variable whose type has been (at least partially)
    --   inferred
    --
    -- >>> pretty @(Entry ()) (SolvedFields 0 (Monotype.Fields [("x", "X")] (Monotype.UnsolvedFields 1)))
    -- a = x: X, b?
    | SolvedAlternatives (Existential Monotype.Union) Monotype.Union
    -- ^ A placeholder alternatives variable whose type has been (at least
    --   partially) inferred
    --
    -- >>> pretty @(Entry ()) (SolvedAlternatives 0 (Monotype.Alternatives [("x", "X")] (Monotype.UnsolvedAlternatives 1)))
    -- a = x: X | b?
    | MarkerType (Existential Monotype)
    -- ^ This is used by the bidirectional type-checking algorithm to separate
    --   context entries introduced before and after type-checking a universally
    --   quantified type
    --
    -- >>> pretty @(Entry ()) (MarkerType 0)
    -- ➤ a: Type
    | MarkerFields (Existential Monotype.Record)
    -- ^ This is used by the bidirectional type-checking algorithm to separate
    --   context entries introduced before and after type-checking universally
    --   quantified fields
    --
    -- >>> pretty @(Entry ()) (MarkerFields 0)
    -- ➤ a: Fields
    | MarkerAlternatives (Existential Monotype.Union)
    -- ^ This is used by the bidirectional type-checking algorithm to separate
    --   context entries introduced before and after type-checking universally
    --   quantified alternatives
    --
    -- >>> pretty @(Entry ()) (MarkerAlternatives 0)
    -- ➤ a: Alternatives
    deriving stock (Eq, Show)

instance Pretty (Entry s) where
    pretty = prettyEntry

{-| A `Context` is an ordered list of `Entry`s

    Note that this representation stores the `Context` entries in reverse
    order, meaning that the beginning of the list represents the entries that
    were added last.  For example, this context:

    > •, a : Bool, b, c?, d = c?, ➤e : Type

    … corresponds to this Haskell representation:

    > [ MarkerType 4
    > , SolvedType 3 (Monotype.UnsolvedType 2)
    > , UnsolvedType 2
    > , Variable "b"
    > , Annotation "a" (Monotype.Scalar Monotype.Bool)
    > ]

    The ordering matters because the bidirectional type-checking algorithm
    uses ordering of `Context` entries to determine scope.  Specifically:

    * each `Entry` in the `Context` can only refer to variables preceding it
      within the `Context`

    * the bidirectional type-checking algorithm sometimes discards all entries
      in the context past a certain entry to reflect the end of their
      \"lifetime\"
-}
type Context s = [Entry s]

prettyEntry :: Entry s -> Doc AnsiStyle
prettyEntry (Variable domain α) =
    label (pretty α) <> operator ":" <> " " <> pretty domain
prettyEntry (UnsolvedType α) =
    pretty α <> "?"
prettyEntry (UnsolvedFields ρ) =
    pretty ρ <> "?"
prettyEntry (UnsolvedAlternatives ρ) =
    pretty ρ <> "?"
prettyEntry (SolvedType α τ) =
    pretty α <> " " <> punctuation "=" <> " " <> pretty τ
prettyEntry (SolvedFields ρ (Monotype.Fields [] Monotype.EmptyFields)) =
    pretty ρ <> " " <> punctuation "=" <> " " <> punctuation "•"
prettyEntry (SolvedFields ρ₀ (Monotype.Fields [] (Monotype.UnsolvedFields ρ₁))) =
        pretty ρ₀
    <>  " "
    <>  punctuation "="
    <>  " "
    <>  pretty ρ₁
    <>  "?"
prettyEntry (SolvedFields ρ₀ (Monotype.Fields [] (Monotype.VariableFields ρ₁))) =
        pretty ρ₀
    <>  " "
    <>  punctuation "="
    <>  " "
    <>  label (pretty ρ₁)
prettyEntry (SolvedFields ρ (Monotype.Fields ((k₀, τ₀) : kτs) fields)) =
        pretty ρ
    <>  " = "
    <>  label (pretty k₀)
    <>  operator ":"
    <>  " "
    <>  pretty τ₀
    <>  foldMap prettyFieldType kτs
    <>  case fields of
            Monotype.EmptyFields ->
                ""
            Monotype.UnsolvedFields ρ₁ ->
                punctuation "," <> " " <> pretty ρ₁ <> "?"
            Monotype.VariableFields ρ₁ ->
                punctuation "," <> " " <> pretty ρ₁
prettyEntry (SolvedAlternatives ρ (Monotype.Alternatives [] Monotype.EmptyAlternatives)) =
    pretty ρ <> " " <> punctuation "=" <> " " <> punctuation "•"
prettyEntry (SolvedAlternatives ρ₀ (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives ρ₁))) =
    pretty ρ₀ <> " " <> punctuation "=" <> " " <> pretty ρ₁ <> "?"
prettyEntry (SolvedAlternatives ρ₀ (Monotype.Alternatives [] (Monotype.VariableAlternatives ρ₁))) =
    pretty ρ₀ <> " " <> punctuation "=" <> " " <>  label (pretty ρ₁)
prettyEntry (SolvedAlternatives ρ₀ (Monotype.Alternatives ((k₀, τ₀) : kτs) fields)) =
        pretty ρ₀
    <>  " "
    <>  punctuation "="
    <>  " "
    <>  prettyAlternativeType (k₀, τ₀)
    <>  foldMap (\kt -> " " <> punctuation "|" <> " " <> prettyAlternativeType kt) kτs
    <>  case fields of
            Monotype.EmptyAlternatives ->
                ""
            Monotype.UnsolvedAlternatives ρ₁ ->
                " " <> punctuation "|" <> " " <> pretty ρ₁ <> "?"
            Monotype.VariableAlternatives ρ₁ ->
                " " <> punctuation "|" <> " " <> label (pretty ρ₁)
prettyEntry (Annotation x α) =
    pretty x <> operator ":" <> " " <> pretty α
prettyEntry (MarkerType α) =
    "➤ " <> pretty α <> ": Type"
prettyEntry (MarkerFields α) =
    "➤ " <> pretty α <> ": Fields"
prettyEntry (MarkerAlternatives α) =
    "➤ " <> pretty α <> ": Alternatives"

prettyFieldType :: (Text, Monotype) -> Doc AnsiStyle
prettyFieldType (k, τ) =
    punctuation "," <> " " <> pretty k <> operator ":" <> " " <> pretty τ

prettyAlternativeType :: (Text, Monotype) -> Doc AnsiStyle
prettyAlternativeType (k, τ) =
    pretty k <> operator ":" <> " " <> pretty τ

{-| Substitute a `Type` using the solved entries of a `Context`

    >>> original = Type{ location = (), node = Type.UnsolvedType 0 }
    >>> pretty original
    a?

    >>> pretty (solveType [ UnsolvedType 1, SolvedType 0 (Monotype.Scalar Monotype.Bool) ] original)
    Bool
-}
solveType :: Context s -> Type s -> Type s
solveType context type_ = foldl snoc type_ context
  where
    snoc t (SolvedType         α τ) = Type.solveType         α τ t
    snoc t (SolvedFields       α r) = Type.solveFields       α r t
    snoc t (SolvedAlternatives α u) = Type.solveAlternatives α u t
    snoc t  _                       = t

{-| Substitute a t`Type.Record` using the solved entries of a `Context`

    >>> original = Type.Fields [("x", Type{ location = (), node = Type.Scalar Monotype.Bool })] (Monotype.UnsolvedFields 0)
    >>> pretty original
    { x: Bool, a? }

    >>> entry = SolvedFields 0 (Monotype.Fields [] Monotype.EmptyFields)
    >>> pretty entry
    a = •

    >>> pretty (solveRecord [ entry ] original)
    { x: Bool }
-}
solveRecord :: Context s -> Type.Record s -> Type.Record s
solveRecord context record = record'
  where
    -- TODO: Come up with total solution
    Type.Type{ node = Type.Record record' } =
        solveType context Type.Type
            { location = error "Grace.Context.solveRecord: Internal error - Missing location field"
            , node = Type.Record record
            }

{-| Substitute a t`Type.Union` using the solved entries of a `Context`
    `Context`

    >>> original = Type.Alternatives [("A", Type{ location = (), node = Type.Scalar Monotype.Bool })] (Monotype.UnsolvedAlternatives 0)
    >>> pretty original
    < A: Bool | a? >

    >>> entry = SolvedAlternatives 0 (Monotype.Alternatives [] Monotype.EmptyAlternatives)
    >>> pretty entry
    a = •

    >>> pretty (solveUnion [ entry ] original)
    < A: Bool >
-}
solveUnion :: Context s -> Type.Union s -> Type.Union s
solveUnion context union = union'
  where
    -- TODO: Come up with total solution
    Type.Type{ node = Type.Union union' }=
        solveType context Type.Type
            { location = error "Grace.Context.solveUnion: Internal error - Missing location field"
            , node = Type.Union union
            }

{-| This function is used at the end of the bidirectional type-checking
    algorithm to complete the inferred type by:

    * Substituting the type with the solved entries in the `Context`

    * Adding universal quantifiers for all unsolved entries in the `Context`

    >>> original = Type{ location = (), node = Type.Function Type{ location = (), node = Type.UnsolvedType 1 } Type{ location = (), node = Type.UnsolvedType 0 } }
    >>> pretty original
    b? -> a?

    >>> pretty (complete [ UnsolvedType 1, SolvedType 0 (Monotype.Scalar Monotype.Bool) ] original)
    forall (a : Type) . a -> Bool
-}
complete :: Context s -> Type s -> Type s
complete context type_ = do
    State.evalState (Monad.foldM snoc type_ context) 0
  where
    numUnsolved = fromIntegral (length (filter predicate context)) - 1
      where
        predicate (UnsolvedType         _) = True
        predicate (UnsolvedFields       _) = True
        predicate (UnsolvedAlternatives _) = True
        predicate  _                       = False

    snoc t (SolvedType         α τ) = do return (Type.solveType         α τ t)
    snoc t (SolvedFields       α r) = do return (Type.solveFields       α r t)
    snoc t (SolvedAlternatives α r) = do return (Type.solveAlternatives α r t)
    snoc t (UnsolvedType α) | α `Type.typeFreeIn` t = do
        n <- State.get

        State.put $! n + 1

        let a = Existential.toVariable (numUnsolved - n)

        let Type{ location } = t

        let node =
                Type.Forall location a Domain.Type (Type.solveType α (Monotype.VariableType a) t)

        return Type.Type{..}
    snoc t (UnsolvedFields ρ) | ρ `Type.fieldsFreeIn` t = do
        n <- State.get

        State.put $! n + 1

        let a = Existential.toVariable (numUnsolved - n)

        let Type{ location } = t

        let node =
                Type.Forall location a Domain.Fields (Type.solveFields ρ (Monotype.Fields [] (Monotype.VariableFields a)) t)

        return Type.Type{..}
    snoc t (UnsolvedAlternatives ρ) | ρ `Type.alternativesFreeIn` t = do
        n <- State.get

        State.put $! n + 1

        let a = Existential.toVariable (numUnsolved - n)

        let Type{ location } = t

        let node =
                Type.Forall location a Domain.Alternatives (Type.solveAlternatives ρ (Monotype.Alternatives [] (Monotype.VariableAlternatives a)) t)

        return Type.Type{..}
    snoc t _ = do
        return t

{-| Split a `Context` into two `Context`s before and after the given
    `UnsolvedType` variable.  Neither `Context` contains the variable

    Returns `Nothing` if no such `UnsolvedType` variable is present within the
    `Context`

    >>> splitOnUnsolvedType 1 [ UnsolvedType 1, SolvedType 0 (Monotype.Scalar Monotype.Bool) ]
    Just ([],[SolvedType 0 (Scalar Bool)])
    >>> splitOnUnsolvedType 0 [ UnsolvedType 1, SolvedType 0 (Monotype.Scalar Monotype.Bool) ]
    Nothing
-}
splitOnUnsolvedType
    :: Existential Monotype
    -- ^ `UnsolvedType` variable to split on
    -> Context s
    -> Maybe (Context s, Context s)
splitOnUnsolvedType α₀ (UnsolvedType α₁ : entries)
    | α₀ == α₁ = return ([], entries)
splitOnUnsolvedType α (entry : entries) = do
    (prefix, suffix) <- splitOnUnsolvedType α entries
    return (entry : prefix, suffix)
splitOnUnsolvedType _ [] = Nothing

{-| Split a `Context` into two `Context`s before and after the given
    `UnsolvedFields` variable.  Neither `Context` contains the variable

    Returns `Nothing` if no such `UnsolvedFields` variable is present within the
    `Context`

    >>> splitOnUnsolvedFields 1 [ UnsolvedFields 1, SolvedType 0 (Monotype.Scalar Monotype.Bool) ]
    Just ([],[SolvedType 0 (Scalar Bool)])
    >>> splitOnUnsolvedFields 0 [ UnsolvedFields 1, SolvedType 0 (Monotype.Scalar Monotype.Bool) ]
    Nothing
-}
splitOnUnsolvedFields
    :: Existential Monotype.Record
    -- ^ `UnsolvedFields` variable to split on
    -> Context s
    -> Maybe (Context s, Context s)
splitOnUnsolvedFields ρ₀ (UnsolvedFields ρ₁ : entries)
    | ρ₀ == ρ₁ = return ([], entries)
splitOnUnsolvedFields ρ (entry : entries) = do
    (prefix, suffix) <- splitOnUnsolvedFields ρ entries
    return (entry : prefix, suffix)
splitOnUnsolvedFields _ [] = Nothing

{-| Split a `Context` into two `Context`s before and after the given
    `UnsolvedAlternatives` variable.  Neither `Context` contains the variable

    Returns `Nothing` if no such `UnsolvedAlternatives` variable is present
    within the `Context`

    >>> splitOnUnsolvedAlternatives 1 [ UnsolvedAlternatives 1, SolvedType 0 (Monotype.Scalar Monotype.Bool) ]
    Just ([],[SolvedType 0 (Scalar Bool)])
    >>> splitOnUnsolvedAlternatives 0 [ UnsolvedAlternatives 1, SolvedType 0 (Monotype.Scalar Monotype.Bool) ]
    Nothing
-}
splitOnUnsolvedAlternatives
    :: Existential Monotype.Union
    -- ^ `UnsolvedAlternatives` variable to split on
    -> Context s
    -> Maybe (Context s, Context s)
splitOnUnsolvedAlternatives ρ₀ (UnsolvedAlternatives ρ₁ : entries)
    | ρ₀ == ρ₁ = return ([], entries)
splitOnUnsolvedAlternatives ρ (entry : entries) = do
    (prefix, suffix) <- splitOnUnsolvedAlternatives ρ entries
    return (entry : prefix, suffix)
splitOnUnsolvedAlternatives _ [] = Nothing

{-| Retrieve a variable's annotated type from a `Context`, given the variable's
    label and index

    >>> lookup "x" 0 [ Annotation "x" Type{ location = (), node = Type.Scalar Monotype.Bool }, Annotation "y" Type{ location = (), node = Type.Scalar Monotype.Natural } ]
    Just (Type {location = (), node = Scalar Bool})
-}
lookup
    :: Text
    -- ^ Variable label
    -> Int
    -- ^ Variable index (See the documentation of `Value.Variable`)
    -> Context s
    -> Maybe (Type s)
lookup _ _ [] =
    Nothing
lookup x₀ n (Annotation x₁ _A : _Γ) =
    if x₀ == x₁
    then
       if n <= 0
       then Just _A
       else lookup x₀ (n - 1) _Γ
    else lookup x₀ n _Γ
lookup x n (_ : _Γ) =
    lookup x n _Γ

{-| Discard all entries from a `Context` up to and including the given `Entry`

    >>> discardUpTo (MarkerType 1) [ UnsolvedType 1, MarkerType 1, UnsolvedType 0 ]
    [UnsolvedType 0]
-}
discardUpTo :: Eq s => Entry s -> Context s -> Context s
discardUpTo entry₀ (entry₁ : _Γ)
    | entry₀ == entry₁ = _Γ
    | otherwise = discardUpTo entry₀ _Γ
discardUpTo _ [] = []
