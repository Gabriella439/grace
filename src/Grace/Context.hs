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
      Solution(..)
    , Entry(..)
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
   >>> import Grace.Type (Type(location,node))
-}

-- | This type is used to track whether variables within the `Context` are
-- `Solved` or `Unsolved`
--
-- This is equivalent to `Maybe`, but with a nicer name
data Solution a = Unsolved | Solved a
    deriving (Eq, Show)

-- | An element of the `Context` list
data Entry s
    = Variable Domain Text
    -- ^ Universally quantified variable
    --
    -- >>> pretty @(Entry ()) (Variable Domain.Type "a")
    -- a: Type
    | Annotation Text (Type.Type s)
    -- ^ A bound variable whose type is known
    --
    -- >>> pretty @(Entry ()) (Annotation "x" "a")
    -- x: a
    | Type (Existential Monotype) (Solution Monotype)
    -- ^ A placeholder type variable which is either `Unsolved` or `Solved`
    --
    -- >>> pretty @(Entry ()) (Type 0 Unsolved)
    -- a?
    -- >>> pretty @(Entry ()) (Type 0 (Solved (Monotype.Scalar Monotype.Bool)))
    -- a = Bool
    | Fields (Existential Monotype.Record) (Solution Monotype.Record)
    -- ^ A placeholder fields variable which is either `Unsolved` or `Solved`
    --
    -- >>> pretty @(Entry ()) (Fields 0 Unsolved)
    -- a?
    -- >>> pretty @(Entry ()) (Fields 0 (Solved (Monotype.Fields [("x", "X")] (Monotype.UnsolvedFields 1))))
    -- a = x: X, b?
    | Alternatives (Existential Monotype.Union) (Solution Monotype.Union)
    -- ^ A placeholder alternatives variable which is either `Unsolved` or
    -- `Solved`
    --
    -- >>> pretty @(Entry ()) (Alternatives 0 Unsolved)
    -- a?
    -- >>> pretty @(Entry ()) (Alternatives 0 (Solved (Monotype.Alternatives [("x", "X")] (Monotype.UnsolvedAlternatives 1))))
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
    > , Type 3 (Solved (Monotype.Type 2 Unsolved))
    > , Type 2 Unsolved
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
prettyEntry (Variable domain a) =
    label (pretty a) <> operator ":" <> " " <> pretty domain
prettyEntry (Type a Unsolved) =
    pretty a <> "?"
prettyEntry (Type a (Solved τ)) =
    pretty a <> " " <> punctuation "=" <> " " <> pretty τ
prettyEntry (Fields p Unsolved) =
    pretty p <> "?"
prettyEntry (Fields p (Solved (Monotype.Fields [] Monotype.EmptyFields))) =
    pretty p <> " " <> punctuation "=" <> " " <> punctuation "•"
prettyEntry (Fields p0 (Solved (Monotype.Fields [] (Monotype.UnsolvedFields p1)))) =
        pretty p0
    <>  " "
    <>  punctuation "="
    <>  " "
    <>  pretty p1
    <>  "?"
prettyEntry (Fields p0 (Solved (Monotype.Fields [] Monotype.HoleFields))) =
        pretty p0
    <>  " "
    <>  punctuation "="
    <>  " "
    <>  "?"
prettyEntry (Fields p0 (Solved (Monotype.Fields [] (Monotype.VariableFields p1)))) =
        pretty p0
    <>  " "
    <>  punctuation "="
    <>  " "
    <>  label (pretty p1)
prettyEntry (Fields p (Solved (Monotype.Fields ((k0, τ0) : kτs) fields))) =
        pretty p
    <>  " = "
    <>  label (pretty k0)
    <>  operator ":"
    <>  " "
    <>  pretty τ0
    <>  foldMap prettyFieldType kτs
    <>  case fields of
            Monotype.EmptyFields ->
                ""
            Monotype.UnsolvedFields p1 ->
                punctuation "," <> " " <> pretty p1 <> "?"
            Monotype.HoleFields ->
                punctuation "," <> " " <> "?"
            Monotype.VariableFields p1 ->
                punctuation "," <> " " <> pretty p1
prettyEntry (Alternatives p Unsolved) =
    pretty p <> "?"
prettyEntry (Alternatives p (Solved (Monotype.Alternatives [] Monotype.EmptyAlternatives))) =
    pretty p <> " " <> punctuation "=" <> " " <> punctuation "•"
prettyEntry (Alternatives p0 (Solved (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives p1)))) =
    pretty p0 <> " " <> punctuation "=" <> " " <> pretty p1 <> "?"
prettyEntry (Alternatives p0 (Solved (Monotype.Alternatives [] Monotype.HoleAlternatives))) =
    pretty p0 <> " " <> punctuation "=" <> " " <> "?"
prettyEntry (Alternatives p0 (Solved (Monotype.Alternatives [] (Monotype.VariableAlternatives p1)))) =
    pretty p0 <> " " <> punctuation "=" <> " " <>  label (pretty p1)
prettyEntry (Alternatives p0 (Solved (Monotype.Alternatives ((k0, τ0) : kτs) fields))) =
        pretty p0
    <>  " "
    <>  punctuation "="
    <>  " "
    <>  prettyAlternativeType (k0, τ0)
    <>  foldMap (\kt -> " " <> punctuation "|" <> " " <> prettyAlternativeType kt) kτs
    <>  case fields of
            Monotype.EmptyAlternatives ->
                ""
            Monotype.UnsolvedAlternatives p1 ->
                " " <> punctuation "|" <> " " <> pretty p1 <> "?"
            Monotype.HoleAlternatives ->
                " " <> punctuation "|" <> " " <> "?"
            Monotype.VariableAlternatives p1 ->
                " " <> punctuation "|" <> " " <> label (pretty p1)
prettyEntry (Annotation x a) =
    pretty x <> operator ":" <> " " <> pretty a
prettyEntry (MarkerType a) =
    "➤ " <> pretty a <> ": Type"
prettyEntry (MarkerFields a) =
    "➤ " <> pretty a <> ": Fields"
prettyEntry (MarkerAlternatives a) =
    "➤ " <> pretty a <> ": Alternatives"

prettyFieldType :: (Text, Monotype) -> Doc AnsiStyle
prettyFieldType (k, τ) =
    punctuation "," <> " " <> pretty k <> operator ":" <> " " <> pretty τ

prettyAlternativeType :: (Text, Monotype) -> Doc AnsiStyle
prettyAlternativeType (k, τ) =
    pretty k <> operator ":" <> " " <> pretty τ

{-| Substitute a `Type` using the solved entries of a `Context`

    >>> original = Type.Type{ location = (), node = Type.UnsolvedType 0 }
    >>> pretty original
    a?

    >>> pretty (solveType [ Type 1 Unsolved, Type 0 (Solved (Monotype.Scalar Monotype.Bool)) ] original)
    Bool
-}
solveType :: Context s -> Type.Type s -> Type.Type s
solveType context type_ = foldl snoc type_ context
  where
    snoc t (Type         a (Solved τ)) = Type.solveType         a τ t
    snoc t (Fields       a (Solved r)) = Type.solveFields       a r t
    snoc t (Alternatives a (Solved u)) = Type.solveAlternatives a u t
    snoc t  _                          = t

{-| Substitute a t`Type.Record` using the solved entries of a `Context`

    >>> original = Type.Fields [("x", Type.Type{ location = (), node = Type.Scalar Monotype.Bool })] (Monotype.UnsolvedFields 0)
    >>> pretty original
    { x: Bool, a? }

    >>> entry = Fields 0 (Solved (Monotype.Fields [] Monotype.EmptyFields))
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

    >>> original = Type.Alternatives [("A", Type.Type{ location = (), node = Type.Scalar Monotype.Bool })] (Monotype.UnsolvedAlternatives 0)
    >>> pretty original
    < A: Bool | a? >

    >>> entry = Alternatives 0 (Solved (Monotype.Alternatives [] Monotype.EmptyAlternatives))
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

    >>> original = Type.Type{ location = (), node = Type.Function Type.Type{ location = (), node = Type.UnsolvedType 1 } Type.Type{ location = (), node = Type.UnsolvedType 0 } }
    >>> pretty original
    b? -> a?

    >>> pretty (complete [ Type 1 Unsolved, Type 0 (Solved (Monotype.Scalar Monotype.Bool)) ] original)
    forall (a : Type) . a -> Bool
-}
complete :: Context s -> Type.Type s -> Type.Type s
complete context type_ = do
    State.evalState (Monad.foldM snoc type_ context) 0
  where
    numUnsolved = fromIntegral (length (filter predicate context)) - 1
      where
        predicate (Type         _ Unsolved) = True
        predicate (Fields       _ Unsolved) = True
        predicate (Alternatives _ Unsolved) = True
        predicate  _                        = False

    snoc t (Type a (Solved τ)) = do
        return (Type.solveType a τ t)

    snoc t (Fields a (Solved r)) = do
        return (Type.solveFields a r t)

    snoc t (Alternatives a (Solved u)) = do
        return (Type.solveAlternatives a u t)

    snoc t (Type a Unsolved) | a `Type.typeFreeIn` t = do
        n <- State.get

        State.put $! n + 1

        let b = Existential.toVariable (numUnsolved - n)

        let Type.Type{ location } = t

        let node =
                Type.Forall location b Domain.Type (Type.solveType a (Monotype.VariableType b) t)

        return Type.Type{..}
    snoc t (Fields p Unsolved) | p `Type.fieldsFreeIn` t = do
        n <- State.get

        State.put $! n + 1

        let b = Existential.toVariable (numUnsolved - n)

        let Type.Type{ location } = t

        let node =
                Type.Forall location b Domain.Fields (Type.solveFields p (Monotype.Fields [] (Monotype.VariableFields b)) t)

        return Type.Type{..}
    snoc t (Alternatives p Unsolved) | p `Type.alternativesFreeIn` t = do
        n <- State.get

        State.put $! n + 1

        let b = Existential.toVariable (numUnsolved - n)

        let Type.Type{ location } = t

        let node =
                Type.Forall location b Domain.Alternatives (Type.solveAlternatives p (Monotype.Alternatives [] (Monotype.VariableAlternatives b)) t)

        return Type.Type{..}
    snoc t _ = do
        return t

{-| Split a `Context` into two `Context`s before and after the given
    `Unsolved` `Type` variable.  Neither `Context` contains the variable

    Returns `Nothing` if no such `Unsolved` `Type` variable is present within
    the `Context`

    >>> splitOnUnsolvedType 1 [ Type 1 Unsolved, Type 0 (Solved (Monotype.Scalar Monotype.Bool)) ]
    Just ([],[Type 0 (Solved (Scalar Bool))])
    >>> splitOnUnsolvedType 0 [ Type 1 Unsolved, Type 0 (Solved (Monotype.Scalar Monotype.Bool)) ]
    Nothing
-}
splitOnUnsolvedType
    :: Existential Monotype
    -- ^ `Unsolved` `Type` variable to split on
    -> Context s
    -> Maybe (Context s, Context s)
splitOnUnsolvedType a0 (Type a1 Unsolved : entries)
    | a0 == a1 = return ([], entries)
splitOnUnsolvedType a (entry : entries) = do
    (prefix, suffix) <- splitOnUnsolvedType a entries
    return (entry : prefix, suffix)
splitOnUnsolvedType _ [] = Nothing

{-| Split a `Context` into two `Context`s before and after the given
    `Unsolved` `Fields` variable.  Neither `Context` contains the variable

    Returns `Nothing` if no such `Unsolved` `Fields` variable is present within
    the `Context`

    >>> splitOnUnsolvedFields 1 [ Fields 1 Unsolved, Type 0 (Solved (Monotype.Scalar Monotype.Bool)) ]
    Just ([],[Type 0 (Solved (Scalar Bool))])
    >>> splitOnUnsolvedFields 0 [ Fields 1 Unsolved, Type 0 (Solved (Monotype.Scalar Monotype.Bool)) ]
    Nothing
-}
splitOnUnsolvedFields
    :: Existential Monotype.Record
    -- ^ `Unsolved` `Fields` variable to split on
    -> Context s
    -> Maybe (Context s, Context s)
splitOnUnsolvedFields p0 (Fields p1 Unsolved : entries)
    | p0 == p1 = return ([], entries)
splitOnUnsolvedFields p (entry : entries) = do
    (prefix, suffix) <- splitOnUnsolvedFields p entries
    return (entry : prefix, suffix)
splitOnUnsolvedFields _ [] = Nothing

{-| Split a `Context` into two `Context`s before and after the given
    `Unsolved` `Alternatives` variable.  Neither `Context` contains the variable

    Returns `Nothing` if no such `Unsolved` `Alternatives` variable is present
    within the `Context`

    >>> splitOnUnsolvedAlternatives 1 [ Alternatives 1 Unsolved, Type 0 (Solved (Monotype.Scalar Monotype.Bool)) ]
    Just ([],[Type 0 (Solved (Scalar Bool))])
    >>> splitOnUnsolvedAlternatives 0 [ Alternatives 1 Unsolved, Type 0 (Solved (Monotype.Scalar Monotype.Bool)) ]
    Nothing
-}
splitOnUnsolvedAlternatives
    :: Existential Monotype.Union
    -- ^ `Unsolved` `Alternatives` variable to split on
    -> Context s
    -> Maybe (Context s, Context s)
splitOnUnsolvedAlternatives p0 (Alternatives p1 Unsolved : entries)
    | p0 == p1 = return ([], entries)
splitOnUnsolvedAlternatives p (entry : entries) = do
    (prefix, suffix) <- splitOnUnsolvedAlternatives p entries
    return (entry : prefix, suffix)
splitOnUnsolvedAlternatives _ [] = Nothing

{-| Retrieve a variable's annotated type from a `Context`, given the variable's
    label and index

    >>> lookup "x" 0 [ Annotation "x" Type.Type{ location = (), node = Type.Scalar Monotype.Bool }, Annotation "y" Type.Type{ location = (), node = Type.Scalar Monotype.Natural } ]
    Just (Type {location = (), node = Scalar Bool})
-}
lookup
    :: Text
    -- ^ Variable label
    -> Int
    -- ^ Variable index (See the documentation of `Value.Variable`)
    -> Context s
    -> Maybe (Type.Type s)
lookup _ _ [] =
    Nothing
lookup x0 n (Annotation x1 _A : _Γ) =
    if x0 == x1
    then
       if n <= 0
       then Just _A
       else lookup x0 (n - 1) _Γ
    else lookup x0 n _Γ
lookup x n (_ : _Γ) =
    lookup x n _Γ

{-| Discard all entries from a `Context` up to and including the given `Entry`

    >>> discardUpTo (MarkerType 1) [ Type 1 Unsolved, MarkerType 1, Type 0 Unsolved ]
    [Type 0 Unsolved]
-}
discardUpTo :: Eq s => Entry s -> Context s -> Context s
discardUpTo entry0 (entry1 : _Γ)
    | entry0 == entry1 = _Γ
    | otherwise = discardUpTo entry0 _Γ
discardUpTo _ [] = []
