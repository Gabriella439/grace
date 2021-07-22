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

      -- * Pretty-printing
    , prettyEntry

      -- * Utilities
    , lookup
    , splitOnUnsolvedType
    , splitOnUnsolvedFields
    , splitOnUnsolvedAlternatives
    , discardUpTo
    , complete
    ) where

import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (lift)
import Data.STRef (STRef)
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
import qualified Data.STRef                 as STRef
import qualified Grace.Domain               as Domain
import qualified Grace.Existential          as Existential
import qualified Grace.Monotype             as Monotype
import qualified Grace.Type                 as Type

{- $setup

   >>> :set -XOverloadedStrings
   >>> :set -XTypeApplications
   >>> import Grace.Type (Type(location,node))
   >>> import Data.Void (Void)
-}

-- | This type is used to track whether variables within the `Context` are
-- `Solved` or `Unsolved`
--
-- This is equivalent to `Maybe`, but with a nicer name
data Solution a = Unsolved | Solved a
    deriving (Eq, Show)

-- | An element of the `Context` list
data Entry s a
    = Variable Domain Text
    -- ^ Universally quantified variable
    --
    -- >>> pretty @(Entry Void ()) (Variable Domain.Type "a")
    -- a: Type
    | Annotation Text (Type.Type a)
    -- ^ A bound variable whose type is known
    --
    -- >>> pretty @(Entry Void ()) (Annotation "x" "a")
    -- x: a
    | Type (Existential Monotype) (STRef s (Solution Monotype))
    -- ^ A placeholder type variable which is either `Unsolved` or `Solved`
    --
    -- >>> pretty @(Entry Void ()) (Type 0 Unsolved)
    -- a?
    -- >>> pretty @(Entry Void ()) (Type 0 (Solved (Monotype.Scalar Monotype.Bool)))
    -- a = Bool
    | Fields (Existential Monotype.Record) (Solution Monotype.Record)
    -- ^ A placeholder fields variable which is either `Unsolved` or `Solved`
    --
    -- >>> pretty @(Entry Void ()) (Fields 0 Unsolved)
    -- a?
    -- >>> pretty @(Entry Void ()) (Fields 0 (Solved (Monotype.Fields [("x", "X")] (Monotype.UnsolvedFields 1))))
    -- a = x: X, b?
    | Alternatives (Existential Monotype.Union) (Solution Monotype.Union)
    -- ^ A placeholder alternatives variable which is either `Unsolved` or
    -- `Solved`
    --
    -- >>> pretty @(Entry Void ()) (Alternatives 0 Unsolved)
    -- a?
    -- >>> pretty @(Entry Void ()) (Alternatives 0 (Solved (Monotype.Alternatives [("x", "X")] (Monotype.UnsolvedAlternatives 1))))
    -- a = x: X | b?
    | MarkerType (Existential Monotype)
    -- ^ This is used by the bidirectional type-checking algorithm to separate
    --   context entries introduced before and after type-checking a universally
    --   quantified type
    --
    -- >>> pretty @(Entry Void ()) (MarkerType 0)
    -- ➤ a: Type
    | MarkerFields (Existential Monotype.Record)
    -- ^ This is used by the bidirectional type-checking algorithm to separate
    --   context entries introduced before and after type-checking universally
    --   quantified fields
    --
    -- >>> pretty @(Entry Void ()) (MarkerFields 0)
    -- ➤ a: Fields
    | MarkerAlternatives (Existential Monotype.Union)
    -- ^ This is used by the bidirectional type-checking algorithm to separate
    --   context entries introduced before and after type-checking universally
    --   quantified alternatives
    --
    -- >>> pretty @(Entry Void ()) (MarkerAlternatives 0)
    -- ➤ a: Alternatives
    deriving stock (Eq)

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
type Context s a = [Entry s a]

prettyEntry :: Entry s a -> ST s (Doc AnsiStyle)
prettyEntry (Variable domain a) =
    return (label (pretty a) <> operator ":" <> " " <> pretty domain)
prettyEntry (Type a ref) = do
    solution <- STRef.readSTRef ref

    case solution of
        Unsolved -> do
            return (pretty a <> "?")
        Solved τ -> do
            return (pretty a <> " " <> punctuation "=" <> " " <> pretty τ)
prettyEntry (Fields p Unsolved) =
    return (pretty p <> "?")
prettyEntry (Fields p (Solved (Monotype.Fields [] Monotype.EmptyFields))) =
    return (pretty p <> " " <> punctuation "=" <> " " <> punctuation "•")
prettyEntry (Fields p0 (Solved (Monotype.Fields [] (Monotype.UnsolvedFields p1)))) = do
    let document =
                pretty p0
            <>  " "
            <>  punctuation "="
            <>  " "
            <>  pretty p1
            <>  "?"

    return document
prettyEntry (Fields p0 (Solved (Monotype.Fields [] Monotype.HoleFields))) = do
    let document =
                pretty p0
            <>  " "
            <>  punctuation "="
            <>  " "
            <>  "?"

    return document
prettyEntry (Fields p0 (Solved (Monotype.Fields [] (Monotype.VariableFields p1)))) = do
    let document =
                pretty p0
            <>  " "
            <>  punctuation "="
            <>  " "
            <>  label (pretty p1)

    return document
prettyEntry (Fields p (Solved (Monotype.Fields ((k0, τ0) : kτs) fields))) = do
    let document =
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

    return document
prettyEntry (Alternatives p Unsolved) =
    return (pretty p <> "?")
prettyEntry (Alternatives p (Solved (Monotype.Alternatives [] Monotype.EmptyAlternatives))) =
    return (pretty p <> " " <> punctuation "=" <> " " <> punctuation "•")
prettyEntry (Alternatives p0 (Solved (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives p1)))) =
    return (pretty p0 <> " " <> punctuation "=" <> " " <> pretty p1 <> "?")
prettyEntry (Alternatives p0 (Solved (Monotype.Alternatives [] Monotype.HoleAlternatives))) =
    return (pretty p0 <> " " <> punctuation "=" <> " " <> "?")
prettyEntry (Alternatives p0 (Solved (Monotype.Alternatives [] (Monotype.VariableAlternatives p1)))) =
    return (pretty p0 <> " " <> punctuation "=" <> " " <>  label (pretty p1))
prettyEntry (Alternatives p0 (Solved (Monotype.Alternatives ((k0, τ0) : kτs) fields))) = do
    let document =
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

    return document
prettyEntry (Annotation x a) =
    return (pretty x <> operator ":" <> " " <> pretty a)
prettyEntry (MarkerType a) =
    return ("➤ " <> pretty a <> ": Type")
prettyEntry (MarkerFields a) =
    return ("➤ " <> pretty a <> ": Fields")
prettyEntry (MarkerAlternatives a) =
    return ("➤ " <> pretty a <> ": Alternatives")

prettyFieldType :: (Text, Monotype) -> Doc AnsiStyle
prettyFieldType (k, τ) =
    punctuation "," <> " " <> pretty k <> operator ":" <> " " <> pretty τ

prettyAlternativeType :: (Text, Monotype) -> Doc AnsiStyle
prettyAlternativeType (k, τ) =
    pretty k <> operator ":" <> " " <> pretty τ

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
complete :: Context s a -> Type.Type a -> ST s (Type.Type a)
complete context type_ = do
    let isUnsolved ref = do
            solution <- STRef.readSTRef ref

            return (solution == Unsolved)

    let predicate (Type         _ ref     ) = isUnsolved ref
        predicate (Fields       _ Unsolved) = return True
        predicate (Alternatives _ Unsolved) = return True
        predicate  _                        = return False

    matches <- Monad.filterM predicate context

    let numUnsolved = fromIntegral (length matches) - 1

    let snoc t (Type a ref) = do
            solution <- lift (STRef.readSTRef ref)

            case solution of
                Solved τ -> do
                    return (Type.solveType a τ t)

                Unsolved | a `Type.typeFreeIn` t -> do
                    n <- State.get

                    State.put $! n + 1

                    let b = Existential.toVariable (numUnsolved - n)

                    let Type.Type{ location } = t

                    let node =
                            Type.Forall location b Domain.Type (Type.solveType a (Monotype.VariableType b) t)

                    return Type.Type{..}

                _ -> do
                    return t

        snoc t (Fields a (Solved r)) = do
            return (Type.solveFields a r t)

        snoc t (Alternatives a (Solved u)) = do
            return (Type.solveAlternatives a u t)

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

    State.evalStateT (Monad.foldM snoc type_ context) 0

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
    -> Context s a
    -> Maybe (Context s a, STRef s (Solution Monotype), Context s a)
splitOnUnsolvedType a0 (Type a1 ref : entries)
    | a0 == a1 = do
        return ([], ref, entries)
splitOnUnsolvedType a0 (entry : entries) = do
        ~(prefix, ref, suffix) <- splitOnUnsolvedType a0 entries

        return (entry : prefix, ref, suffix)
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
    -> Context s a
    -> Maybe (Context s a, Context s a)
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
    -> Context s a
    -> Maybe (Context s a, Context s a)
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
    -> Context s a
    -> Maybe (Type.Type a)
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
discardUpTo :: Eq a => Entry s a -> Context s a -> Context s a
discardUpTo entry0 (entry1 : _Γ)
    | entry0 == entry1 = _Γ
    | otherwise = discardUpTo entry0 _Γ
discardUpTo _ [] = []
