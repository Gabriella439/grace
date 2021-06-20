{-| A `Context` is an ordered list of `Entry`s used as the state for the
    bidirectional type-checking algorithm
-}
module Grace.Context
    ( -- * Types
      Entry(..)
    , Context

      -- * Utilities
    , lookup
    , splitOnUnsolved
    , splitOnUnsolvedFields
    , splitOnUnsolvedAlternatives
    , discardUpTo
    , solve
    , solveRecord
    , solveUnion
    , complete
    ) where

import Data.Text (Text)
import Grace.Domain (Domain)
import Grace.Existential (Existential)
import Grace.Monotype (Monotype)
import Grace.Type (Type(..))
import Prelude hiding (lookup)
import Prettyprinter (Doc, Pretty(..))

import qualified Control.Monad              as Monad
import qualified Control.Monad.State.Strict as State
import qualified Grace.Domain               as Domain
import qualified Grace.Existential          as Existential
import qualified Grace.Monotype             as Monotype
import qualified Grace.Type                 as Type
import qualified Prettyprinter              as Pretty

-- | An element of the `Context` list
data Entry s
    = Variable Domain Text
    -- ^ Type variable
    --
    -- >>> pretty @(Entry ()) (Variable Domain.Type "a")
    -- a
    | Annotation Text (Type s)
    -- ^ A bound variable whose type is known
    --
    -- >>> pretty @(Entry ()) (Annotation "x" "a")
    -- x : a
    | Unsolved (Existential Monotype)
    -- ^ A placeholder type variable whose type has not yet been inferred
    --
    -- >>> pretty @(Entry ()) (Unsolved 0)
    -- a?
    | UnsolvedFields (Existential Monotype.Record)
    -- ^ A placeholder fields variable whose type has not yet been inferred
    --
    -- >>> pretty @(Entry ()) (UnsolvedFields 0)
    -- |a?
    | UnsolvedAlternatives (Existential Monotype.Union)
    -- ^ A placeholder alternatives variable whose type has not yet been
    -- inferred
    --
    -- >>> pretty @(Entry ()) (UnsolvedAlternatives 0)
    -- |a?
    | Solved (Existential Monotype) Monotype
    -- ^ A placeholder type variable whose type has been (at least partially)
    --   inferred
    --
    -- >>> pretty @(Entry ()) (Solved 0 Monotype.Bool)
    -- a = Bool
    | SolvedFields (Existential Monotype.Record) Monotype.Record
    -- ^ A placeholder fields variable whose type has been (at least partially)
    --   inferred
    --
    -- >>> pretty @(Entry ()) (SolvedFields 0 (Monotype.Fields [("x", "X")] (Monotype.UnsolvedFields 1)))
    -- a = x : X | b?
    | SolvedAlternatives (Existential Monotype.Union) Monotype.Union
    -- ^ A placeholder alternatives variable whose type has been (at least
    --   partially) inferred
    --
    -- >>> pretty @(Entry ()) (SolvedAlternatives 0 (Monotype.Alternatives [("x", "X")] (Monotype.UnsolvedAlternatives 1)))
    -- a = x : X | b?
    | Marker (Existential Monotype)
    -- ^ This is used by the bidirectional type-checking algorithm to separate
    --   context entries introduced before and after type-checking a universally
    --   quantified type
    --
    -- >>> pretty @(Entry ()) (Marker 0)
    -- ➤a : Type
    | MarkerFields (Existential Monotype.Record)
    -- ^ This is used by the bidirectional type-checking algorithm to separate
    --   context entries introduced before and after type-checking universally
    --   quantified fields
    --
    -- >>> pretty @(Entry ()) (MarkerFields 0)
    -- ➤a : Fields
    | MarkerAlternatives (Existential Monotype.Union)
    -- ^ This is used by the bidirectional type-checking algorithm to separate
    --   context entries introduced before and after type-checking universally
    --   quantified alternatives
    --
    -- >>> pretty @(Entry ()) (MarkerAlternatives 0)
    -- ➤a : Alternatives
    deriving stock (Eq, Show)

instance Pretty (Entry s) where
    pretty = prettyEntry

{-| A `Context` is an ordered list of `Entry`s

    Note that this representation stores the `Context` entries in reverse
    order, meaning that the beginning of the list represents the entries that
    were added last.  For example, this context:

    > ·, a : Bool, b, c?, d = c?, ➤e

    … corresponds to this Haskell representation:

    > [ Marker 4
    > , Solved 3 (Monotype.Unsolved 2)
    > , Unsolved 2
    > , Variable "b"
    > , Annotation "a" Type.Bool
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

prettyEntry :: Entry s -> Doc a
prettyEntry (Variable _ α) =
    Pretty.pretty α
prettyEntry (Unsolved α) =
    Pretty.pretty α <> "?"
prettyEntry (UnsolvedFields ρ) =
    "|" <> Pretty.pretty ρ <> "?"
prettyEntry (UnsolvedAlternatives ρ) =
    "|" <> Pretty.pretty ρ <> "?"
prettyEntry (Solved α τ) =
    Pretty.pretty α <> " = " <> Pretty.pretty τ
prettyEntry (SolvedFields ρ (Monotype.Fields [] Monotype.EmptyFields)) =
    Pretty.pretty ρ <> " = •"
prettyEntry (SolvedFields ρ₀ (Monotype.Fields [] (Monotype.UnsolvedFields ρ₁))) =
    Pretty.pretty ρ₀ <> " = • | " <>  Pretty.pretty ρ₁ <> "?"
prettyEntry (SolvedFields ρ₀ (Monotype.Fields [] (Monotype.VariableFields ρ₁))) =
    Pretty.pretty ρ₀ <> " = • | " <>  Pretty.pretty ρ₁
prettyEntry (SolvedFields ρ (Monotype.Fields ((k₀, τ₀) : kτs) fields)) =
        Pretty.pretty ρ
    <>  " = "
    <>  Pretty.pretty k₀
    <>  " : "
    <>  Pretty.pretty τ₀
    <>  foldMap prettyFieldType kτs
    <>  case fields of
            Monotype.EmptyFields       -> ""
            Monotype.UnsolvedFields ρ₁ -> " | " <> Pretty.pretty ρ₁ <> "?"
            Monotype.VariableFields ρ₁ -> " | " <> Pretty.pretty ρ₁
prettyEntry (SolvedAlternatives ρ (Monotype.Alternatives [] Monotype.EmptyAlternatives)) =
    Pretty.pretty ρ <> " = •"
prettyEntry (SolvedAlternatives ρ₀ (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives ρ₁))) =
    Pretty.pretty ρ₀ <> " = • | " <>  Pretty.pretty ρ₁ <> "?"
prettyEntry (SolvedAlternatives ρ₀ (Monotype.Alternatives [] (Monotype.VariableAlternatives ρ₁))) =
    Pretty.pretty ρ₀ <> " = • | " <>  Pretty.pretty ρ₁
prettyEntry (SolvedAlternatives ρ₀ (Monotype.Alternatives ((k₀, τ₀) : kτs) fields)) =
        Pretty.pretty ρ₀
    <>  " = "
    <>  Pretty.pretty k₀
    <>  " : "
    <>  Pretty.pretty τ₀
    <>  foldMap prettyAlternativeType kτs
    <>  " | "
    <>  case fields of
            Monotype.EmptyAlternatives       -> ""
            Monotype.UnsolvedAlternatives ρ₁ -> Pretty.pretty ρ₁ <> "?"
            Monotype.VariableAlternatives ρ₁ -> Pretty.pretty ρ₁
prettyEntry (Annotation x α) =
    Pretty.pretty x <> " : " <> Pretty.pretty α
prettyEntry (Marker α) =
    "➤" <> Pretty.pretty α <> " : Type"
prettyEntry (MarkerFields α) =
    "➤" <> Pretty.pretty α <> " : Fields"
prettyEntry (MarkerAlternatives α) =
    "➤" <> Pretty.pretty α <> " : Alternatives"

prettyFieldType :: (Text, Monotype) -> Doc a
prettyFieldType (k, τ) = ", " <> Pretty.pretty k <> " : " <> Pretty.pretty τ

prettyAlternativeType :: (Text, Monotype) -> Doc a
prettyAlternativeType (k, τ) =
    ", " <> Pretty.pretty k <> " : " <> Pretty.pretty τ

{-| Substitute a `Type` using the `Solved`, `SolvedFields`, and
    `SolvedAlternatives` entries of a `Context`

    >>> solve [ Unsolved 1, Solved 0 Monotype.Bool ] Type{ location = (), node = Type.Unsolved 0 }
    Type {location = (), node = Bool}
-}
solve :: Context s -> Type s -> Type s
solve context type_ = foldl snoc type_ context
  where
    snoc t (Solved             α τ) = Type.solve             α τ t
    snoc t (SolvedFields       α r) = Type.solveFields       α r t
    snoc t (SolvedAlternatives α r) = Type.solveAlternatives α r t
    snoc t  _                       = t

{-| Substitute a t`Type.Record` using the solved entries of a `Context`

    >>> solveRecord [ SolvedFields 0 (Monotype.Fields [] Monotype.EmptyFields) ] (Type.Fields [("a", Type{ location = (), node = Type.Bool })] (Monotype.UnsolvedFields 0))
    Fields [("a",Type {location = (), node = Bool})] EmptyFields
-}
solveRecord :: Context s -> Type.Record s -> Type.Record s
solveRecord context record = record'
  where
    -- TODO: Come up with total solution
    Type.Type{ node = Type.Record record' } =
        solve context Type.Type
            { location = error "Grace.Context.solveRecord: Internal error - Missing location field"
            , node = Type.Record record
            }

{-| Substitute a t`Type.Union` using the solved entries of a `Context`
    `Context`

    >>> solveUnion [ SolvedAlternatives 0 (Monotype.Alternatives [] Monotype.EmptyAlternatives) ] (Type.Alternatives [("a", Type{ location = (), node = Type.Bool })] (Monotype.UnsolvedAlternatives 0))
    Alternatives [("a",Type {location = (), node = Bool})] EmptyAlternatives
-}
solveUnion :: Context s -> Type.Union s -> Type.Union s
solveUnion context union = union'
  where
    -- TODO: Come up with total solution
    Type.Type{ node = Type.Union union' }=
        solve context Type.Type
            { location = error "Grace.Context.solveUnion: Internal error - Missing location field"
            , node = Type.Union union
            }

{-| This function is used at the end of the bidirectional type-checking
    algorithm to complete the inferred type by:

    * Substituting the type with all `Solved` / `SolvedFields` entries in the
      `Context`

    * Adding universal quantifiers for all `Unsolved` entries in the `Context`

    >>> complete [ Unsolved 1, Solved 0 Monotype.Bool ] Type{ location = (), node = Type.Function Type{ location = (), node = Type.Unsolved 1 } Type{ location = (), node = Type.Unsolved 0 } }
    Type {location = (), node = Forall () "a" Type (Type {location = (), node = Function (Type {location = (), node = Variable "a"}) (Type {location = (), node = Bool})})}
-}
complete :: Context s -> Type s -> Type s
complete context type_ = do
    State.evalState (Monad.foldM snoc type_ context) 0
  where
    snoc t (Solved             α τ) = do return (Type.solve             α τ t)
    snoc t (SolvedFields       α r) = do return (Type.solveFields       α r t)
    snoc t (SolvedAlternatives α r) = do return (Type.solveAlternatives α r t)
    snoc t (Unsolved α) = do
        n <- State.get

        State.put $! n + 1

        let a = Existential.toVariable n

        let node =
                Type.Forall (Type.location t) a Domain.Type (Type.solve α (Monotype.Variable a) t)

        let Type{ location } = t

        return Type.Type{..}
    snoc t (UnsolvedFields ρ) = do
        n <- State.get

        State.put $! n + 1

        let a = Existential.toVariable n

        let node =
                Type.Forall (Type.location t) a Domain.Fields (Type.solveFields ρ (Monotype.Fields [] (Monotype.VariableFields a)) t)

        let Type{ location } = t

        return Type.Type{..}
    snoc t (UnsolvedAlternatives ρ) = do
        n <- State.get

        State.put $! n + 1

        let a = Existential.toVariable n

        let node =
                Type.Forall (Type.location t) a Domain.Alternatives (Type.solveAlternatives ρ (Monotype.Alternatives [] (Monotype.VariableAlternatives a)) t)

        let Type{ location } = t

        return Type.Type{..}
    snoc t _ = do
        return t

{-| Split a `Context` into two `Context`s before and after the given
    `Unsolved` variable.  Neither `Context` contains the variable

    Returns `Nothing` if no such `Unsolved` variable is present within the
    `Context`

    >>> splitOnUnsolved 1 [ Unsolved 1, Solved 0 Monotype.Bool ]
    Just ([],[Solved 0 Bool])
    >>> splitOnUnsolved 0 [ Unsolved 1, Solved 0 Monotype.Bool ]
    Nothing
-}
splitOnUnsolved
    :: Existential Monotype
    -- ^ `Unsolved` variable to split on
    -> Context s
    -> Maybe (Context s, Context s)
splitOnUnsolved α₀ (Unsolved α₁ : entries)
    | α₀ == α₁ = return ([], entries)
splitOnUnsolved α (entry : entries) = do
    (prefix, suffix) <- splitOnUnsolved α entries
    return (entry : prefix, suffix)
splitOnUnsolved _ [] = Nothing

{-| Split a `Context` into two `Context`s before and after the given
    `UnsolvedFields` variable.  Neither `Context` contains the variable

    Returns `Nothing` if no such `UnsolvedFields` variable is present within the
    `Context`

    >>> splitOnUnsolvedFields 1 [ UnsolvedFields 1, Solved 0 Monotype.Bool ]
    Just ([],[Solved 0 Bool])
    >>> splitOnUnsolvedFields 0 [ UnsolvedFields 1, Solved 0 Monotype.Bool ]
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

    >>> splitOnUnsolvedAlternatives 1 [ UnsolvedAlternatives 1, Solved 0 Monotype.Bool ]
    Just ([],[Solved 0 Bool])
    >>> splitOnUnsolvedAlternatives 0 [ UnsolvedAlternatives 1, Solved 0 Monotype.Bool ]
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

    >>> lookup "x" 0 [ Annotation "x" Type{ location = (), node = Type.Bool }, Annotation "y" Type{ location = (), node = Type.Natural } ]
    Just (Type {location = (), node = Bool})
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

    >>> discardUpTo (Marker 1) [ Unsolved 1, Marker 1, Unsolved 0 ]
    [Unsolved 0]
-}
discardUpTo :: Eq s => Entry s -> Context s -> Context s
discardUpTo entry₀ (entry₁ : _Γ)
    | entry₀ == entry₁ = _Γ
    | otherwise = discardUpTo entry₀ _Γ
discardUpTo _ [] = []
