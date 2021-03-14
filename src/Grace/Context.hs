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
    , splitOnUnsolvedRow
    , discardUpTo
    , solve
    , solveRecord
    , complete
    ) where

import Data.Text (Text)
import Grace.Existential (Existential)
import Grace.Monotype (Monotype)
import Grace.Type (Type)
import Prelude hiding (lookup)
import Prettyprinter (Doc, Pretty(..))

import qualified Control.Monad              as Monad
import qualified Control.Monad.State.Strict as State
import qualified Grace.Existential          as Existential
import qualified Grace.Monotype             as Monotype
import qualified Grace.Type                 as Type
import qualified Prettyprinter              as Pretty

-- | An element of the `Context` list
data Entry
    = Variable Text
    -- ^ Type variable
    --
    -- >>> pretty (Variable "a")
    -- a
    | Annotation Text Type
    -- ^ A bound variable whose type is known
    --
    -- >>> pretty (Annotation "x" "a")
    -- x : a
    | Unsolved (Existential Monotype)
    -- ^ A placeholder type variable whose type has not yet been inferred
    --
    -- >>> pretty (Unsolved 0)
    -- a?
    | UnsolvedRow (Existential Monotype.Record)
    -- ^ A placeholder row variable whose type has not yet been inferred
    --
    -- >>> pretty (UnsolvedRow 0)
    -- |a?
    | Solved (Existential Monotype) Monotype
    -- ^ A placeholder type variable whose type has been (at least partially)
    --   inferred
    --
    -- >>> pretty (Solved 0 Monotype.Bool)
    -- a = Bool
    | SolvedRow (Existential Monotype.Record) Monotype.Record
    -- ^ A placeholder row variable whose type has been (at least partially)
    --   inferred
    --
    -- >>>  pretty (SolvedRow 0 (Monotype.Fields [("x", "X")] (Just 1)))
    -- a = x : X | b
    | Marker (Existential Monotype)
    -- ^ This is used by the bidirectional type-checking algorithm to separate
    --   context entries introduced before and after type-checking a universally
    --   quantified type
    --
    -- >>> pretty (Marker 0)
    -- ➤a
    deriving (Eq, Show)

instance Pretty Entry where
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
type Context = [Entry]

prettyEntry :: Entry -> Doc a
prettyEntry (Variable α) =
    Pretty.pretty α
prettyEntry (Unsolved α) =
    Pretty.pretty α <> "?"
prettyEntry (UnsolvedRow ρ) =
    "|" <> Pretty.pretty ρ <> "?"
prettyEntry (Solved α τ) =
    Pretty.pretty α <> " = " <> Pretty.pretty τ
prettyEntry (SolvedRow ρ (Monotype.Fields [] Nothing)) =
    Pretty.pretty ρ <> " = •"
prettyEntry (SolvedRow ρ₀ (Monotype.Fields [] (Just ρ₁))) =
    Pretty.pretty ρ₀ <> " = • | " <>  Pretty.pretty ρ₁
prettyEntry (SolvedRow ρ (Monotype.Fields ((k₀, τ₀) : kτs) Nothing)) =
        Pretty.pretty ρ
    <>  " = "
    <>  Pretty.pretty k₀
    <>  " : "
    <>  Pretty.pretty τ₀
    <>  foldMap prettyKeyType kτs
prettyEntry (SolvedRow ρ₀ (Monotype.Fields ((k₀, τ₀) : kτs) (Just ρ₁))) =
        Pretty.pretty ρ₀
    <>  " = "
    <>  Pretty.pretty k₀
    <>  " : "
    <>  Pretty.pretty τ₀
    <>  foldMap prettyKeyType kτs
    <>  " | "
    <>  Pretty.pretty ρ₁
prettyEntry (Annotation x α) =
    Pretty.pretty x <> " : " <> Pretty.pretty α
prettyEntry (Marker α) =
    "➤" <> Pretty.pretty α

prettyKeyType :: (Text, Monotype) -> Doc a
prettyKeyType (k, τ) = ", " <> Pretty.pretty k <> " : " <> Pretty.pretty τ

{-| Substitute a `Type` using the `Solved` and `SolvedRow` entries of a
    `Context`

    >>> solve [ Unsolved 1, Solved 0 Monotype.Bool ] (Type.Function (Type.Unsolved 0) (Type.Unsolved 0))
    Function Bool Bool
-}
solve :: Context -> Type -> Type
solve context type_ = foldl snoc type_ context
  where
    snoc t (Solved    α τ) = Type.solve    α τ t
    snoc t (SolvedRow α r) = Type.solveRow α r t
    snoc t  _              = t

{-| Substitute a t`Type.Record` using the `Solved` and `SolvedRow` entries of a
    `Context`

    >>> solve [ SolvedRow 0 (Monotype.Fields [] Nothing) ] (Type.Record (Type.Fields [("a", Type.Bool)] (Just 0)))
    Record (Fields [("a",Bool)] Nothing)
-}
solveRecord :: Context -> Type.Record -> Type.Record
solveRecord context record = record'
  where
    -- TODO: Come up with total solution
    Type.Record record' = solve context (Type.Record record)

{-| This function is used at the end of the bidirectional type-checking
    algorithm to complete the inferred type by:

    * Substituting the type with all `Solved` / `SolvedRow` entries in the
      `Context`

    * Adding universal quantifiers for all `Unsolved` entries in the `Context`

    >>> complete [ Unsolved 1, Solved 0 Monotype.Bool ] (Type.Function (Type.Unsolved 1) (Type.Unsolved 0))
    Forall "a" (Function (Variable "a") Bool)
-}
complete :: Context -> Type -> Type
complete context type_ = do
    State.evalState (Monad.foldM snoc type_ context) 0
  where
    snoc t (Solved    α τ) = do return (Type.solve    α τ t)
    snoc t (SolvedRow α r) = do return (Type.solveRow α r t)
    snoc t (Unsolved α) = do
        n <- State.get

        State.put $! n + 1

        let a = Existential.toVariable n

        return (Type.Forall a (Type.solve α (Monotype.Variable a) t))
-- TODO: Allow for user-visible row polymorphism so that this can be implemented
{-
    snoc t (UnsolvedRow ρ) = do
        n <- State.get

        State.put $! n + 1

        let a = Existential.toVariable n

        return (Type.Forall a (Type.solveRow ρ (Monotype.Fields [] (Just a)) t))
-}
    snoc t  _           = do
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
    -> Context
    -> Maybe (Context, Context)
splitOnUnsolved α₀ (Unsolved α₁ : entries)
    | α₀ == α₁ = return ([], entries)
splitOnUnsolved α (entry : entries) = do
    (prefix, suffix) <- splitOnUnsolved α entries
    return (entry : prefix, suffix)
splitOnUnsolved _ [] = Nothing

{-| Split a `Context` into two `Context`s before and after the given
    `UnsolvedRow` variable.  Neither `Context` contains the variable

    Returns `Nothing` if no such `UnsolvedRow` variable is present within the
    `Context`

    >>> splitOnUnsolvedRow 1 [ UnsolvedRow 1, Solved 0 Monotype.Bool ]
    Just ([],[Solved 0 Bool])
    >>> splitOnUnsolvedRow 0 [ UnsolvedRow 1, Solved 0 Monotype.Bool ]
    Nothing
-}
splitOnUnsolvedRow
    :: Existential Monotype.Record
    -- ^ `UnsolvedRow` variable to split on
    -> Context
    -> Maybe (Context, Context)
splitOnUnsolvedRow ρ₀ (UnsolvedRow ρ₁ : entries)
    | ρ₀ == ρ₁ = return ([], entries)
splitOnUnsolvedRow ρ (entry : entries) = do
    (prefix, suffix) <- splitOnUnsolvedRow ρ entries
    return (entry : prefix, suffix)
splitOnUnsolvedRow _ [] = Nothing

{-| Retrieve a variable's annotated type from a `Context`, given the variable's
    label and index

    >>> lookup "x" 0 [ Annotation "x" Type.Bool, Annotation "y" (Type.Function Type.Bool Type.Bool) ]
    Just Bool
-}
lookup
    :: Text
    -- ^ Variable label
    -> Int
    -- ^ Variable index (See the documentation of `Value.Variable`)
    -> Context
    -> Maybe Type
lookup _ _ [] =
    Nothing
lookup x₀ n (Annotation x₁ _A : _Γ) =
    if x₀ == x₁
    then
       if n <= 0
       then Just _A
       else lookup x₀ (n - 1) _Γ
    else lookup x₀ n _Γ
lookup x₀ n (Variable x₁ : _Γ) =
    if x₀ == x₁
    then
        if n <= 0
        then Nothing
        else lookup x₀ (n - 1) _Γ
    else lookup x₀ n _Γ
lookup x n (_ : _Γ) =
    lookup x n _Γ

{-| Discard all entries from a `Context` up to and including the given `Entry`

    >>> discardUpTo (Marker 1) [ Unsolved 1, Marker 1, Unsolved 0 ]
    [Unsolved 0]
-}
discardUpTo :: Entry -> Context -> Context
discardUpTo entry₀ (entry₁ : _Γ)
    | entry₀ == entry₁ = _Γ
    | otherwise = discardUpTo entry₀ _Γ
discardUpTo _ [] = []
