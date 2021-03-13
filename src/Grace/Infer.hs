{-| This module implements the bidirectional type-checking algorithm from:

    Dunfield, Joshua, and Neelakantan R. Krishnaswami. \"Complete and easy bidirectional typechecking for higher-rank polymorphism.\" ACM SIGPLAN Notices 48.9 (2013): 429-442.

    The source code of this module strives to use the same variable-naming
    conventions as the original paper.  The source code also includes comments
    highlighting which subexpressions in the code correspond to which judgments
    from the paper.

    The main difference from the original algorithm that this module uses
    `Control.Monad.State.Strict.StateT` to thread around `Context`s and
    manipulate them instead of explicit `Context` passing as in the original
    paper.
-}
module Grace.Infer
    ( -- * Type inference
      typeOf

    , -- * Internal implementation
      Status(..)
    , wellFormedType
    , subtype
    , instantiateL
    , instantiateR
    , infer
    , check
    , inferApplication
    ) where

import Data.Text (Text)

import Control.Applicative ((<|>))
import Control.Monad.Except (MonadError(..))
import Control.Monad.State.Strict (MonadState)
import Data.Foldable (traverse_)
import Data.String.Interpolate (__i)
import Grace.Context (Context, Entry)
import Grace.Syntax (Syntax)
import Grace.Type (Type)
import Prettyprinter (Pretty)

import qualified Control.Monad.Except      as Except
import qualified Control.Monad.State       as State
import qualified Data.Map                  as Map
import qualified Data.Text                 as Text
import qualified Grace.Context             as Context
import qualified Grace.Monotype            as Monotype
import qualified Grace.Syntax              as Syntax
import qualified Grace.Type                as Type
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty.Text

-- | Type-checking state
data Status = Status
    { count :: !Int
      -- ^ Used to generate fresh unsolved variables (e.g. α̂, β̂)
    , context :: Context
      -- ^ The type-checking context (e.g. Γ, Δ, Θ)
    }
    deriving (Show)

orDie :: MonadError Text m => Maybe a -> Text -> m a
Just x  `orDie` _       = return x
Nothing `orDie` message = Except.throwError message

fresh :: MonadState Status m => m Int
fresh = do
    Status{ count = n, .. } <- State.get
    State.put $! Status{ count = n + 1, .. }
    return n

push :: MonadState Status m => Entry -> m ()
push entry = State.modify (\s -> s { context = entry : context s })

get :: MonadState Status m => m Context
get = fmap context State.get

set :: MonadState Status m => Context -> m ()
set context = State.modify (\s -> s{ context })

discardUpTo :: MonadState Status m => Entry -> m ()
discardUpTo entry =
    State.modify (\s -> s { context = Context.discardUpTo entry (context s) })

prettyToText :: Pretty a => a -> Text
prettyToText =
      Pretty.Text.renderStrict
    . Pretty.layoutPretty Pretty.defaultLayoutOptions 
    . Pretty.pretty

listToText :: Pretty a => [a] -> Text
listToText elements =
    Text.intercalate "\n" (map (\entry -> "• " <> prettyToText entry) elements)

{-| This corresponds to the judgment:

    > Γ ⊢ A

    … which checks that under context Γ, type A is well-formed
-}
wellFormedType :: MonadError Text m => Context -> Type -> m ()
-- UvarWF
wellFormedType _Γ (Type.Variable α)
    | Context.Variable α `elem` _Γ = return ()
    | otherwise                    = Except.throwError [__i|
Unbound type variable: #{α}
|]
-- ArrowWF
wellFormedType _Γ (Type.Function _A _B) = do
    wellFormedType _Γ _A
    wellFormedType _Γ _B
-- ForallWF
wellFormedType _Γ (Type.Forall α _A) = do
    wellFormedType (Context.Variable α : _Γ) _A
-- EvarWF / SolvedEvarWF
wellFormedType _Γ _A@(Type.Unsolved α₀)
    | any predicate _Γ = return ()
    | otherwise        = Except.throwError [__i|
Internal error: Invalid context

The following type:

↳ #{prettyToText _A}

… is not well-formed within the following context:

#{listToText _Γ}
|]
  where
    predicate (Context.Unsolved α₁) = α₀ == α₁
    predicate (Context.Solved α₁ _) = α₀ == α₁
    predicate  _                    = False
wellFormedType _Γ (Type.List _A) = do
    wellFormedType _Γ _A
wellFormedType _Γ (Type.Record kAs Nothing) = do
    traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
wellFormedType _Γ (Type.Record kAs (Just ρ₀))
    | any predicate _Γ = do
        traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
    | otherwise = do
        Except.throwError [__i|
Internal error: Invalid context

The following row type variable:

↳ #{prettyToText (Type.Unsolved ρ₀)}

… is not well-formed within the following context:

#{listToText _Γ}
|]
  where
    predicate (Context.Unsolved ρ₁     ) = ρ₀ == ρ₁
    predicate (Context.SolvedRow ρ₁ _ _) = ρ₀ == ρ₁
    predicate  _                         = False
wellFormedType _Γ Type.Bool = do
    return ()

{-| This corresponds to the judgment:

    > Γ ⊢ A <: B ⊣ Δ

    … which updates the context Γ to produce the new context Δ, given that the
    type A is a subtype of type B.
-}
subtype :: (MonadState Status m, MonadError Text m) => Type -> Type -> m ()
subtype _A₀ _B₀ = do
    _Γ <- get

    case (_A₀, _B₀) of
        -- <:Var
        (Type.Variable α₀, Type.Variable α₁)
            | α₀ == α₁ && Context.Variable α₀ `elem` _Γ -> do
                return ()
        -- <:Exvar
        (Type.Unsolved α₀, Type.Unsolved α₁)
            | α₀ == α₁ && Context.Unsolved α₀ `elem` _Γ -> do
                return ()
        -- InstantiateL
        (Type.Unsolved α, _A)
            | not (α `Type.freeIn` _A) && elem (Context.Unsolved α) _Γ -> do
                instantiateL α _A
        -- InstantiateR
        (_A, Type.Unsolved α)
            | not (α `Type.freeIn` _A) && elem (Context.Unsolved α) _Γ -> do
                instantiateR _A α
        -- <:→
        (Type.Function _A₁ _A₂, Type.Function _B₁ _B₂) -> do
            subtype _B₁ _A₁
            _Θ <- get
            subtype (Context.solve _Θ _A₂) (Context.solve _Θ _B₂)
        -- <:∀L
        (Type.Forall α₀ _A, _B) -> do
            α₁ <- fresh
            push (Context.Marker α₁)
            push (Context.Unsolved α₁)
            subtype (Type.substitute α₀ 0 (Type.Unsolved α₁) _A) _B
            discardUpTo (Context.Marker α₁)
        -- <:∀R
        (_A, Type.Forall α _B) -> do
            push (Context.Variable α)
            subtype _A _B
            discardUpTo (Context.Variable α)
        (Type.Bool, Type.Bool) -> do
            return ()
        (Type.List _A, Type.List _B) -> do
            subtype _A _B
        (Type.Record kAs₀ Nothing, Type.Record kBs₀ Nothing) -> do
            let mapA = Map.fromList kAs₀
            let mapB = Map.fromList kBs₀

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            let nullA = Map.null extraA
            let nullB = Map.null extraB

            if | not nullA && not nullB -> do
                Except.throwError [__i|

                The following record type:

                ↳ #{prettyToText _A₀}

                … is not a subtype of:

                ↳ #{prettyToText _B₀}

                The former record has the following extra fields:

                #{listToText (Map.keys extraA)}

                … while the latter record has the following extra fields:

                #{listToText (Map.keys extraB)}
                |]

               | not nullA && nullB -> do
                Except.throwError [__i|

                The following record type:

                ↳ #{prettyToText _A₀}

                … is not a subtype of:

                ↳ #{prettyToText _B₀}

                The former record has the following extra fields:

                #{listToText (Map.keys extraA)}
                |]

               | nullA && not nullB -> do
                Except.throwError [__i|

                The following record type:

                ↳ #{prettyToText _A₀}

                … is not a subtype of:

                ↳ #{prettyToText _B₀}

                The latter record has the following extra fields:

                #{listToText (Map.keys extraB)}
                |]

               | otherwise -> do
                let process _ (_A₁, _B₁) = do
                        _Θ <- get
                        subtype (Context.solve _Θ _A₁) (Context.solve _Θ _B₁)

                _ <- Map.traverseWithKey process both

                return ()
        (Type.Record kAs₀ (Just ρ), Type.Record kBs₀ Nothing) -> do
            let mapA = Map.fromList kAs₀
            let mapB = Map.fromList kBs₀

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            if | not (Map.null extraA) -> do
                Except.throwError [__i|

                The following record type:

                ↳ #{prettyToText _A₀}

                … is not a subtype of:

                ↳ #{prettyToText _B₀}

                The former record has the following extra fields:

                #{listToText (Map.keys extraA)}

                |]

               | otherwise -> do
                let process _ (_A₁, _B₁) = do
                        _Θ <- get
                        subtype (Context.solve _Θ _A₁) (Context.solve _Θ _B₁)

                _ <- Map.traverseWithKey process both

                instantiateRowL ρ (Map.toList extraB) Nothing

        (Type.Record kAs₀ Nothing, Type.Record kBs₀ (Just ρ)) -> do
            let mapA = Map.fromList kAs₀
            let mapB = Map.fromList kBs₀

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            if | not (Map.null extraB) -> do
                Except.throwError [__i|

                The following record type:

                ↳ #{prettyToText _A₀}

                … is not a subtype of:

                ↳ #{prettyToText _B₀}

                The latter record has the following extra fields:

                #{listToText (Map.keys extraB)}

                |]

               | otherwise -> do
                let process _ (_A₁, _B₁) = do
                        _Θ <- get
                        subtype (Context.solve _Θ _A₁) (Context.solve _Θ _B₁)

                _ <- Map.traverseWithKey process both

                instantiateRowR (Map.toList extraA) Nothing ρ

        (Type.Record kAs₀ (Just ρ₀), Type.Record kBs₀ (Just ρ₁)) -> do
            let mapA = Map.fromList kAs₀
            let mapB = Map.fromList kBs₀

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            let process _ (_A₁, _B₁) = do
                    _Θ <- get
                    subtype (Context.solve _Θ _A₁) (Context.solve _Θ _B₁)

            _ <- Map.traverseWithKey process both

            ρ₂ <- fresh

            let ρ₀First = do
                    (_ΓR, _ΓL) <- Context.split ρ₀ _Γ
                    _          <- Context.split ρ₁ _ΓL
                    return (set (_ΓR <> (Context.Unsolved ρ₀ : Context.Unsolved ρ₂ : _ΓL)))

            let ρ₁First = do
                    (_ΓR, _ΓL) <- Context.split ρ₁ _Γ
                    _          <- Context.split ρ₀ _ΓL
                    return (set (_ΓR <> (Context.Unsolved ρ₁ : Context.Unsolved ρ₂ : _ΓL)))

            case ρ₀First <|> ρ₁First of
                Nothing -> do
                    Except.throwError [__i|
                    Internal error: Invalid context

                    One of the following row type variables is missing from the context:

                    #{listToText [Type.Unsolved ρ₀, Type.Unsolved ρ₁ ]}
                    |]
                Just m -> do
                    m

            instantiateRowL ρ₀ (Map.toList extraB) (Just ρ₂)

            instantiateRowR (Map.toList extraA) (Just ρ₂) ρ₁

        (_A, _B) -> do
            Except.throwError [__i|
            Not a subtype
        
            The following type:
        
            ↳ #{prettyToText _A}
        
            … cannot be a subtype of:
        
            ↳ #{prettyToText _B}
            |]

{-| This corresponds to the judgment:

    > Γ ⊢ α̂ :≦ A ⊣ Δ

    … which updates the context Γ to produce the new context Δ, by instantiating
    α̂ such that α̂ <: A.
-}
instantiateL :: (MonadState Status m, MonadError Text m) => Int -> Type -> m ()
instantiateL α _A₀ = do
    _Γ₀ <- get

    let instLSolve _A τ = do
            (_Γ', _Γ) <- Context.split α _Γ₀ `orDie` "InstLSolve"
            wellFormedType _Γ _A
            set (_Γ' <> (Context.Solved α τ : _Γ))

    case _A₀ of
        -- InstLReach
        Type.Unsolved β
            | Just (_ΓR, _Γ₁) <- Context.split β _Γ₀
            , Just (_ΓM, _ΓL) <- Context.split α _Γ₁ -> do
                set (_ΓR <> (Context.Solved β (Monotype.Unsolved α) : _ΓM) <> (Context.Unsolved α : _ΓL))
        -- InstLSolve
        Type.Unsolved β -> do
            instLSolve (Type.Unsolved β) (Monotype.Unsolved β)
        Type.Variable β -> do
            instLSolve (Type.Variable β) (Monotype.Variable β)
        Type.Bool -> do
            instLSolve Type.Bool Monotype.Bool
        -- InstLArr
        Type.Function _A₁ _A₂ -> do
            (_ΓR, _ΓL) <- Context.split α _Γ₀ `orDie` "InstLArr"
            α₁ <- fresh
            α₂ <- fresh
            set (_ΓR <> (Context.Solved α (Monotype.Function (Monotype.Unsolved α₁) (Monotype.Unsolved α₂)) : Context.Unsolved α₁ : Context.Unsolved α₂ : _ΓL))
            instantiateR _A₁ α₁
            _Θ <- get
            instantiateL α₂ (Context.solve _Θ _A₂)
        -- InstLAllR
        Type.Forall β _B
            | Context.Unsolved α `elem` _Γ₀ -> do
                push (Context.Variable β)
                instantiateL  α _B
                discardUpTo (Context.Variable β)
            | otherwise -> do
                Except.throwError [__i|
                Internal error: Malformed context

                The following unsolved variable:

                ↳ #{prettyToText (Context.Unsolved α)}}

                … cannot be instantiated because the variable is missing from the context:

                #{listToText _Γ₀}
                |]
        Type.List _A -> do
            (_ΓR, _ΓL) <- Context.split α _Γ₀ `orDie` "InstLList"
            α₁ <- fresh
            set (_ΓR <> (Context.Solved α (Monotype.List (Monotype.Unsolved α₁)) : Context.Unsolved α₁ : _ΓL))
            instantiateL α₁ _A
        Type.Record kAs ρ -> do
            (_ΓR, _ΓL) <- Context.split α _Γ₀ `orDie` "InstLRecord"
            let process (k, _A) = do
                    β <- fresh
                    return (k, _A, β)
            kAβs <- traverse process kAs
            let βs = map (\(_, _, β) -> Context.Unsolved β) kAβs
            let kβs = map (\(k, _, β) -> (k, Monotype.Unsolved β)) kAβs
            set (_ΓR <> (Context.Solved α (Monotype.Record kβs ρ) : βs <> _ΓL))
            let instantiate (_, _A, β) = do
                    _Θ <- get
                    instantiateL β (Context.solve _Θ _A)
            traverse_ instantiate kAβs

{-| This corresponds to the judgment:

    > Γ ⊢ A ≦: α̂ ⊣ Δ

    … which updates the context Γ to produce the new context Δ, by instantiating
    α̂ such that A :< α̂.
-}
instantiateR :: (MonadState Status m, MonadError Text m) => Type -> Int -> m ()
instantiateR _A₀ α = do
    _Γ₀ <- get

    let instRSolve _A τ = do
            (_Γ', _Γ) <- Context.split α _Γ₀ `orDie` "InstRSolve"
            wellFormedType _Γ _A
            set (_Γ' <> (Context.Solved α τ : _Γ))

    case _A₀ of
        -- InstRReach
        Type.Unsolved β
            | Just (_ΓR, _Γ₁) <- Context.split β _Γ₀
            , Just (_ΓM, _ΓL) <- Context.split α _Γ₁ -> do
                set (_ΓR <> (Context.Solved β (Monotype.Unsolved α) : _ΓM) <> (Context.Unsolved α : _ΓL))
        -- InstRSolve
        Type.Unsolved β -> do
            instRSolve (Type.Unsolved β) (Monotype.Unsolved β)
        Type.Variable β -> do
            instRSolve (Type.Variable β) (Monotype.Variable β)
        Type.Bool -> do
            instRSolve Type.Bool Monotype.Bool
        -- InstRArr
        Type.Function _A₁ _A₂ -> do
            (_ΓR, _ΓL) <- Context.split α _Γ₀ `orDie` "InstRArr"
            α₁ <- fresh
            α₂ <- fresh
            set (_ΓR <> (Context.Solved α (Monotype.Function (Monotype.Unsolved α₁) (Monotype.Unsolved α₂)) : Context.Unsolved α₁ : Context.Unsolved α₂ : _ΓL))
            instantiateL α₁ _A₁
            _Θ <- get
            instantiateR (Context.solve _Θ _A₂) α₂
        -- InstRAllL
        Type.Forall β₀ _B
            | Context.Unsolved α `elem` _Γ₀ -> do
                β₁ <- fresh
                push (Context.Marker β₁)
                push (Context.Unsolved β₁)
                instantiateR (Type.substitute β₀ 0 (Type.Unsolved β₁) _B) α
                discardUpTo (Context.Marker β₁)
            | otherwise -> do
                Except.throwError [__i|
                Internal error: Malformed context
        
                The following unsolved variable:
        
                ↳ #{prettyToText (Context.Unsolved α)}}
        
                … cannot be instantiated because the variable is missing from the context:
        
                #{listToText _Γ₀}
                |]
        Type.List _A -> do
            (_ΓR, _ΓL) <- Context.split α _Γ₀ `orDie` "InstRArr"
            α₁ <- fresh
            set (_ΓR <> (Context.Solved α (Monotype.List (Monotype.Unsolved α₁)) : Context.Unsolved α₁ : _ΓL))
            instantiateR _A α₁
        Type.Record kAs ρ -> do
            (_ΓR, _ΓL) <- Context.split α _Γ₀ `orDie` "InstRRecord"
            let process (k, _A) = do
                    β <- fresh
                    return (k, _A, β)
            kAβs <- traverse process kAs
            let βs = map (\(_, _A, β) -> Context.Unsolved β) kAβs
            let kβs = map (\(k, _, β) -> (k, Monotype.Unsolved β)) kAβs
            set (_ΓR <> (Context.Solved α (Monotype.Record kβs ρ) : βs <> _ΓL))
            let instantiate (_, _A, β) = do
                    _Θ <- get
                    instantiateR (Context.solve _Θ _A) β
            traverse_ instantiate kAβs

instantiateRowL
    :: (MonadState Status m, MonadError Text m)
    => Int -> [(Text, Type)] -> Maybe Int -> m ()
instantiateRowL ρ₀ kAs ρ₁ = do
    _Γ <- get

    (_ΓR, _ΓL) <- Context.split ρ₀ _Γ `orDie` "instantiateRowL"

    let process (k, _A) = do
            β <- fresh

            return (k, _A, β)

    kAβs <- traverse process kAs

    let instantiate (_, _A, β) = do
            _Θ <- get

            instantiateL β (Context.solve _Θ _A)

    traverse_ instantiate kAβs

    let βs  = map (\(_, _, β) -> Context.Unsolved β      ) kAβs
    let kβs = map (\(k, _, β) -> (k, Monotype.Unsolved β)) kAβs

    set (_ΓR <> (Context.SolvedRow ρ₀ kβs ρ₁ : βs <> _ΓL))

instantiateRowR
    :: (MonadState Status m, MonadError Text m)
    => [(Text, Type)] -> Maybe Int -> Int -> m ()
instantiateRowR kAs ρ₁ ρ₀ = do
    _Γ <- get

    (_ΓR, _ΓL) <- Context.split ρ₀ _Γ `orDie` "instantiateRowR"

    let process (k, _A) = do
            β <- fresh

            return (k, _A, β)

    kAβs <- traverse process kAs

    let instantiate (_, _A, β) = do
            _Θ <- get

            instantiateR (Context.solve _Θ _A) β

    traverse_ instantiate kAβs

    let βs  = map (\(_, _, β) -> Context.Unsolved β      ) kAβs
    let kβs = map (\(k, _, β) -> (k, Monotype.Unsolved β)) kAβs

    set (_ΓR <> (Context.SolvedRow ρ₀ kβs ρ₁ : βs <> _ΓL))

{-| This corresponds to the judgment:

    > Γ ⊢ e ⇒ A ⊣ Δ

    … which infers the type of e under input context Γ, producing an inferred
    type of A and an updated context Δ.
-}
infer :: (MonadState Status m, MonadError Text m) => Syntax -> m Type
-- Var
infer (Syntax.Variable x₀ n) = do
    _Γ <- get
    case Context.lookup x₀ n _Γ of
        Just _A -> do
            return _A
        Nothing -> do
            Except.throwError [__i|
            Unbound variable: ${x₀ <> if n == 0 then "" else "@" <> Text.pack (show n)}
            |]
infer (Syntax.Lambda x e) = do
    α <- fresh
    β <- fresh
    push (Context.Unsolved α)
    push (Context.Unsolved β)
    push (Context.Annotation x (Type.Unsolved α))
    check e (Type.Unsolved β)
    discardUpTo (Context.Annotation x (Type.Unsolved α))
    return (Type.Function (Type.Unsolved α) (Type.Unsolved β))
-- →E
infer (Syntax.Application e₁ e₂) = do
    _A <- infer e₁
    _Θ <- get
    inferApplication (Context.solve _Θ _A) e₂
-- Anno
infer (Syntax.Annotation e _A) = do
    _Γ <- get
    wellFormedType _Γ _A
    check e _A
    return _A
-- let
infer (Syntax.Let x Nothing a b) = do
    _A <- infer a
    push (Context.Annotation x _A)
    infer b
infer (Syntax.Let x (Just _A) a b) = do
    check a _A
    push (Context.Annotation x _A)
    infer b
infer (Syntax.List xs) = do
    case xs of
        [] -> do
            α <- fresh
            push (Context.Unsolved α)
            return (Type.List (Type.Unsolved α))
        y : ys -> do
            _A <- infer y
            traverse_ (`check` _A) ys
            return (Type.List _A)
infer (Syntax.Record kvs) = do
    let process (k, v) = do
            _A <- infer v
            return (k, _A)
    kAs <- traverse process kvs
    return (Type.Record kAs Nothing)
infer (Syntax.Field record key) = do
    α <- fresh
    ρ <- fresh
    push (Context.Unsolved α)
    push (Context.Unsolved ρ)
    check record (Type.Record [(key, Type.Unsolved α)] (Just ρ))
    return (Type.Unsolved α)
infer (Syntax.If predicate l r) = do
    check predicate Type.Bool
    _L₀ <- infer l
    _Γ  <- get
    let _L₁ = Context.solve _Γ _L₀
    check r _L₁
    return _L₁
infer (Syntax.And l r) = do
    check l Type.Bool
    check r Type.Bool
    return Type.Bool
infer (Syntax.Or l r) = do
    check l Type.Bool
    check r Type.Bool
    return Type.Bool
infer Syntax.True = do
    return Type.Bool
infer Syntax.False = do
    return Type.Bool

{-| This corresponds to the judgment:

    > Γ ⊢ e ⇐ A ⊣ Δ

    … which checks that e has type A under input context Γ, producing an updated
    context Δ.
-}
check :: (MonadState Status m, MonadError Text m) => Syntax -> Type -> m ()
-- →I
check (Syntax.Lambda x e) (Type.Function _A _B) = do
    push (Context.Annotation x _A)
    check e _B
    discardUpTo (Context.Annotation x _A)
-- ∀I
check e (Type.Forall α _A) = do
    push (Context.Variable α)
    check e _A
    discardUpTo (Context.Variable α)
-- Sub
check e _B = do
    _A <- infer e
    _Θ <- get
    subtype (Context.solve _Θ _A) (Context.solve _Θ _B)

{-| This corresponds to the judgment:

    > Γ ⊢ A • e ⇒⇒ C ⊣ Δ

    … which infers the result type C when a function of type A is applied to an
    input argument e, under input context Γ, producing an updated context Δ.
-}
inferApplication
    :: (MonadState Status m, MonadError Text m) => Type -> Syntax -> m Type
-- ∀App
inferApplication (Type.Forall α₀ _A) e = do
    α₁ <- fresh
    push (Context.Unsolved α₁)
    inferApplication (Type.substitute α₀ 0 (Type.Unsolved α₁) _A) e
-- αApp
inferApplication (Type.Unsolved α) e = do
    _Γ <- get
    (_ΓR, _ΓL) <- Context.split α _Γ `orDie` "αApp"
    α₁ <- fresh
    α₂ <- fresh
    set (_ΓR <> (Context.Solved α (Monotype.Function (Monotype.Unsolved α₁) (Monotype.Unsolved α₂)) : Context.Unsolved α₁ : Context.Unsolved α₂ : _ΓL))
    check e (Type.Unsolved α₁)
    return (Type.Unsolved α₂)
inferApplication (Type.Function _A _C) e = do
    check e _A
    return _C
inferApplication (Type.Variable α) _ = do
    Except.throwError [__i|
Internal error: Unexpected type variable in function type

The following type variable:

↳ #{α}

… should have been replaced with an unsolved variable when type-checking a
function application.
|]
inferApplication _A _ = do
    Except.throwError [__i|
Not a function type

An expression of the following type:

↳ #{prettyToText _A}

… was invoked as if it were a function, but the above type is not a function
type.
|]

-- | Infer the `Type` of the given `Syntax` tree
typeOf :: Syntax -> Either Text Type
typeOf syntax = do
    let initialStatus = Status{ count = 0, context = [] }
    (_A, Status{ context = _Δ }) <- State.runStateT (infer syntax) initialStatus
    return (Context.complete _Δ _A)
