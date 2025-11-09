{-| This module contains the `Grace` `Monad` shared by type inference and
    evaluation
-}
module Grace.Monad
    ( -- * Monad
      Status(..)
    , Grace(..)
    , runGrace
    , evalGrace
    , execGrace
    ) where

import Control.Monad.Catch (MonadThrow(..), MonadCatch(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (MonadState, StateT)
import Grace.Context (Context)
import Grace.Input (Input)
import Grace.Location (Location)
import Grace.Parallelizable (Parallelizable)

import qualified Control.Monad.State as State
import qualified Grace.Parallelizable as Parallelizable

-- | Interpretation state
data Status = Status
    { count :: !Int
      -- ^ Used to generate fresh unsolved variables (e.g. α̂, β̂ from the
      --   original paper)

    , context :: Context Location
      -- ^ The type-checking context (e.g. Γ, Δ, Θ)

    , input :: Input
      -- ^ The parent import, used to resolve relative imports
    }

-- | The shared `Monad` threaded throughout all phases of interpretation
newtype Grace a = Grace{ parallelizable :: Parallelizable (StateT Status IO) a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadCatch
        , MonadIO
        , MonadState Status
        , MonadThrow
        )

-- | Run the `Grace` `Monad`, preserving the result and final `Status`
runGrace :: MonadIO io => Status -> Grace a -> io (a, Status)
runGrace status Grace{ parallelizable } =
    liftIO (State.runStateT (Parallelizable.serialize parallelizable) status)

-- | Run the `Grace` `Monad`, discarding the final `Status`
evalGrace :: MonadIO io => Status -> Grace a -> io a
evalGrace status Grace{ parallelizable } =
    liftIO (State.evalStateT (Parallelizable.serialize parallelizable) status)

-- | Run the `Grace` `Monad`, discarding the result
execGrace :: MonadIO io => Status -> Grace a -> io Status
execGrace status Grace{ parallelizable } =
    liftIO (State.execStateT (Parallelizable.serialize parallelizable) status)
