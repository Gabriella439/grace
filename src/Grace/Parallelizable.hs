-- | Computations with optional concurrency
module Grace.Parallelizable
    ( -- * Monad
      Parallelizable(..)
    , serialize
    ) where

import Control.Applicative (liftA2)
import Control.Concurrent.Async (Concurrently(..))
import Control.Exception.Safe (MonadCatch)
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Morph (MFunctor(..))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans (MonadTrans(..))

-- | The `Concurrent` segment (if any) of a `Parallelizable` computation
data Concurrent io a = Concurrent (Concurrently (Parallelizable io a)) | Pure a
    deriving stock (Functor)

instance MFunctor Concurrent where
    hoist nat (Concurrent concurrently) =
        Concurrent (fmap (hoist nat) concurrently)
    hoist _ (Pure a) =
        Pure a

{-| This represents a computation that alternates between `Serial` and
    `Concurrent` segments.  The `Applicative` instance will try to run
    `Concurrent` segments in parallel as much as possible.
-}
newtype Parallelizable io a = Serial{ serial :: io (Concurrent io a) }
    deriving stock (Functor)

-- | Serialize a `Parallelizable` computation
serialize :: MonadIO io => Parallelizable io a -> io a
serialize Serial{ serial } = do
    step <- serial

    case step of
        Pure a -> do
            return a

        Concurrent concurrently -> do
            parallelizable <- liftIO (runConcurrently concurrently)

            serialize parallelizable

instance Monad io => Applicative (Parallelizable io) where
    pure a = Serial{ serial = pure (Pure a) }

    Serial{ serial = serial₀ } <*> Serial{ serial = serial₁ } = Serial{ serial }
      where
        serial = do
            step₀ <- serial₀

            case step₀ of
                Pure f -> do
                    fmap (fmap f) serial₁
                Concurrent concurrently₀ -> do
                    step₁ <- serial₁

                    case step₁ of
                        Pure x -> do
                            return (Concurrent (fmap (fmap (\f -> f x)) concurrently₀))
                        Concurrent concurrently₁ -> do
                            let concurrently = do
                                    parallelizable₀ <- concurrently₀
                                    parallelizable₁ <- concurrently₁

                                    return (parallelizable₀ <*> parallelizable₁)

                            return (Concurrent concurrently)

instance Monad io => Monad (Parallelizable io) where
    Serial{ serial = serial₀ } >>= f =
        Serial{ serial = serial₁ }
      where
        serial₁ = do
            step <- serial₀

            case step of
                Pure x -> do
                    serial (f x)
                Concurrent concurrently -> do
                    return (Concurrent (fmap (>>= f) concurrently))

instance MonadTrans Parallelizable where
    lift m = Serial{ serial }
      where
        serial = do
            a <- m

            return (Pure a)

instance Monad io => MonadIO (Parallelizable io) where
    liftIO io = Serial{ serial }
      where
        serial = do
            pure (Concurrent (Concurrently (fmap pure io)))

instance (Monad io, Semigroup a) => Semigroup (Parallelizable io a) where
    (<>) = liftA2 (<>)

instance (Monad io, Monoid a) => Monoid (Parallelizable io a) where
    mempty = pure mempty

instance MFunctor Parallelizable where
    hoist nat Serial{ serial = serial₀ } =
        Serial{ serial = serial₁ }
      where
        serial₁ = nat (fmap (hoist nat) serial₀)

instance (MonadState s io) => MonadState s (Parallelizable io) where
    get = lift get

    put s = lift (put s)

    state f = lift (state f)

instance (MonadThrow io) => MonadThrow (Parallelizable io) where
    throwM e = lift (throwM e)

instance (MonadCatch io, MonadIO io) => MonadCatch (Parallelizable io) where
    catch m f = lift (catch (serialize m) (serialize . f))

instance (MonadReader r io) => MonadReader r (Parallelizable io) where
    ask = lift ask

    local f = hoist (local f)
