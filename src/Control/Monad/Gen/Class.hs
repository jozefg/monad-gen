module Control.Monad.Gen.Class where
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Instances
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer

-- | The MTL style class for generating fresh values
class Monad m => MonadGen e m | m -> e where
  -- | Generate a fresh value @e@, @gen@ should never produce the
  -- same value within a monadic computation.
  gen :: m e

instance MonadGen e m => MonadGen e (IdentityT m) where
  gen = lift gen
instance MonadGen e m => MonadGen e (StateT s m) where
  gen = lift gen
instance MonadGen e m => MonadGen e (ReaderT s m)  where
  gen = lift gen
instance (MonadGen e m, Monoid s) => MonadGen e (WriterT s m)  where
  gen = lift gen
instance MonadGen e m => MonadGen e (ListT m) where
  gen = lift gen
instance MonadGen e m => MonadGen e (MaybeT m) where
  gen = lift gen
instance (MonadGen e m, Error err) => MonadGen e (ErrorT err m) where
  gen = lift gen
