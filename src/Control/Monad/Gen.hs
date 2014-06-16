{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses                    #-}
{-# LANGUAGE UndecidableInstances, DeriveFunctor, FunctionalDependencies #-}
module Control.Monad.Gen
       ( GenT
       , Gen
       , MonadGen(..)
       , runGenT
       , runGen
       , runGenTInt
       , runGenInt) where
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Identity
import Control.Monad.Error
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List

-- | The monad transformer for generating fresh values.
newtype GenT e m a = GenT {unGenT :: StateT e m a}
                   deriving(Functor)
instance Monad m => Monad (GenT e m) where
  return = GenT . return
  (GenT m) >>= f = GenT $ m >>= unGenT . f
instance (Functor f, Monad f) => Applicative (GenT e f) where
  pure = GenT . pure
  (GenT f) <*> (GenT a) = GenT $ f <*> a

type Gen e = GenT e Identity

instance MonadTrans (GenT e) where
  lift = GenT . lift

instance MonadReader r m => MonadReader r (GenT e m) where
  local f = GenT . local f . unGenT
  ask     = GenT ask
instance MonadState s m => MonadState s (GenT e m) where
  get    = GenT $ lift get
  put    = GenT . lift . put
instance (MonadWriter w m) => MonadWriter w (GenT e m) where
  tell m = lift $ tell m
  listen = GenT . listen . unGenT
  pass   = GenT . pass . unGenT

-- | The MTL style class for generating fresh values
class Monad m => MonadGen e m | m -> e where
  -- | Generate a fresh value @e@, @gen@ should never produce the
  -- same value within a monadic computation.
  gen :: m e

instance (Monad m, Enum e) => MonadGen e (GenT e m) where
  gen = GenT $ modify succ >> get
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

runGenT :: Monad m => e -> GenT e m a -> m a
runGenT e = flip evalStateT e . unGenT

runGen :: e -> Gen e a -> a
runGen e = runIdentity . runGenT e

-- | A shortcut for the common case where we want fresh
-- `Integer`s.
runGenTInt :: Monad m => GenT Integer m a -> m a
runGenTInt = runGenT 0

runGenInt :: Gen Integer a -> a
runGenInt = runIdentity . runGenTInt 
