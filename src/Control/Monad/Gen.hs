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
import qualified Control.Monad.Trans.State as State
import Control.Monad.Gen.Class

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
