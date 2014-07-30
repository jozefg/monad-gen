{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses                    #-}
{-# LANGUAGE UndecidableInstances, DeriveFunctor, FunctionalDependencies #-}
module Control.Monad.Gen
       ( GenT
       , Gen
       , MonadGen(..)
       , runGenT
       , runGen
       , runGenTWith
       , runGenWith) where
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


-- | Run a @GenT@ computation starting from the value
-- @toEnum 0@
runGenT :: (Enum e, Monad m) => GenT e m a -> m a
runGenT = runGenTWith (toEnum 0)

-- | Run a @Gen@ computation starting from the value
-- @toEnum 0@
runGen :: Enum e => Gen e a -> a
runGen = runGenWith (toEnum 0)

-- | Run a @GenT@ computation starting from a specific value @e@.
runGenTWith :: Monad m => e -> GenT e m a -> m a
runGenTWith e = flip evalStateT e . unGenT

-- | Run a @Gen@ computation starting from a specific value @e@.
runGenWith :: e -> Gen e a -> a
runGenWith e = runIdentity . runGenTWith e
