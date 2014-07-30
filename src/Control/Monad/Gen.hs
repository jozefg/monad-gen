{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances, DeriveFunctor      #-}
{-# LANGUAGE CPP #-}
module Control.Monad.Gen
       ( GenT
       , Gen
       , module Control.Monad.Gen.Class
       , runGenT
       , runGen
       , runGenTWith
       , runGenWith) where
#if MIN_VERSION_mtl(2, 2, 1)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Cont.Class
import Control.Monad.Reader.Class
import Control.Monad.State
import Control.Monad.Writer.Class
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
instance (Monad m, Functor m, MonadPlus m) => Alternative (GenT e m) where
  empty = mzero
  (<|>) = mplus

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
instance MonadFix m => MonadFix (GenT e m) where
  mfix = GenT . mfix . (unGenT .)
instance MonadPlus m =>  MonadPlus (GenT e m) where
  mzero = GenT mzero
  mplus (GenT m) (GenT m') = GenT $ mplus m m'
instance MonadIO m => MonadIO (GenT e m) where
  liftIO = GenT . liftIO
instance MonadCont m => MonadCont (GenT e m) where
  callCC f = GenT $ callCC (unGenT . f . (GenT .))

#if MIN_VERSION_mtl(2, 2, 1)
#else
instance MonadError e m => MonadError e (GenT e m) where
  throwError = GenT . throwError
  catchError m h = GenT $ catchError (unGenT m) (unGenT . h)
#endif

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
