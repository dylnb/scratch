{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

-------------------------------------------------------------------------------------------
-- https://hackage.haskell.org/package/indexed-extras-0.1.1
-------------------------------------------------------------------------------------------
module IxCont where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Indexed
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Indexed.Trans

newtype IxContT m r o a = IxContT { runIxContT :: (a -> m o) -> m r }

ilower :: Monad m => IxContT m r a a -> m r 
ilower m = runIxContT m return

instance IxFunctor (IxContT m) where
  imap f m = IxContT $ \c -> runIxContT m (c . f)

instance IxPointed (IxContT m) where
  ireturn a = IxContT ($a)

instance Monad m => IxApplicative (IxContT m) where
  iap = iapIxMonad

instance Monad m => IxMonad (IxContT m) where
  ibind f c = IxContT $ \k -> runIxContT c $ \a -> runIxContT (f a) k

instance Monad m => Functor (IxContT m i j) where
  fmap = imap

instance Monad m => Applicative (IxContT m i i) where
  pure = ireturn
  (<*>) = iap

instance Monad m => Monad (IxContT m i i) where
  return = ireturn
  m >>= k = ibind k m

instance IxMonadTrans IxContT where
  ilift m = IxContT (m >>=)

instance MonadReader e m => MonadReader e (IxContT m i i) where
  ask = ilift ask
  local f m = IxContT $ \c -> do
    r <- ask
    local f (runIxContT m (local (const r) . c))

instance MonadState e m => MonadState e (IxContT m i i) where
  get = ilift get
  put = ilift . put

instance MonadIO m => MonadIO (IxContT m i i) where
  liftIO = ilift . liftIO 

type IxCont = IxContT Identity

runIxCont :: IxCont r o a -> (a -> o) -> r 
runIxCont m k = runIdentity $ runIxContT m (return . k)

ixcont :: ((a -> o) -> r) -> IxCont r o a
ixcont m = IxContT $ \k -> Identity $ m (runIdentity . k)

iliftM :: IxMonad m => (t -> b) -> m i k t -> m i k b
iliftM f m = ireturn f `iap` m

iliftM2 :: IxMonad m => (t -> t1 -> b) -> m i j t -> m j k t1 -> m i k b
iliftM2 f m1 m2 = ireturn f `iap` m1 `iap` m2

iguard :: IxMonadZero m => Bool -> m i i ()
iguard b = if b then ireturn () else imzero
