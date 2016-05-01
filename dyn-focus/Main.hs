{-# LANGUAGE FlexibleContexts, TypeSynonymInstances #-}

module Main where

-- some potential combined state and pointed set effects

import Control.Applicative
import Control.Monad
import Control.Monad.State


data PairT m a = PairT {fstP :: m a, sndP :: m a}

instance Show (m a) => Show (PairT m a) where
  show (PairT x y) = show (x,y)

instance Functor m => Functor (PairT m) where
  fmap f (PairT m n) = PairT (fmap f m) (fmap f n)

instance Applicative m => Applicative (PairT m) where
  pure x = PairT (pure x) (pure x)
  (PairT mf nf) <*> (PairT mx nx) = PairT (mf <*> mx) (nf <*> nx)

instance Monad m => Monad (PairT m) where
  return = pure
  PairT m n >>= k = PairT (m >>= fstP . k) (n >>= sndP . k)

instance MonadTrans PairT where
  lift m = PairT m m

instance MonadPlus m => Alternative (PairT m) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus m => MonadPlus (PairT m) where
  mzero = lift mzero
  (PairT m n) `mplus` (PairT m' n') = PairT (m `mplus` m') (n `mplus` n')
  
type Ent = Int
type Env = [Ent]
type Focus a = StateT Env (PairT []) a
               -- Env -> ([(a,Env)], [(a,Env)])

boy :: [Ent]
boy = [1,2,3]

aboy :: Focus Ent
aboy = lift $ lift boy

domain :: [Ent]
domain = [1..6]

foc :: Ent -> Focus Ent
foc x = lift $ PairT [x] [0..x]

johnF :: Focus Ent
johnF = foc john
  where john = 3


------------------------------------------------------------------------------

data PointedT m a = PointedT {point :: Maybe a, alts :: m a}

instance (Show a, Show (m a)) => Show (PointedT m a) where
  show (PointedT x m) = show (x, m)

instance Monad m => Functor (PointedT m) where
  fmap = liftM

instance Monad m => Applicative (PointedT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (PointedT m) where
  return x = PointedT (return x) (return x)
  PointedT pt m >>= k = PointedT newpt (m >>= alts . k)
    where newpt = case pt of
            Nothing -> Nothing -- uh oh, pointlessness persists! :(
            Just x  -> point $ k x

instance MonadPlus m => Alternative (PointedT m) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus m => MonadPlus (PointedT m) where
  mzero = PointedT mzero mzero
  (PointedT x m) `mplus` (PointedT y n) = PointedT (x `mplus` y) (m `mplus` n)
                                          -- first point overrides second :(

type DynFocus = StateT Env (PointedT [])

focD :: a -> DynFocus a
focD = lift . return

run :: StateT Env [] a -> [(a,Env)]
run = flip runStateT [1]

runD :: DynFocus a -> PointedT [] (a,Env)
runD = flip runStateT [1]


main :: IO ()
main = return ()
