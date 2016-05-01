{-# LANGUAGE FlexibleContexts, TypeSynonymInstances #-}

module DynFocus where

import Control.Monad
import Control.Monad.State

-- just for the hell of it, here's a data type for a doubleton list
data Pair a = Pair a a

-- this is especially pointless, as PairT m a == Pair (m a)
data PairT m a = PairT {fstP :: m a, sndP :: m a}

instance Show (m a) => Show (PairT m a) where
  show (PairT x y) = show (x,y)

-- but at least it's a monad!
-- this is actually not 100% trivial, since the composition of two monads
-- m1 (m2 a) is not always itself a monad. but in this case it is
instance Monad m => Monad (PairT m) where
  return x = PairT (return x) (return x)
  PairT m n >>= k = PairT (m >>= fstP . k) (n >>= sndP . k)

instance MonadTrans PairT where
  lift m = PairT m m

instance MonadPlus m => MonadPlus (PairT m) where
  mzero = lift mzero
  (PairT m n) `mplus` (PairT m' n') = PairT (m `mplus` m') (n `mplus` n')
  

------------------------------------------------------------------------------

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

some :: MonadPlus m => (Ent -> m Bool) -> m Ent
some restr = filtM restr (take 3 domain)
  where filtM :: MonadPlus m => (a -> m Bool) -> [a] -> m a
        filtM _ [] = mzero
        filtM p (x:xs) = do flg <- p x
                            let hd = if flg then p x >> return x else mzero
                            hd `mplus` filtM p xs

foc :: Ent -> Focus Ent
foc x = lift $ PairT [x] [0..x]

johnF :: Focus Ent
johnF = foc john
  where john = 3


-- more pointlessness: TupleT m a = [m a]
data TupleT m a = TupleT {runTupleT :: [m a]}

instance Show (m a) => Show (TupleT m a) where
  show (TupleT xs) = show xs

-- this is only a monad when all tuples are the same size!!!
-- in which case, it's an n-adic generalization of PairT
instance MonadPlus m => Monad (TupleT m) where
  return x = TupleT [return x]
  TupleT xs >>= k = TupleT [m >>= k' i | (m,i) <- xs']
    where xs' = zip xs [0..]
          k' i x = runTupleT (k x) !! i

instance MonadTrans TupleT where
  lift m = TupleT [m]

instance MonadPlus m => MonadPlus (TupleT m) where
  mzero = TupleT [mzero]
  TupleT xs `mplus` TupleT ys = TupleT $ xs `mplus` ys


------------------------------------------------------------------------------

data PointedT m a = PointedT {point :: Maybe a, alts :: m a}

instance (Show a, Show (m a)) => Show (PointedT m a) where
  show (PointedT x m) = show (x, m)

instance Monad m => Monad (PointedT m) where
  return x = PointedT (return x) (return x)
  PointedT pt m >>= k = PointedT newpt (m >>= alts . k)
    where newpt = case pt of Nothing -> Nothing
                             Just x  -> point $ k x

instance MonadPlus m => MonadPlus (PointedT m) where
  mzero = PointedT mzero mzero
  (PointedT x m) `mplus` (PointedT y n) = PointedT (x `mplus` y) (m `mplus` n)

type DynFocus = StateT Env (PointedT [])

focD :: a -> DynFocus a
focD = lift . return

run :: StateT Env [] a -> [(a,Env)]
run = flip runStateT [1]

runD :: DynFocus a -> PointedT [] (a,Env)
runD = flip runStateT [1]
