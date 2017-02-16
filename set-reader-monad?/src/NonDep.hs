module NonDep where

import Control.Monad.Random
import System.IO.Unsafe

type Env = Int
data NonDep a = ND {unND :: Rand StdGen (Env -> a)}

instance Monad NonDep where
  return x = ND $ uniform [const x]
  m >>= k = ND $ do
    alpha <- unND m
    return $ \r -> unsafePerformIO . evalRandIO $ unND (k (alpha r)) <*> return r
-- [\r -> f (k . alpha $ r) r | alpha <- m, f <- cfs]

instance Functor NonDep where
  fmap f m = m >>= return . f

instance Applicative NonDep where
  pure = return
  mf <*> mx = mf >>= \f -> mx >>= \x -> return (f x)

alts :: [a] -> NonDep a
alts xs = ND $ uniform $ map const xs
