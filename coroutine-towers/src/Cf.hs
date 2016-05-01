module Cf where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Identity
import Control.Monad.Cont
import Control.Monad.Extra (anyM, allM)
import Data.List.Extra (subsequences, elemIndex)
import Data.Maybe (fromJust)


------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

type E = Int
type T = Bool
type ET = E -> T
type EET = E -> ET

type Trampoline r a = Coroutine Identity (ContT r IO) a

------------------------------------------------------------------------------
-- Grammar
------------------------------------------------------------------------------

{--}
(</>) :: (Functor s, Monad m) => Coroutine s m (a -> b) -> Coroutine s m a -> Coroutine s m b
m </> n = Coroutine $
  resume m >>= \e -> case e of
    Right f -> resume $ fmap f n
    Left s  -> return $ Left (fmap (n <\>) s)

(<\>) :: (Functor s, Monad m) => Coroutine s m a -> Coroutine s m (a -> b) -> Coroutine s m b
m <\> n = Coroutine $ 
  resume m >>= \e -> case e of
    Right x -> resume $ fmap ($x) n
    Left s  -> return $ Left (fmap (n </>) s)

lower :: Monad m => ContT a m a -> m a
lower = flip runContT return

runSent :: Monad m => Coroutine Identity m a -> m a
runSent = pogoStick runIdentity

evalSent :: Monad m => Coroutine Identity (ContT a m) a -> m a
evalSent = lower . runSent

------------------------------------------------------------------------------
-- Domain
------------------------------------------------------------------------------

{--}
dom :: [E]
dom = [1..3]

------------------------------------------------------------------------------
-- Language
------------------------------------------------------------------------------

{--}
one, two, three :: Trampoline T E
[one, two, three] = map return dom

{--}
delay :: (Monad s, Monad m) => Coroutine s m a -> Coroutine s m a
delay = suspend . return

{--}
smiled, cried, body :: (Functor s, Monad m) => Coroutine s m ET
smiled = return (< 3)
cried = return even
body = return (const True)

likes, hates :: (Functor s, Monad m) => Coroutine s m EET
likes = return (==)
hates = return (\y x -> x <= y)

sm_cf :: Trampoline T (ET -> E)
sm_cf = lift $ ContT $ \k -> anyM k cfs

ev_cf :: Trampoline T (ET -> E)
ev_cf = lift $ ContT $ \k -> allM k cfs


-- powerset of domain, minus empty set
powerdom = tail $ subsequences dom
-- all possible ways of choosing one of three elements for 2^3 - 1 subsets
cfIndices = sequence $ take (2^(length dom) - 1) $ repeat [1..length dom]
choose cf p = nth (ix - 1) ps
  where nth n xs = xs !! (min n $ length xs - 1) -- safe selection function
        ix = cf !! (fromJust $ ps `elemIndex` powerdom) -- get choice ix from cf for this prop
        ps = filter p dom -- characteristic set
cfs = map choose cfIndices


every = \k -> all k cfs
some = \k -> any k cfs
person = const True

t1 = every (\f -> some (\g -> f (<= g person) == 1))
t2 = every (\f -> some (\g -> f (== g person) == 1))
t3 = every (\f -> 2 <= f person)

{--}
smbody, evbody :: Trampoline T E
smbody = sm_cf </> body
evbody = ev_cf </> body

{--}
smSm, smEv, evSm, smEvInv :: Trampoline T T
smSm = smbody <\> (likes </> smbody)
smEv = smbody <\> (likes </> evbody)
evSm = evbody <\> (likes </> smbody)
smEvInv = delay smbody <\> (likes </> evbody)

lnkSm, lnkEv, lnkTest, lnkInv :: Trampoline T T
lnkSm = (sm_cf </> (hates </> three)) <\> smiled
lnkEv = (ev_cf </> (hates </> three)) <\> smiled
lnkTest = (ev_cf </> (hates </> smbody)) <\> (likes </> one)
lnkInv = (sm_cf </> (hates </> evbody)) <\> (likes </> one)

test = Coroutine $ do
  Right x <- resume evbody
  resume $ sm_cf </> (hates </> return x) <\> (likes </> one)

{--}
main :: IO ()
main = forM_ sents $ evalSent >=> print
  where sents = [smSm, smEv, evSm, smEvInv]

