{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module ValueReturns where

import Control.Monad.Cont
import Control.Monad.Identity
import Control.Monad.Coroutine
import Control.Monad.Indexed.Free
import IxCont
import Control.Monad.Indexed

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

type E = Int
type T = Bool
type ET = E -> T
type EET = E -> ET

data Val = E E | T T | C (Co E)
fromT (T b) = b
fromE (E x) = x
fromC (C co) = co

instance Show Val where
  show (E x) = show x
  show (T b) = show b
  show (C co) = "coroutine"

{--}
type Co a = Coroutine Identity (Cont Val) a

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

{--}
class Lower a where
  lower :: a -> Val
instance Lower T where
  lower = T
instance Lower a => Lower (Cont Val a) where
  lower = flip runCont lower

{--}
runSent :: Monad m => Coroutine Identity m a -> m a 
runSent = pogoStick runIdentity

{--}
evalSent :: Lower a => Co a -> Val
evalSent = lower . runSent

------------------------------------------------------------------------------
-- Domain
------------------------------------------------------------------------------

{--}
dom :: [E]
dom = [1..5]

------------------------------------------------------------------------------
-- Language
------------------------------------------------------------------------------

{--}
one, two, three, four, five :: Co E
[one, two, three, four, five] = map return dom

{--}
delay :: (Monad s, Monad m) => Coroutine s m a -> Coroutine s m a
delay = suspend . return

{--}
smiled, cried, body :: (Functor s, Monad m) => Coroutine s m ET
smiled = return (<= 3)
cried = return even
body = return (const True)

likes, hates :: (Functor s, Monad m) => Coroutine s m EET
likes = return (==)
hates = return (\y x -> x <= y)

sm, ev :: Co E
sm = lift $ cont $ \p ->
  let restr = filter (fromT . p) dom
  in  C (lift $ cont $ \k -> T $ any (fromT . k) restr)

ev = lift $ cont $ \p ->
  let restr = filter (fromT . p) dom
  in  C (lift $ cont $ \k -> T $ all (fromT . k) restr)

smbody = clower $ sm <\> body
evbody = clower $ ev <\> body

clower :: Co T -> Co E
clower = fromC . evalSent

cclower :: Co T -> Co E
cclower co = Coroutine $ resume co >>= \(Left (Identity m)) -> resume (clower m)

{--}
smSm, smEv, evSm, smEvInv :: Co T
smSm = smbody <\> (likes </> smbody)
smEv = smbody <\> (likes </> evbody)
evSm = evbody <\> (likes </> smbody)
smEvInv = delay smbody <\> (likes </> evbody)

lnkSrf, lnkInv :: Co T
lnkSrf = clower (sm <\> (hates </> evbody)) <\> (likes </> one)
lnkInv = cclower (delay sm <\> (hates </> evbody)) <\> (likes </> one)

-- sad_lf = Coroutine $ do 
--   Right x <- resume evbody
--   resume $ sm (hates </> return x) <\> smiled

{--}
main :: IO ()
main = forM_ sents $ print . fromT . evalSent
  where sents = [smSm, smEv, evSm, smEvInv]

--}
