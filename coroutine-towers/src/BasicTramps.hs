{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module BasicTramps where

import Control.Monad.Cont
import Control.Monad.Coroutine
import Control.Monad.Identity
import Control.Monad.Extra (anyM, allM)


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

{--}
class Monad m => Lower m a where
  lower :: a -> m Bool
instance Monad m => Lower m Bool where
  lower = return
instance Lower m a => Lower m (ContT Bool m a) where
  lower m = runContT m lower

{--}
runSent :: Monad m => Coroutine Identity m a -> m a 
runSent = pogoStick runIdentity

{--}
evalSent :: (Monad m, Lower m a) => Coroutine Identity (ContT Bool m) a -> m Bool
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
smbody, evbody :: Trampoline T E
smbody = lift $ ContT $ \k -> or <$> mapM k dom
evbody = lift $ ContT $ \k -> and <$> mapM k dom

{--}
delay :: (Monad s, Monad m) => Coroutine s m a -> Coroutine s m a
delay = suspend . return

{--}
smiled, cried :: (Functor s, Monad m) => Coroutine s m ET
smiled = return (<= 3)
cried = return (> 0)

likes, hates :: (Functor s, Monad m) => Coroutine s m EET
likes = return (==)
hates = return (/=)

{--}
sm', ev' :: ContT T IO ET -> ContT T IO E
sm' p = ContT $ \k -> filterM rstr dom >>= anyM k
  where rstr x = lower $ p <*> return x

ev' p = ContT $ \k -> filterM rstr dom >>= allM k
  where rstr x = lower $ p <*> return x

{--}
sm, ev :: Trampoline T ET -> Trampoline T E
sm p = lift $ sm' (runSent p) 
ev p = lift $ ev' (runSent p)

{--}
smSm, smEv, evSm, smEvInv :: Trampoline T T
smSm = smbody <\> likes </> smbody
smEv = smbody <\> likes </> evbody
evSm = evbody <\> likes </> smbody
smEvInv = delay smbody <\> likes </> evbody

sad_lf = Coroutine $ do 
  Right x <- resume evbody
  resume $ sm (hates </> return x) <\> smiled

{--
hap_lf = Coroutine $ do
  case resume evbody of
    Right x -> blah
    Left f -> if something f
                 then let this = that
                       in one thing before another
--}

{--}
main :: IO ()
main = forM_ sents $ evalSent >=> print
  where sents = [smSm, smEv, evSm, smEvInv, sad_lf]


--}
