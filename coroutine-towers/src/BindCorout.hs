module BindCorout where

import IxCont
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Identity
import Control.Monad.Cont
import Control.Monad.Extra (anyM, allM)


------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

type E = Int
type T = Bool
type ET = E -> T
type EET = E -> ET

type Trampoline r a = Coroutine Identity (ContT r IO) a
-- type PushPull a = EitherFunctor ((->) (Maybe a)) ((,) a)
-- type Transducer r x = Coroutine (PushPull E) (ContT r IO) x


------------------------------------------------------------------------------
-- Domain
------------------------------------------------------------------------------

{--}
dom :: [E]
dom = [1..5]

{--}
push :: Monad m => Coroutine Identity m a -> Coroutine (Yield a) m a
push co = Coroutine $ resume co >>= \e -> case e of
  Right x             -> resume $ yield x >> return x
  Left (Identity co') -> resume $ push co'

-- awaitT :: Monad m => Transducer m (Maybe E)
-- awaitT = suspend (LeftF return)

-- yieldT :: Monad m => E -> Transducer m ()
-- yieldT x = suspend (RightF (x , return ()))

abody :: Coroutine (Yield E) IO ()
abody = concatYields (yield dom)

left :: Coroutine (Await E) IO T
left = await >>= \x -> return (x <= 3)

sent :: Coroutine Identity IO (T, ())
sent = weave sequentialBinder (weaveAwaitYield 6) left abody 

{--}
he :: Coroutine (Await E) (ContT T IO) E
he = await

