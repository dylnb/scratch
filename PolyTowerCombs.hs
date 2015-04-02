{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

module PolyTowerCombs where

import Control.Monad.State
import Control.Monad.Cont

------------------------------------------------------------------------------

data Ent = Atom (String,Int)

instance Show Ent where
  show (Atom (x,y)) = x ++ show y

instance Eq Ent where
  (Atom (x,tag)) == (Atom (x',tag')) = x == x' && tag == tag'


type Stack = [Ent]
type M     = StateT Stack []
type K     = Cont (M Bool)

------------------------------------------------------------------------------

class Apply f g ret | f g -> ret where
  app :: f -> g -> ret
instance Apply (a -> b) a b where
  f `app` x = f x
instance Apply a (a -> b) b where
  x `app` f = f x
instance Apply s t ret => Apply (K s) (K t) (K ret) where
  app = liftM2 app

class Lower m where
  lower :: m -> M Bool
instance Lower Bool where
  lower = unit
instance Lower a => Lower (K a) where
  lower m = runCont m lower

------------------------------------------------------------------------------

unit :: a -> M a
unit = return

(--@) :: M a -> (a -> M b) -> M b
(--@) = (>>=)
infixl 1 --@

run :: M Bool -> [(Bool,Stack)]
run m = runStateT m []

------------------------------------------------------------------------------

j :: Ent
j = Atom ("j",1)

------------------------------------------------------------------------------

john :: K Ent
john = return j

reallyjohn :: K (K Ent)
reallyjohn = return john

left :: K (Ent -> Bool)
left = return (== j)

reallyleft :: K (K (Ent -> Bool))
reallyleft = return left

------------------------------------------------------------------------------

johnleft :: K Bool
johnleft = john `app` left

leftjohn :: K Bool
leftjohn = left `app` john

evaljohnleft :: [(Bool, Stack)]
evaljohnleft = run $ lower johnleft

------------------------------------------------------------------------------

johnreallyleft :: K (K Bool)
johnreallyleft = reallyjohn `app` reallyleft

leftreallyjohn :: K (K Bool)
leftreallyjohn = reallyleft `app` reallyjohn

evaljohnreallyleft ::  [(Bool, Stack)]
evaljohnreallyleft = run $ lower johnreallyleft

