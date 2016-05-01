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
-- instance Apply s t ret => Apply (K s) (K t) (K ret) where
instance (Apply s t ret, Monad m) => Apply (m s) (m t) (m ret) where
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

-- won't be needing this, but what the hell
(--@) :: M a -> (a -> M b) -> M b
(--@) = (>>=)
infixl 1 --@

run :: M Bool -> [(Bool,Stack)]
run m = runStateT m []

------------------------------------------------------------------------------

john' :: Ent
john' = Atom ("j",1)

left' :: Ent -> Bool
left' = (== john')

------------------------------------------------------------------------------

john :: K Ent
john = return john'

reallyjohn :: K (K Ent)
reallyjohn = return john

left :: K (Ent -> Bool)
left = return left'

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

evaljohnreallyleft :: [(Bool, Stack)]
evaljohnreallyleft = run $ lower johnreallyleft

------------------------------------------------------------------------------

johns :: [Ent]
johns = return john'

lefts :: [Ent -> Bool]
lefts = return left'

johnslefts :: [Bool]
johnslefts = johns `app` lefts

reallyjohns :: [[Ent]]
reallyjohns = [johns]

reallylefts :: [[Ent -> Bool]]
reallylefts = [lefts]

johnsreallylefts :: [[Bool]]
johnsreallylefts = reallyjohns `app` reallylefts

------------------------------------------------------------------------------

may :: [Bool] -> [Bool]
may = return . or

mayjohnslefts :: [Bool]
mayjohnslefts = may `app` johnslefts

mays :: [[Bool] -> [Bool]]
mays = return may

maysjohnslefts :: [[Bool]]
maysjohnslefts = mays `app` johnsreallylefts

-------------------------------------------------------------------------------


johnr :: Int -> Ent
johnr = return john'

leftr :: Int -> (Ent -> Bool)
leftr = return left'

johnleftr :: Int -> Bool
johnleftr = johnr `app` leftr

leftjohnr :: Int -> Bool
leftjohnr = leftr `app` johnr
