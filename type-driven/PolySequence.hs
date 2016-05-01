{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

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
-- instance (Apply s t ret, Applicative m) => Apply (m s) (m t) (m ret) where
  -- app = liftM2 app

-- class Lower m where
--   lower :: m -> M Bool
-- instance Lower Bool where
--   lower = return
-- instance Lower a => Lower (K a) where
--   lower m = runCont m lower

data Expr ret where
  Merge :: (Applicative m, Apply f g ret)
        => Expr (m f) -> Expr (m g) -> Expr (m ret)
  Lex :: ret -> Expr ret

-- instance Functor Expr where
--   fmap f (Lex v) = Lex (f v)
--   fmap f (Merge e1 e2) = Merge (fmap f e1) (fmap f e2)

-- instance Foldable Expr where
--   foldMap f (Lex v) = f v
--   foldMap f (Merge e1 e2) = foldMap e1 `mappend` foldMap e2

-- instance Traversable Expr where
--   sequenceA (Lex v) = Lex <$> v
--   sequenceA (Merge e1 e2) = Merge <$> e1 <*> e2

------------------------------------------------------------------------------

{--

run :: M Bool -> [(Bool,Stack)]
run m = runStateT m []

john' :: Ent
john' = Atom ("j",1)

left' :: Ent -> Bool
left' = (== john')


john :: K Ent
john = return john'

left :: K (Ent -> Bool)
left = return left'

------------------------------------------------------------------------------

{--
eval :: Applicative m => Expr (m a) -> m a
eval (Lex v) = v
eval (Merge e1 e2) = eval e1 `app` eval e2
--}

{--}
johnleft :: Expr (K Bool)
johnleft = Merge (Lex john) (Lex left)

{--}
leftjohn :: K Bool
leftjohn = left `app` john

evaljohnleft :: [(Bool, Stack)]
evaljohnleft = run $ lower johnleft

--}
