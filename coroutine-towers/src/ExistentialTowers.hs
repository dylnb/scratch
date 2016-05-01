{-# LANGUAGE ExistentialQuantification #-}

module ExistentialTowers where

import Control.Monad.Identity


------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

type E = Int
type T = Bool
type ET = E -> T
type EET = E -> ET

main :: IO ()
main = return ()

{--}
data Tower r o a = Tower { runT :: (a -> o) -> r }

fm :: (a1 -> a) -> Tower r o a1 -> Tower r o a
fm f t = Tower $ \k -> runT t (k . f)

app :: Tower i j (a -> b) -> Tower j k a -> Tower i k b
m `app` n = m `bind` \f -> n `bind` \x -> eta (f x)

data Co s a = forall i j. Co (Tower i j (Either (s (Co s a)) a))

bind :: Tower i j a -> (a -> Tower j k b) -> Tower i k b
m `bind` f = Tower $ \k -> runT m (\x -> runT (f x) k)

eta :: a -> Tower i i a
eta x = Tower $ \k -> k x

{--}
instance Functor s => Functor (Co s) where
   fmap f (Co t) = Co (fm (apply f) t)
      where apply fc (Right x) = Right (fc x)
            apply fc (Left s) = Left (fmap (fmap fc) s)
  


------------------------------------------------------------------------------
-- Grammar
------------------------------------------------------------------------------

{--
(</>) :: Functor s => Co s (a -> b) -> Co s a -> Co s b
(Co m) </> (Co n) = Co $
  m `bind` \e -> case e of
    Right f -> case fmap f (Co n) of {Co t -> t}
    Left s  -> eta $ Left (fmap ((Co n) <\>) s)

(<\>) :: Functor s => Co s a -> Co s (a -> b) -> Co s b
(Co m) <\> (Co n) = Co $ 
  m `bind` \e -> case e of
    Right x -> case fmap ($x) (Co n) of {Co t -> t}
    Left s  -> eta $ Left (fmap ((Co n) </>) s)


--}
