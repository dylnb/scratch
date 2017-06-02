module Main where

import Control.Monad.Free
import Control.Comonad.Store
import Data.Foldable (traverse_)

data Ent = John | Mary deriving (Eq, Show, Ord)

type Foc a = Free (Store Ent) a

run :: Foc a -> a
run (Pure x) = x
run (Free m) = let (g,x) = runStore m in run (g x)

foc :: Ent -> Foc Ent
foc x = liftF $ store id x

johnF, maryF :: Foc Ent
johnF = foc John
maryF = foc Mary

left :: Ent -> Bool
left = (== John)

saw :: Ent -> Ent -> Bool
saw Mary John = True
saw _    _    = False

thinks :: Bool -> Ent -> Bool
thinks True John = True
thinks _    _    = False

johnF_left :: Foc Bool
johnF_left = johnF >>= \x -> return $ left x

maryF_left :: Foc Bool
maryF_left = maryF >>= \x -> return $ left x

johnF_saw_maryF :: Foc Bool
johnF_saw_maryF = johnF >>= \x -> maryF >>= \y -> return $ saw y x

maryF_saw_johnF :: Foc Bool
maryF_saw_johnF = johnF >>= \x -> maryF >>= \y -> return $ saw x y


john_thinks_johnF_left :: Foc Bool
john_thinks_johnF_left = johnF_left >>= \p -> return $ thinks p John

john_thinks_maryF_left :: Foc Bool
john_thinks_maryF_left = maryF_left >>= \p -> return $ thinks p John


type MultiArg a = Free ((->) Ent) a

depth :: MultiArg a -> Int
depth (Pure x) = 0
depth (Free g) = 1 + depth (g John)

data KMart a = KMart {fg :: [Ent], bg :: MultiArg a}

instance Show a => Show (KMart a) where
  show (KMart xs g) = show (xs, show (depth g) ++ "-place fun")

separate :: Foc a -> KMart a
separate (Pure x) =
  KMart [] (Pure x)
separate (Free m) =
  let (g,x) = runStore m
      KMart xs _ = separate (g x)
   in KMart (x:xs) (wrap $ \x' -> bg (separate (g x')))

emulcify :: KMart a -> Foc a
emulcify (KMart (x:xs) g) =
  wrap $ store (\x' -> emulcify (KMart xs (apply x' g))) x

apply :: Ent -> MultiArg a -> MultiArg a
apply _ (Pure a) = Pure a
apply x (Free g) = g x

main :: IO ()
main = traverse_ disp
  [ johnF_left
  , maryF_left
  , johnF_saw_maryF
  , maryF_saw_johnF
  , john_thinks_johnF_left
  , john_thinks_maryF_left
  ]
    where disp :: Show a => Foc a -> IO ()
          disp m = putStrLn $ (show $ separate m) ++ ": " ++ (show $ run m)
