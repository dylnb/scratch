module Main where

import NonDep
import Control.Monad.Random
import Data.List (nub)

main :: IO ()
main = do
  sequence (replicate 20 left) >>= \xs -> putStrLn ("LEFT: " ++ show (nub [x 0 | x <- xs]))
  sequence (replicate 20 right) >>= \xs -> putStrLn ("RIGHT: " ++ show (nub [x 0 | x <- xs]))

m :: NonDep Int
m = return 10

f,g :: Int -> NonDep Int
f = \x -> alts [x-1, x-2]
g = \x -> alts [x-5, x-7]

left, right :: IO (Env -> Int)
left  = evalRandIO . unND $ (m >>= f) >>= g
right = evalRandIO . unND $ m >>= (f >=> g)

