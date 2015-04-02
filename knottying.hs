data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show (Branch left right) = "[ " ++ show left ++ show right ++ "] "
  show (Leaf x) = show x ++ " "


trace :: (a -> b -> (c, b)) -> a -> c
trace f input = let (output, x) = f input x
                in output

repmin :: Ord a => Tree a -> Tree a
repmin = trace repIImin

repIImin :: Ord a => Tree a -> a -> (Tree a, a)
repIImin (Leaf val) rep = (Leaf rep, val)
repIImin (Branch left right) rep = let (left' , l) = repIImin left  rep
                                       (right', r) = repIImin right rep
                                   in (Branch left' right', min l r)


t :: Tree Int
t = Branch (Branch (Leaf 4) (Leaf 2)) (Branch (Branch (Leaf 1) (Leaf 3)) (Leaf 6))
-- t =   ---------*---------
--       |                 |
--    ---*---           ---*---
--    |     |          |      |
--    4     2       ---*---   6
--                  |     |
--                  1     3

t' = repmin t
-- t' =   ---------*---------
--        |                 |
--     ---*---           ---*---
--     |     |          |      |
--     1     1       ---*---   1
--                   |     |
--                   1     1



fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

fibs10 :: [Int]
fibs10 = take 10 fibs
