
import Control.Monad (liftM2, join)

class Comonad w where
  counit :: w a -> a
  cobind :: (w a -> b) -> w a -> w b

data Cat = E | T | Cat :\: Cat | Cat :/: Cat deriving (Show, Eq)
data Tree a = Leaf a | Branch a (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show = unlines . pp
    where pp (Leaf x) = [show x]
          pp (Branch x left right) = show x : pp_subtree left right
          pp_subtree left right =
             pad "+- " "|  " (pp right) ++ pad "`- " "   " (pp left)
          pad first rest = zipWith (++) (first : repeat rest)

instance Comonad Tree where
  counit (Leaf x) = x
  counit (Branch x _ _) = x

  cobind f z = case z of
    Leaf _ -> Leaf (f z)
    Branch _ t1 t2 -> Branch (f z) (cobind f t1) (cobind f t2)

cat :: Tree Cat -> Maybe Cat
cat (Leaf c) = Just c
cat (Branch _ t1 t2) = join $ liftM2 apply (cat t1) (cat t2)
  where apply (f :/: x) y | x == y  = Just f
        apply y (x :\: f) | x == y  = Just f 
        apply _ _                   = Nothing                    

leaf :: a -> Tree a
leaf = Leaf
branch :: Tree a -> Tree a -> Tree a
branch = Branch undefined

gq, ent, vrb, hfvrb, goodTree, goodHFTree, badTree :: Tree Cat
gq  = leaf (T:/:(E:\:T))
ent = leaf E
vrb = leaf ((E:\:T):/:E)
hfvrb = leaf (E:\:(E:\:T))
goodTree = branch gq (branch vrb ent)
goodHFTree = branch gq (branch ent hfvrb)
badTree  = branch ent (branch vrb gq) 

testGood, testGoodHF, testBad :: Tree (Maybe Cat)
testGood   = cobind cat goodHFTree
testGoodHF = cobind cat goodTree
testBad    = cobind cat badTree

main :: IO ()
main = print testGood >> print testGoodHF >> print testBad
