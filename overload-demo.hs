-- Here we overload an operator called 'unit' to send values of any type a to
-- monadic values of type m a, for any monad m. But we only define two
-- instances of the general 'unit' function, one for lists (which just embeds
-- x in a singleton) and one for functions on some fixed domain r (which just
-- returns the constant function to x)
class Monad m => Monadic m where
  unit :: a -> m a
instance Monadic [] where
  unit x = [x]
instance Monadic ((->) r) where
  unit x = \g -> x

-- push1 prepends 1 to a list
push1 :: [Int] -> [Int]
push1 ys = 1:ys

-- eval1 passes the string "1" into a function
eval1 :: (String -> Int) -> Int
eval1 m = m "1"

-- mlist is the list [2,3,4]
mlist :: [Int]
mlist = [2,3,4]

-- mfunc reads in a string representation of a number, then adds it to 4
mfunc :: String -> Int
mfunc = \g -> (extract g) + 4
  where extract g = (read g :: Int)

{-
-- these do what you'd expect
push1 mlist == [1,2,3,4]
eval1 mfunc == 5

-- if you query the type of 'unit 4', you get something that's still
-- polymorphic: type m Int, for some Monadic type m; effectively something
-- that's either of type [Int] or of type String -> Int
:t unit 4

-- then if you pass 'unit 4' into one of the non-polymorphic types, it forces
-- 'unit 4' to assume one of its two possible return types, which in turn
-- determines one of its two possible meanings
push1 (unit 4) == [1,4]
eval1 (unit 4) == 4
-}
