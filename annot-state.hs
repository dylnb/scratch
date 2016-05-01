
import Control.Monad

-- a toy resumption monad, from
-- http://stackoverflow.com/questions/10236953/the-pause-monad

data AnnotatedComputation s a = AC { runAC :: s -> (Step s a, s) }

data Step s a = Value a | Break (AnnotatedComputation s a)

instance Functor (AnnotatedComputation s) where
  fmap = liftM

instance Applicative (AnnotatedComputation s) where
  pure x = AC $ \s -> (Value x, s)
  (<*>) = ap

instance Monad (AnnotatedComputation s) where
  return = pure
  m >>= k = AC $ \s -> case runAC m s of
    (Value a, s') -> runAC (k a) s'
    (Break m', s') -> (Break (m' >>= k), s')

get :: AnnotatedComputation s s
get = AC $ \s -> (Value s, s)

put :: s -> AnnotatedComputation s ()
put s = AC $ const (Value (), s)

checkpoint :: AnnotatedComputation s ()
checkpoint = AC $ \s -> (Break (return ()), s)

runAt :: (Show s, Show a) => s -> AnnotatedComputation s a -> IO (a,s)
runAt s m = case runAC m s of
  (Value x, s') -> return (x, s')
  (Break next, s') -> print s' >> runAt s' next

test :: String -> AnnotatedComputation Int String
test inp = do
  x <- get
  put (x + 1)
  checkpoint
  return (tail inp)

main :: IO ()
main = print =<< runAt 0 (test >=> test >=> test >=> test $ "test-string")
