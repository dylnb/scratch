

-- a toy sort of resumption monad, from
-- http://stackoverflow.com/questions/10236953/the-pause-monad

data Snapped s a = Snapped { runSnapped :: s -> (Status s a, s) }

data Status s a
    = Done a
    | Snapshot (Snapped s a)

instance Monad (Snapped s) where
    return a = Snapped (\s -> (Done a, s))
    m >>= k = Snapped $ \s ->
        case runSnapped m s of
            (Done a, s') -> runSnapped (k a) s'
            (Snapshot m', s') -> (Snapshot (m' >>= k), s')

get :: Snapped s s
get = Snapped (\s -> (Done s, s))

put :: s -> Snapped s ()
put s = Snapped (const (Done (), s))

snap :: Snapped s ()
snap = Snapped (\s -> (Snapshot (return ()), s))

run :: (Show s, Show a) => s -> Snapped s a -> IO (a,s)
run s m =
    case runSnapped m s of
        (Done x, s') -> return (x, s')
        (Snapshot next, s') -> print s' >> run s' next

test :: Int -> Snapped Int Int
test n = do
    x <- get
    put (x * 2)
    snap
    return (n + 1)

main :: IO ()
main = run 1000 (test 0 >>= test >>= test >>= test) >>= print
