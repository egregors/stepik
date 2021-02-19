import Control.Monad.State

fibStep :: State (Integer, Integer) ()
fibStep = do
  (a, b) <- get
  put (b, a + b)
  return ()

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState $ replicateM n m

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)