import Control.Monad

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = Log [msg] . f

fromLogger :: Log a -> (a, [String])
fromLogger (Log xs val) = (val, xs)

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog log f = Log (preMsg ++ [head msg]) val
  where
    (preVal, preMsg) = fromLogger log
    (val, msg) = fromLogger $ f preVal

data Log a = Log [String] a deriving (Show)

instance Functor Log where
  fmap = liftM

instance Applicative Log where
  pure = return
  (<*>) = ap

instance Monad Log where
  return = returnLog
  (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList = foldl (>>=) . return

-- playground

add1Log = toLogger (+ 1) "added one"

mult2Log = toLogger (* 2) "multiplied by 2"

t = execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]