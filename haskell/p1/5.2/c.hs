data Log a = Log [String] a deriving (Show)

fromLogger :: Log a -> (a, [String])
fromLogger (Log xs val) = (val, xs)

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog log f = Log (preMsg ++ [head msg]) val
  where
    (preVal, preMsg) = fromLogger log
    (val, msg) = fromLogger $ f preVal
