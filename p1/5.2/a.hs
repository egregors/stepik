data Log a = Log [String] a deriving (Show)

fromLogger :: Log a -> (a, String)
fromLogger (Log [xs] val) = (val, xs)

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = Log [msg] . f

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers val f g = Log [fMsg, gMsg] res
  where
    (fRes, fMsg) = fromLogger $ f val
    (res, gMsg) = fromLogger $ g fRes
