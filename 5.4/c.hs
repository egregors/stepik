pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple n = do
  a <- [1 .. n]
  b <- [1 .. n]
  c <- [1 .. n]
  True <- return $ a ^ 2 + b ^ 2 == c ^ 2
  True <- return $ a < b
  return (a, b, c)