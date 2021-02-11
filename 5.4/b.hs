type Board = Int

nextPositions :: Board -> [Board]
nextPositions b = [b .. b + 3]

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred
  | n < 0 = []
  | n == 0 = [b | pred b]
  | n > 0 = do
    p <- nextPositions b
    nextPositionsN p (n - 1) pred
