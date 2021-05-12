data Tree a = Leaf a | Node (Tree a) (Tree a)

avg :: Tree Int -> Int
avg t =
  let (c, s) = go t
   in s `div` c
  where
    go :: Tree Int -> (Int, Int)
    go (Leaf n) = (1, n)
    go (Node a b) = (fst goA + fst goB, snd goA + snd goB)
      where
        goA = go a
        goB = go b
