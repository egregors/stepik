data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node (Leaf _) b) = 1 + height b
height (Node a (Leaf _)) = 1 + height a
height (Node a b) = height a + height b
-- what i should to do here???

size :: Tree a -> Int
size (Leaf _) = 1
size (Node a b) = 1 + size a + size b

test0 = height (Leaf 1) == 0

test1 = height (Node (Leaf 1) (Leaf 1)) == 1

test2 = height (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1)) == 2

test3 = height (Node (Node (Leaf 1) (Leaf 1)) (Node (Leaf 1) (Leaf 1))) == 2

test4 = height (Node (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1)) (Node (Leaf 1) (Leaf 1))) == 3

test5 = height (Node (Node (Node (Leaf 1) (Leaf 1)) (Node (Leaf 1) (Leaf 1))) (Node (Leaf 1) (Leaf 1))) == 3

test6 = height (Node (Node (Node (Leaf 1) (Leaf 1)) (Node (Leaf 1) (Leaf 1))) (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1))) == 3

test7 = height (Node (Node (Node (Leaf 1) (Leaf 1)) (Node (Leaf 1) (Leaf 1))) (Node (Node (Leaf 1) (Leaf 1)) (Node (Leaf 1) (Leaf 1)))) == 3

test8 = height (Node (Node (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1)) (Node (Leaf 1) (Leaf 1))) (Node (Node (Leaf 1) (Leaf 1)) (Node (Leaf 1) (Leaf 1)))) == 4

tests = [test0, test1, test2, test3, test4, test5, test6, test7, test8]