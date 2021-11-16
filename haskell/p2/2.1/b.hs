data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
newtype Postorder a = PostO (Tree a) deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)

--      3
--     / \
--    2   4
--     \
--      1

-- [1,2,3,4]
instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l

-- [3,1,2,4]
instance Foldable Preorder where
    foldr f ini (PreO Nil) = ini
    foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini r') l')
      where
        l' = PreO l
        r' = PreO r

-- [2,1,4,3]
instance Foldable Postorder where
    foldr f ini (PostO Nil) = ini
    foldr f ini (PostO (Branch l x r)) = foldr f (foldr f (f x ini) r') l'
      where
        l' = PostO l
        r' = PostO r

-- [3,1,4,2]
instance Foldable Levelorder where
    foldr f ini (LevelO tree) = g [tree]
      where
        g [] = ini
        g (Nil : xs) = g xs
        g ((Branch l x r) : xs) = f x (g (xs ++ [l, r]))

tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
tests =
    [ foldr (:) [] tree
    , foldr (:) [] $ PreO tree
    , foldr (:) [] $ PostO tree
    , foldr (:) [] $ LevelO tree
    ]