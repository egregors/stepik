-- GHCi> foldr (++) "!!" (Tr "ab" "cd" "efg")
-- "abcdefg!!"
-- GHCi> foldl (++) "!!" (Tr "ab" "cd" "efg")
-- "!!abcdefg"

data Triple a = Tr a a a  deriving (Eq,Show)

instance Foldable Triple where
    foldr f ini (Tr a b c) = f a $ f b $ f c ini
    foldl f ini (Tr a b c) = f (f (f ini a) b) c
