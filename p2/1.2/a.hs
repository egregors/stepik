import Control.Applicative (ZipList(ZipList), getZipList)

(>$<) :: (a -> b) ->  [a] -> [b]
(>$<) f xs = getZipList $ f <$> ZipList xs

(>*<) :: [a -> b] -> [a] -> [b]
(>*<) fs xs = getZipList $ ZipList fs <*> ZipList xs