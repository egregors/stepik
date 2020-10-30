import qualified Data.List as L
import Prelude hiding (lookup)

class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k, v)] -> m k v
  fromList [] = empty
  fromList ((k, v) : xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap {getListMap :: [(k, v)]}
  deriving (Eq, Show)

instance MapLike ListMap where
  empty = ListMap []
  lookup _ (ListMap []) = Nothing
  lookup key (ListMap m) = L.lookup key m
  insert key value m = ListMap $ (key, value) : (getListMap $ delete key m)
  delete key (ListMap m) = ListMap $ filter (\(k, _) -> (k /= key)) m
