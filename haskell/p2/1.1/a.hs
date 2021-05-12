newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}
newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

instance Functor (Arr2 e1 e2) where
  fmap f = Arr2 . fmap (fmap f) . getArr2

instance Functor (Arr3 e1 e2 e3) where
  fmap f = Arr3 . fmap (fmap (fmap f)) . getArr3


t1 = getArr2 (fmap length (Arr2 take)) 10 "abc"
t2 = getArr3 (tail <$> tail <$> Arr3 zipWith) (+) [1,2,3,4] [10,20,30,40,50]