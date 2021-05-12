newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
  fmap f = Arr2 . fmap (fmap f) . getArr2

instance Functor (Arr3 e1 e2 e3) where
  fmap f = Arr3 . fmap (fmap (fmap f)) . getArr3

instance Applicative (Arr2 e1 e2) where
  pure x = Arr2 (\e1 e2 -> x)
  (<*>) (Arr2 fa) (Arr2 fb) = Arr2 (\e1 e2 -> fa e1 e2 $ fb e1 e2)

instance Applicative (Arr3 e1 e2 e3) where
  pure x = Arr3 (\e1 e2 e3 -> x)
  (<*>) (Arr3 fa) (Arr3 fb) = Arr3 (\e1 e2 e3 -> fa e1 e2 e3 $ fb e1 e2 e3)
