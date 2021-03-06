data Triple a = Tr a a a deriving (Eq, Show)

instance Functor Triple where
  fmap = (<*>) . pure

instance Applicative Triple where
  pure a = Tr a a a
  (Tr f g p) <*> (Tr a b c) = Tr (f a) (g b) (p c)