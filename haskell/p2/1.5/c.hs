{-# LANGUAGE TypeOperators #-}

infixr 9 |.|
newtype (|.|) f g a = Cmps {getCmps :: f (g a)} deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap h (Cmps x) = Cmps $ fmap (fmap h) x

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
  pure = Cmps . pure . pure
  (<*>) = undefined
newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) }
  deriving (Eq,Show)

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 (Cmps c1) = getCmps <$> c1

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 (Cmps c1) = fmap unCmps3 c1