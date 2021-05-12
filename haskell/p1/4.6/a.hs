import Data.Semigroup

newtype Xor = Xor {getXor :: Bool}
  deriving (Eq, Show)

instance Semigroup Xor where
  Xor False <> Xor False = Xor False
  Xor False <> Xor True = Xor True
  Xor True <> Xor False = Xor True
  Xor True <> Xor True = Xor False

instance Monoid Xor where
  mappend a b = a <> b
  mempty = Xor False
  mconcat xs = foldr mappend mempty xs