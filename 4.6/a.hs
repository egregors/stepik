import Data.Semigroup

newtype Xor = Xor {getXor :: Bool}
  deriving (Eq, Show)

instance Semigroup Xor where
  Xor False <> Xor False = Xor False
  Xor False <> Xor True = Xor True
  Xor True <> Xor False = Xor True
  Xor True <> Xor True = Xor False


instance Monoid Xor where
  mempty = Xor False