import Control.Monad

newtype SomeType a = SomeType {runSomeType :: a} deriving (Show)

instance Functor SomeType where
  fmap f x = do f <$> x