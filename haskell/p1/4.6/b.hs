import Data.Semigroup

newtype Maybe' a = Maybe' {getMaybe :: Maybe a}
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Maybe' a) where
  (<>) = undefined

instance Monoid a => Monoid (Maybe' a) where
  mempty = Maybe' $ Just mempty
  mappend (Maybe' Nothing) _ = Maybe' Nothing
  mappend _ (Maybe' Nothing) = Maybe' Nothing
  mappend (Maybe' a) (Maybe' b) = Maybe' (a `mappend` b)
