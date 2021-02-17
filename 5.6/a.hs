local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ runReader m . f