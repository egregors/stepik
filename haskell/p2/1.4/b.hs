newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
  fmap f p = Prs fun where
      fun s = (\(a, s) -> (f a, s)) <$> runPrs p s

instance Applicative Prs where
  -- pure :: a -> f a
  pure a = Prs fun where
    fun s = Just (a, s)
  -- (<*>) :: f (a -> b) -> f a -> f b
  pf <*> pv = Prs fun where
    fun s = do
      (g, s')  <- runPrs pf s
      (a, s'') <- runPrs pv s'
      return (g a, s'')

anyChr :: Prs Char
anyChr = Prs f where
    f ""     = Nothing
    f (c:cs) = Just (c, cs)
