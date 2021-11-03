newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
  fmap f p = Prs fun where
      fun s = (\(a, s) -> (f a, s)) <$> runPrs p s

anyChr :: Prs Char
anyChr = Prs f where
    f ""     = Nothing
    f (c:cs) = Just (c, cs)
