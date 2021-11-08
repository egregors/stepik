newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

instance Functor PrsE where
  fmap f p = PrsE fun where
    fun s = case runPrsE p s of
      Left err      -> Left err
      Right (a, s') -> Right (f a, s')

instance Applicative PrsE where
  pure a = PrsE fun where
      fun s = Right (a, s)
  pf <*> pv = PrsE fun where
      fun s = do
          (g, s') <- runPrsE pf s
          (a, s'') <-runPrsE pv s'
          return (g a, s'')
