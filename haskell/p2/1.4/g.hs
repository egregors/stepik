
import Control.Applicative ( Alternative(many, empty, (<|>)) )
import Data.Char (chr)

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

instance Alternative Prs where
  empty = Prs f where
      f _ = Nothing
  p<|>q = Prs f where
      f s = runPrs p s <|> runPrs q s

many1 :: Prs a -> Prs [a]
many1 p = (:) <$> p <*> many p

char :: Char -> Prs Char
char c = Prs fun where
  fun (x:xs) | x == c = Just (c, xs)
  fun _ = Nothing

nat :: Prs Int
nat = read <$> many1 digits where
  digits = char '0' <|> char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5' <|> char '6' <|> char '7' <|> char '8' <|> char '9'

mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat