import Data.Char (isDigit)

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x : xs)
  | isDigit x = Just x
  | otherwise = findDigit xs

findDigitOrX :: [Char] -> Char
findDigitOrX xs = case findDigit xs of
  Just x -> x
  Nothing -> 'X'
