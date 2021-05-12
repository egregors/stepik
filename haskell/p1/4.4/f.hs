import Data.List (isInfixOf)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving (Show, Eq)

data Person = Person {firstName :: String, lastName :: String, age :: Int} deriving (Show, Eq)

parsePerson :: String -> Either Error Person
parsePerson [] = Left ParsingError
parsePerson cmd
  | isValidCmd cmd /= True = Left ParsingError
  | isDataComplete cmd /= True = Left IncompleteDataError
  | strIsNumber age /= True = Left $ IncorrectDataError age
  | otherwise =
    Right Person {firstName = firstName, lastName = lastName, age = read age}
  where
    firstName = getFieldValue cmd "firstName"
    lastName = getFieldValue cmd "lastName"
    age = getFieldValue cmd "age"

isValidParam :: String -> Bool
isValidParam param
  | not $ isInfixOf " = " param = False
  | otherwise = True

isValidCmd :: String -> Bool
isValidCmd cmd = all id . map isValidParam $ lines cmd

isDataComplete :: [Char] -> Bool
isDataComplete cmd = all id $ map (flip elem fields) requiredFields
  where
    fields = map (head . words) $ lines cmd
    requiredFields = ["firstName", "lastName", "age"]

strIsNumber :: [Char] -> Bool
strIsNumber [] = False
strIsNumber [x] = x `elem` ['0' .. '9']
strIsNumber (x : xs) = if not $ x `elem` ['0' .. '9'] then False else strIsNumber xs

getFieldValue :: [Char] -> [Char] -> [Char]
getFieldValue cmd fieldName = drop (length fieldName + 3) . concat . filter (\e -> (head . words) e == fieldName) $ lines cmd