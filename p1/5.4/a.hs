import Data.Char (isDigit)

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken xs = if all isDigit xs then Just $ Number $ read xs else Nothing

tokenize :: String -> Maybe [Token]
tokenize = mapM asToken . words