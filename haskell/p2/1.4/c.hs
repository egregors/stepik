newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE p = PrsE f where
    f [] = Left "unexpected end of input"
    f (c:cs) | p c = Right (c, cs)
             | otherwise = Left $ "unexpected " ++ [c]

charE :: Char -> PrsE Char
charE c = satisfyE (== c)
