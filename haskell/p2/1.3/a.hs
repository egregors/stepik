{-# LANGUAGE FlexibleContexts #-}

import           Text.Parsec

getList :: Parsec String u [String]
getList = many digit `sepBy` char ';'
