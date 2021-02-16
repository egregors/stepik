module Main where

import Control.Monad (liftM)
import Data.List (isInfixOf)
import System.Directory (getDirectoryContents, removeFile)

removeFile' :: String -> IO ()
removeFile' f = do
  putStr $ "Removing file: " ++ f
  removeFile f

main' :: IO ()
main' = do
  putStrLn "Substring: "
  sub <- getLine
  if sub == ""
    then putStrLn "Canceled"
    else do
      files <- getDirectoryContents "." >>= \ps -> return [p | p <- ps, sub `isInfixOf` p]
      mapM_ removeFile' files

main = main'