module Main where

main' :: IO ()
main' = do
  putStrLn "What is your name?"
  putStr "Name: "
  name <- getLine
  if null name 
    then main' 
    else putStrLn $ "Hi, " ++ name ++ "!"
  return ()

main :: IO ()
main = main'