data Result = Fail | Success

doSomeWork :: Int -> (Result,Int)
doSomeWork d = if odd d then (Success, 0) else (Fail, 69)

processData :: Int -> String
processData x = case doSomeWork x of
    (Success, _) -> "Success"
    (Fail, code) -> "Fail: " ++ show code
