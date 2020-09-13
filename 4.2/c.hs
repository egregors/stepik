data Result = Fail | Success

data Result' = Fail' Int | Success'

instance Show Result' where
  show (Fail' code) = "Fail: " ++ show code
  show Success' = "Success"

doSomeWork' :: SomeData -> Result'
doSomeWork' x = case doSomeWork x of
  (Success, _) -> Success'
  (Fail, code) -> Fail' code