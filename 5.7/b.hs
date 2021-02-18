import Control.Monad.Writer (Writer, execWriter, writer)
import Data.Monoid (Sum (..))

type Shopping = Writer (Sum Integer) ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans" 19200
  purchase "Water" 180
  purchase "Lettuce" 328

purchase :: String -> Integer -> Shopping
purchase _ cost = writer ((), Sum cost)

total :: Shopping -> Integer
total = getSum . execWriter