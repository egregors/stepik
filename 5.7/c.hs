import Control.Monad.Writer (Writer, execWriter, writer)
import Data.Monoid (Sum (..))

type Shopping = Writer (Sum Integer, [String]) ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans" 19200
  purchase "Water" 180
  purchase "Lettuce" 328

purchase :: String -> Integer -> Shopping
purchase item cost = writer ((), (Sum cost, [item]))

total :: Shopping -> Integer
total s = getSum $ fst $ execWriter s

items :: Shopping -> [String]
items = snd . execWriter 