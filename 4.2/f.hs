-- Целое число можно представить как список битов со знаком.

-- Реализуйте функции сложения и умножения для таких целых чисел, считая, что младшие биты идут в
-- начале списка, а старшие — в конце. Можно считать, что на вход не будут подаваться числа с ведущими нулями.

data Bit = Zero | One

data Sign = Minus | Plus

data Z = Z Sign [Bit]

instance Show Bit where
  show Zero = "0"
  show One = "1"

instance Show Z where
  show (Z Minus bit) = "-" ++ show bit
  show (Z Plus bit) = "+" ++ show bit

add :: Z -> Z -> Z
add = undefined

mul :: Z -> Z -> Z
mul = undefined


