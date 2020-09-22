-- Целое число можно представить как список битов со знаком.

-- Реализуйте функции сложения и умножения для таких целых чисел, считая, что младшие биты идут в
-- начале списка, а старшие — в конце. Можно считать, что на вход не будут подаваться числа с ведущими нулями.

data Bit = Zero | One deriving (Eq)

data Sign = Minus | Plus deriving (Eq)

data Z = Z Sign [Bit] deriving (Eq)

instance Show Bit where
  show Zero = "0"
  show One = "1"

instance Show Z where
  show (Z Minus bit) = "-" ++ show bit
  show (Z Plus bit) = "+" ++ show bit

add :: Z -> Z -> Z
add (Z _ []) (Z _ []) = (Z Plus [])
add (Z _ []) (Z s bs) = (Z s bs)
add (Z s bs) (Z _ []) = (Z s bs)
add a b = toBinWithSign $ fromBinWithSign a + fromBinWithSign b

mul :: Z -> Z -> Z
mul (Z _ []) (Z _ []) = (Z Plus [])
mul (Z _ []) (Z _ bs) = (Z Plus [])
mul (Z _ bs) (Z _ []) = (Z Plus [])
mul a b = toBinWithSign $ fromBinWithSign a * fromBinWithSign b

-- 10110110 = (1·2^7)+(0·2^6)+(1·2^5)+(1·2^4)+(0·2^3)+(1·2^2)+(1·2^1)+(0·2^0) = 128+32+16+4+2 = 182

getDigit :: Bit -> Int
getDigit Zero = 0
getDigit One = 1

fromBin :: [Bit] -> Int
fromBin bits = sum $ foldr (\(idx, el) acc -> ((getDigit el) * 2 ^ idx) : acc) [] $ zip [length bits - 1, length bits - 2 .. 0] $ reverse bits

fromBinWithSign :: Z -> Int
fromBinWithSign (Z Plus bits) = fromBin bits
fromBinWithSign (Z Minus bits) = fromBin bits * (-1)

toBin :: Int -> [Bit]
toBin 0 = []
toBin 1 = [One]
toBin n = conv n
  where
    conv n
      | rest == 1 = bit : [One]
      | rest == 0 = bit : [Zero]
      | otherwise = bit : conv rest
      where
        bit = if n `mod` 2 == 0 then Zero else One
        rest = n `div` 2

toBinWithSign :: Int -> Z
toBinWithSign n
  | n < 0 = (Z Minus $ toBin $ abs n)
  | otherwise = (Z Plus $ toBin n)

-- -- tests

test001 = (add (Z Plus []) (Z Plus [])) == Z Plus []

test002 = (add (Z Plus []) (Z Plus [One])) == Z Plus [One]

test003 = (add (Z Plus []) (Z Minus [One])) == Z Minus [One]

test011 = (add (Z Plus [Zero, One, One]) (Z Plus [One])) == Z Plus [One, One, One]

test012 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One])) == Z Plus [Zero, Zero, Zero, One]

test013 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus [Zero, Zero, One, One]

test021 = (add (Z Minus [Zero, One, One]) (Z Minus [One])) == Z Minus [One, One, One]

test022 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One])) == Z Minus [Zero, Zero, Zero, One]

test023 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One, One]

test031 = (add (Z Minus [Zero, One, One]) (Z Plus [One])) == Z Minus [One, Zero, One]

test032 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One])) == Z Minus [Zero, Zero, One]

test033 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus []

test041 = (add (Z Plus [Zero, One, One]) (Z Minus [One])) == Z Plus [One, Zero, One]

test042 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One])) == Z Plus [Zero, Zero, One]

test043 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Plus []

test051 = (add (Z Plus [One]) (Z Minus [One])) == Z Plus []

test052 = (add (Z Plus [One]) (Z Minus [One, One])) == Z Minus [Zero, One]

test053 = (add (Z Plus [One]) (Z Minus [Zero, One])) == Z Minus [One]

test054 = (add (Z Plus [One]) (Z Minus [Zero, Zero, Zero, One])) == Z Minus [One, One, One]

test055 = (add (Z Plus [One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, Zero, One]

test056 = (add (Z Plus [Zero, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One]

test057 = (add (Z Plus [Zero, One]) (Z Minus [Zero, Zero, One])) == Z Minus [Zero, One]

test058 = (add (Z Plus [One, Zero, One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, One]

testAdd = test001 && test002 && test003 && test011 && test012 && test013 && test021 && test022 && test023 && test031 && test032 && test033 && test041 && test042 && test043 && test051 && test052 && test053 && test054 && test055 && test056 && test057 && test058

emptyZ = Z Plus []

test101 = (mul (Z Plus []) (Z Plus [])) == emptyZ
test102 = (mul (Z Plus []) (Z Plus [One])) == emptyZ
test103 = (mul (Z Plus []) (Z Minus [One])) == emptyZ
test104 = (mul (Z Plus [One]) (Z Plus [])) == emptyZ
test105 = (mul (Z Minus [One]) (Z Plus [])) == emptyZ

test111 = (mul (Z Plus [One]) (Z Plus [One])) == Z Plus [One]
test112 = (mul (Z Minus [One]) (Z Plus [One])) == Z Minus [One]
test113 = (mul (Z Plus [One]) (Z Minus [One])) == Z Minus [One]
test114 = (mul (Z Minus [One]) (Z Minus [One])) == Z Plus [One]

test121 = (mul (Z Plus [One]) (Z Plus [Zero, One])) == Z Plus [Zero, One]
test122 = (mul (Z Plus [Zero, Zero, One]) (Z Plus [Zero, Zero, One])) == Z Plus [Zero, Zero, Zero, Zero, One]

test131 = (mul (Z Plus [One, Zero, One, Zero, One]) (Z Plus [One, One, One])) == Z Plus [One, One, Zero, Zero, One, Zero, Zero, One]
testMul = test101 && test102 && test103 && test104 && test105 && test111 && test112 && test113 && test114 && test121 && test122 && test131
testAll = testAdd && testMul