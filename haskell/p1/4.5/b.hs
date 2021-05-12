data Nat = Zero | Suc Nat deriving (Show)

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = (Suc (toNat (n -1)))

add :: Nat -> Nat -> Nat
add a b = toNat $ fromNat a + fromNat b

mul :: Nat -> Nat -> Nat
mul a b = toNat $ fromNat a * fromNat b

fac :: Nat -> Nat
fac n = toNat $ fac' $ fromNat n

fac' n = foldl (\acc el -> acc * el) 1 [1 .. n]