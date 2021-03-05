-- Плоскость разбита на квадратные ячейки. Стороны ячеек параллельны осям координат.
-- Координаты углов ячейки с координатой (0,0) имеют неотрицательные координаты.
-- Один из углов этой ячейки имеет координату (0,0). С ростом координат ячеек
-- увеличиваются координаты точек внутри этих ячеек.

-- Реализуйте функции getCenter, которая принимает координату ячейки и возвращает
-- координату ее центра, и функцию getCell, которая принимает координату точки и
-- возвращает номер ячейки в которой находится данная точка. В качестве первого
-- аргумента обе эти функции принимают ширину ячейки.

import GHC.Float (int2Double)

data Coord a = Coord a a deriving (Show, Eq)

truncate' :: Double -> Int -> Double
truncate' x n = (fromIntegral (floor (x * t))) / t
  where
    t = 10 ^ n

getCenter :: Double -> Coord Int -> Coord Double
getCenter size (Coord x y) = Coord cx cy
  where
    cx = truncate' (size * int2Double x + size / 2) 2
    cy = truncate' (size * int2Double y + size / 2) 2

getCell :: Double -> Coord Double -> Coord Int
getCell size (Coord x y) = Coord cx cy
  where
    cx = floor $ x / size
    cy = floor $ y / size

test1 = getCenter 2.2 (Coord 2 1) == Coord 5.5 3.3
test2 = getCell 2.2 (Coord 3.2 1.6) == Coord 1 0