-- Реализуйте функцию distance, возвращающую расстояние между двумя точками.
-- A(-1, 3) и B(6,2).

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point ax ay) (Point bx by) = sqrt ((bx - ax) ^ 2 + (by - ay) ^ 2)