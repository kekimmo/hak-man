
module Point where

type Point = (Int, Int)

add :: Point -> Point -> Point
add (a, b) (x, y) = (a + x, b + y)

mul :: Int -> Point -> Point
mul c (x, y) = (c * x, c * y)

sub :: Point -> Point -> Point
sub (a, b) (x, y) = (a - x, b - y)

vector :: Point -> Point -> Point
vector = flip sub

