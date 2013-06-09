
module Actor where

import Direction
import Point
import Base


data Actor = Actor { pos :: Point
                   , dir ::  Direction
                   } deriving (Show)


move :: Point -> Actor -> Actor
move p ac = ac { pos = p }


turn :: Direction -> Actor -> Actor
turn d ac = ac { dir = d }


corner :: Actor -> Point
corner ac = (x - actorSize `div` 2, y - actorSize `div` 2)
  where (x, y) = pos ac

