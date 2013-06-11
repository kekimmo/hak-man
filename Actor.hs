
module Actor where

import Direction
import Point
import Base


data Actor = Actor { pos :: Point
                   , dir ::  Direction
                   , nextTurn :: Direction
                   } deriving (Show)


move :: Point -> Actor -> Actor
move p ac = ac { pos = p }


turn :: Direction -> Actor -> Actor
turn d ac = ac { dir = d }


setNextTurn :: Direction -> Actor -> Actor
setNextTurn d ac = ac { nextTurn = d }


applyNextTurn :: Actor -> Actor
applyNextTurn ac = turn (nextTurn ac) ac


reverseDirection :: Actor -> Actor
reverseDirection ac = turn (opposite $ dir ac) ac


corner :: Actor -> Point
corner ac = (x - actorSize `div` 2, y - actorSize `div` 2)
  where (x, y) = pos ac

