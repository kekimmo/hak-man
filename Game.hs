
module Game where

import Base
import Level
import Actor
import Direction as Dir
import Point
import qualified Draw


data Input = NoInput | Turn Direction


data Game = Game { player :: Actor
                 , level :: Level
                 , nextTurn :: Direction
                 } deriving (Show)


step :: Input -> Game -> IO Game
step input game = do
  let plr = player game

  let desiredDir = case input of
                     NoInput -> (nextTurn game)
                     Turn d -> d

  let turnedPlr = if canTurn (level game) desiredDir plr
                    then turn desiredDir plr
                    else plr 

  let movedPlr = moveActor (level game) turnedPlr

  return game { player = movedPlr, nextTurn = desiredDir }


moveActor lev ac = if moveOk then move (wrapActor dims newPos) ac else ac 
  where 
  dims = mul tileSize $ dimensions lev
  newPos@(x, y) = add (pos ac) (delta 1 $ dir ac)
  (tx, ty) = toTile newPos
  tcx = tx * tileSize + tileR
  tcy = ty * tileSize + tileR
  moveOk = ok tx ty &&
           (x == tcx || (x < tcx && ok (tx - 1) ty) || (x > tcx && ok (tx + 1) ty)) &&
           (y == tcy || (y < tcy && ok tx (ty - 1)) || (y > tcy && ok tx (ty + 1)))
  ok x y = walkable lev (wrap lev (x, y))


canTurn :: Level -> Direction -> Actor -> Bool
canTurn lev d ac = (reversing || atJunction ac) && walkable lev targetTile
  where reversing = d == opposite (dir ac)
        targetTile = wrap lev . add (toTile . pos $ ac) . delta 1 $ d


atJunction :: Actor -> Bool
atJunction ac = centered x && centered y 
  where (x, y) = pos ac
        centered u = (u + actorR - tileR) `mod` tileSize == 0

