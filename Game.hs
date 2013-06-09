
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
                 , enemies :: [Actor]
                 } deriving (Show)


step :: Input -> Game -> IO Game
step input game = do
  let plr = player game

  let desiredDir = case input of
                     NoInput -> (nextTurn game)
                     Turn d -> d

  let turnedPlr = if canTurn (level game) plr desiredDir 
                    then turn desiredDir plr
                    else plr 

  let movedPlr = moveActor (level game) turnedPlr

  let applyAI en = turn (decideTurn (level game) (toTile . pos $ movedPlr) en) en 

  let turnEnemy en = if atJunction en
                       then applyAI en
                       else en

  let turnedEnemies = map turnEnemy (enemies game)

  let movedEnemies = map (moveActor (level game)) turnedEnemies
  
  return game { player = movedPlr
              , nextTurn = desiredDir
              , enemies = movedEnemies
              }


decideTurn :: Level -> Point -> Actor -> Direction
decideTurn lev target ac = snd . minimum $ scoredTurns
  where
    scoredTurns = map score turns
    turns = allowedTurns lev ac
    junction = toTile (pos ac) 
    score turn = (distance (neighbor lev junction turn) target, turn)


distance :: Point -> Point -> Int
distance (a, b) (x, y) = round . sqrt . fromIntegral $ ((a - x) ^ 2 + (b - y) ^ 2)


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


allowedTurns :: Level -> Actor -> [Direction]
allowedTurns lev ac = noReversing . possibleTurns lev $ ac
  where noReversing = filter (/= (opposite $ dir ac)) 


possibleTurns :: Level -> Actor -> [Direction]
possibleTurns lev ac = if atJunction ac
                         then filter (canTurn lev ac) Dir.all
                         else []


canTurn :: Level -> Actor -> Direction -> Bool
canTurn lev ac d = (reversing || atJunction ac) && walkable lev targetTile
  where reversing = d == opposite (dir ac)
        targetTile = wrap lev . add (toTile . pos $ ac) . delta 1 $ d


atJunction :: Actor -> Bool
atJunction ac = centered x && centered y 
  where (x, y) = pos ac
        centered u = (u - tileR) `mod` tileSize == 0

