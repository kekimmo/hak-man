
module Game where

import Control.Monad.State

import Base
import Level
import Actor
import Direction as Dir
import Point


data Game = Game { ticks :: Integer
                 , player :: Actor
                 , level :: Level
                 , nextTurn :: Direction
                 , enemies :: [Actor]
                 } deriving (Show)


data Output = Output { }


setNextTurn :: Direction -> Game -> Game 
setNextTurn d game = game { nextTurn = d }


step :: State Game Output
step = do
  game <- get
  let plr = player game

  let d = nextTurn game
  let turnedPlr = if canTurn (level game) plr d
                    then turn d plr
                    else plr 

  let movedPlr = moveActor (level game) turnedPlr

  let movedEnemies = if even $ ticks game
                       then updateEnemies (level game) movedPlr (enemies game)
                       else enemies game
  
  put $ game { ticks = ticks game + 1
             , player = movedPlr
             , enemies = movedEnemies
             }

  return Output {}


updateEnemies :: Level -> Actor -> [Actor] -> [Actor]
updateEnemies lev plr ens = movedEnemies
  where 
        turnedEnemies = map turnEnemy ens 
        movedEnemies = map (moveActor lev) turnedEnemies
        applyAI en = turn (decideTurn lev (toTile . pos $ plr) en) en 
        turnEnemy en = if atJunction en
                         then applyAI en
                         else en


decideTurn :: Level -> Point -> Actor -> Direction
decideTurn lev target ac = snd . minimum $ scoredTurns
  where
    scoredTurns = map score turns
    turns = allowedTurns lev ac
    junction = toTile (pos ac) 
    score d = (sqDistance (neighbor lev junction d) target, d)


sqDistance :: Point -> Point -> Int
sqDistance (a, b) (x, y) = square 
  where dx = a - x :: Int
        dy = b - y :: Int
        square = dx*dx + dy*dy



moveActor :: Level -> Actor -> Actor
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
  ok s t = walkable lev (wrap lev (s, t))


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

