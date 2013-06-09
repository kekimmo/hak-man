
module Game where

import Control.Monad.State

import qualified Data.Map as Map

import Base
import Level
import Actor
import Direction as Dir
import Point


type Enemies = Map.Map EnemyType Actor

data Game = Game { ticks :: Integer
                 , player :: Actor
                 , level :: Level
                 , nextTurn :: Direction
                 , enemies :: Enemies
                 } deriving (Show)


data Output = Output { enemyTargets :: Map.Map EnemyType Point
                     }


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

  let targets = findTargets turnedPlr (enemies game)

  let movedEnemies = if even $ ticks game
                       then updateEnemies (level game) movedPlr (enemies game)
                       else enemies game
  
  put $ game { ticks = ticks game + 1
             , player = movedPlr
             , enemies = movedEnemies
             }

  return Output { enemyTargets = targets }


findTargets :: Actor -> Enemies -> Map.Map EnemyType Point
findTargets plr ens = Map.mapWithKey findTarget ens
  where pTile = toTile $ pos plr
        pDir = dir plr
        errVec n = add pTile $ case pDir of
          UP -> add (-n, 0) $ delta n UP
          _ -> delta n pDir 
        findTarget BLINKY _ = pTile
        findTarget PINKY _ = errVec 4
        findTarget INKY _ = add pTile . mul 2 $ vector blinkyTile (errVec 2)
          where blinkyTile = toTile . pos $ ens Map.! BLINKY
        findTarget CLYDE clyde = if dist > 8 then pTile else scatterTarget CLYDE 
          where dist = tileDistance (toTile $ pos clyde) pTile 


scatterTarget :: EnemyType -> Point
scatterTarget BLINKY = (25, 0)
scatterTarget PINKY = (2, 0)
scatterTarget INKY = (27, 0)
scatterTarget CLYDE = (0, 33) 


updateEnemies :: Level -> Actor -> Enemies -> Enemies
updateEnemies lev plr ens = movedEnemies
  where 
        turnedEnemies = Map.map turnEnemy ens 
        movedEnemies = Map.map (moveActor lev) turnedEnemies
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


tileDistance :: Point -> Point -> Int
tileDistance a b = round doubleSqrt 
  where doubleSqrt = sqrt $ fromIntegral $ sqDistance a b :: Double


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

