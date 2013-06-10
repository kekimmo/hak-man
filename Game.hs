
module Game where

import Control.Monad.State
import Data.Maybe

import qualified Data.Map as Map

import Base
import Level
import Actor
import Direction as Dir
import Point


type Enemies = Map.Map EnemyType Actor
data EnemyMode = SCATTER | CHASE | FRIGHTENED | RETURN deriving (Show, Eq)
type EnemyModes = Map.Map EnemyType EnemyMode

data Game = Game { ticks :: Integer
                 , player :: Actor
                 , level :: Level
                 , pills :: Map.Map Point Pill
                 , nextTurn :: Direction
                 , enemies :: Enemies
                 , enemyModes :: EnemyModes
                 , phase :: Int
                 , timeInPhase :: Integer
                 } deriving (Show)


type Phase = (EnemyMode, Integer)
phases :: [Phase]
phases = [(SCATTER, 7 * 60)
         ,(CHASE, 20 * 60)
         ]

data Output = Output { enemyTargets :: Map.Map EnemyType Point
                     , messages :: [String]
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

  let pTile = toTile $ pos movedPlr
  let chomped = Map.lookup pTile (pills game)

  let phaseTime = timeInPhase game
  let phaseTimeLimit = snd $ phases !! phase game
  let changePhases = phaseTime > phaseTimeLimit
  let newPhase = if changePhases
                   then (phase game + 1) `rem` length phases
                   else phase game
  let eMode = fst $ phases !! newPhase

  let newEnemyModes = if changePhases
                        then Map.map (changedMode eMode) (enemyModes game)
                        else enemyModes game

  let ens = enemies game
  let targets = findTargets movedPlr newEnemyModes ens

  let movedEnemies = if even $ ticks game
                       then updateEnemies (level game) targets (enemies game)
                       else enemies game
  
  put $ game { ticks = ticks game + 1
             , player = movedPlr
             , pills = (if isJust chomped then Map.delete pTile else id) $ pills game
             , enemies = movedEnemies
             , enemyModes = newEnemyModes
             , phase = newPhase
             , timeInPhase = if changePhases then 1 else phaseTime + 1
             }

  return Output { enemyTargets = targets
                , messages = if changePhases then [show eMode] else []
                }


changedMode :: EnemyMode -> EnemyMode -> EnemyMode
changedMode newMode oldMode = if oldMode == FRIGHTENED then oldMode else newMode


findTargets :: Actor -> EnemyModes -> Enemies -> Map.Map EnemyType Point
findTargets plr enModes ens = Map.mapWithKey findTarget enModes
  where findTarget enType SCATTER = scatterTarget enType
        findTarget enType CHASE = chaseTarget enType 
        findTarget _ _ = (0, 0)
        pTile = toTile $ pos plr
        pDir = dir plr
        errVec n = add pTile $ case pDir of
          UP -> add (-n, 0) $ delta n UP
          _ -> delta n pDir 
        chaseTarget BLINKY = pTile
        chaseTarget PINKY = errVec 4
        chaseTarget INKY = add pTile . mul 2 $ vector blinkyTile (errVec 2)
          where blinkyTile = toTile . pos $ ens Map.! BLINKY
        chaseTarget CLYDE = if dist > 8 then pTile else scatterTarget CLYDE 
          where dist = tileDistance (toTile $ pos clyde) pTile 
                clyde = ens Map.! CLYDE


scatterTarget :: EnemyType -> Point
scatterTarget BLINKY = (25, 0)
scatterTarget PINKY = (2, 0)
scatterTarget INKY = (27, 33)
scatterTarget CLYDE = (0, 33) 


updateEnemies :: Level -> Map.Map EnemyType Point -> Enemies -> Enemies
updateEnemies lev targets ens = movedEnemies
  where 
        turnedEnemies = Map.mapWithKey turnEnemy ens 
        movedEnemies = Map.map (moveActor lev) turnedEnemies
        applyAI eType en = turn (decideTurn lev (targets Map.! eType) en) en 
        turnEnemy eType en = if atJunction en
                         then applyAI eType en
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

