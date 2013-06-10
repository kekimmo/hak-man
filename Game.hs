
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
                 , pills :: Pills
                 , nextTurn :: Direction
                 , enemies :: Enemies
                 , enemyModes :: EnemyModes
                 , phase :: Int
                 , frightenedTimeLeft :: Int
                 , timeInPhase :: Integer
                 } deriving (Show)


type Phase = (EnemyMode, Integer)
--phases :: Map.Map EnemyMode Integer
phases :: [Phase]
phases = [(SCATTER, 7 * 60)
         ,(CHASE, 20 * 60)
         ]

data Output = Output { enemyTargets :: Map.Map EnemyType Point
                     , messages :: [String]
                     }

setNextTurn :: Direction -> Game -> Game 
setNextTurn d game = game { nextTurn = d }


setPlayer :: Actor -> Game -> Game
setPlayer plr game = game { player = plr }


updatePlayer :: (Actor -> Actor) -> Game -> Game
updatePlayer f game = game { player = f $ player game }


updateEnemies :: (Enemies -> Enemies) -> Game -> Game
updateEnemies f game = game { enemies = f $ enemies game }


updatePills :: (Pills -> Pills) -> Game -> Game
updatePills f game = game { pills = f $ pills game }


removePill :: Point -> Game -> Game
removePill p = updatePills (Map.delete p) 


stepPlayer :: Game -> Game
stepPlayer game = updatePlayer (pMove . pTurn) game
  where pMove = moveActor lev
        pTurn = if canTurn lev plr d then turn d else id
        lev = level game
        plr = player game
        d = nextTurn game


stepChomp :: Game -> Game
stepChomp game = case chomped of
    Nothing  -> game
    (Just _) -> removePill p game
  where p = toTile $ pos $ player game
        chomped = Map.lookup p (pills game)



step :: State Game Output
step = do
  modify stepPlayer
  modify stepChomp

  let mNewPhase = liftM2 changedPhase (gets phase) (gets timeInPhase)
  newPhase <- liftM2 fromMaybe (gets phase) mNewPhase 
  changedPhases <- liftM isJust mNewPhase

  let eMode = fst $ phases !! newPhase

  enModes <- gets enemyModes
  let newEnemyModes = if changedPhases
                        then Map.map (changedMode eMode) enModes
                        else enModes 

  plr <- gets player
  ens <- gets enemies 
  let targets = findTargets plr newEnemyModes ens

  t <- gets ticks
  lev <- gets level
  when (even t) $
    modify $ updateEnemies $ refreshEnemies lev targets
  
  game <- get
  put $ game { ticks = ticks game + 1
             , enemyModes = newEnemyModes
             , phase = newPhase
             , timeInPhase = if changedPhases then 1 else timeInPhase game + 1
             }

  return Output { enemyTargets = targets
                , messages = if changedPhases then [show eMode] else []
                }


changedPhase :: Int -> Integer -> Maybe Int
changedPhase oldPhase phaseTime = if phaseTime > phaseTimeLimit
                                    then Just newPhase
                                    else Nothing
  where phaseTimeLimit = snd $ phases !! oldPhase
        newPhase = (oldPhase + 1) `rem` length phases
                                  

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


refreshEnemies :: Level -> Map.Map EnemyType Point -> Enemies -> Enemies
refreshEnemies lev targets ens = movedEnemies
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

