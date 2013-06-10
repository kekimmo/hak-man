
module Game where

import Control.Monad.State
import Data.Maybe
import Data.Foldable (sequence_)

import qualified Data.Map as Map

import Base
import Level
import Actor
import Direction as Dir
import Point
import Enemy
import Event


type Enemies = Map.Map EnemyType Enemy
type EnemyModes = Map.Map EnemyType EnemyMode


data FrightenedState = FrightenedState { timeLeft :: Int
                                       , enemiesEaten :: Int
                                       } deriving (Show)


updateEnemiesEaten :: (Int -> Int) -> FrightenedState -> FrightenedState
updateEnemiesEaten f s = s { enemiesEaten = f $ enemiesEaten s }


data Game = Game { ticks :: Integer
                 , player :: Actor
                 , alive :: Bool
                 , level :: Level
                 , pills :: Pills
                 , nextTurn :: Direction
                 , enemies :: Enemies
                 , phase :: Int
                 , frState :: FrightenedState
                 , modeOrder :: Maybe EnemyMode
                 , timeInPhase :: Integer
                 , pendingEvents :: [Event]
                 } deriving (Show)


type Phase = (EnemyMode, Integer)
--phases :: Map.Map EnemyMode Integer
phases :: [Phase]
phases = [(SCATTER, 7 * 60)
         ,(CHASE, 20 * 60)
         ]

data Output = Output { enemyTargets :: Map.Map EnemyType Point
                     , events :: [Event]
                     }

setNextTurn :: Direction -> Game -> Game 
setNextTurn d game = game { nextTurn = d }


setPlayer :: Actor -> Game -> Game
setPlayer plr game = game { player = plr }


updatePlayer :: (Actor -> Actor) -> Game -> Game
updatePlayer f game = game { player = f $ player game }


updateEnemies :: (Enemies -> Enemies) -> Game -> Game
updateEnemies f game = game { enemies = f $ enemies game }


updateEnemy :: (Enemy -> Enemy) -> EnemyType -> Game -> Game
updateEnemy f eType = updateEnemies (Map.adjust f eType)


updatePills :: (Pills -> Pills) -> Game -> Game
updatePills f game = game { pills = f $ pills game }


updateFrState :: (FrightenedState -> FrightenedState) -> Game -> Game
updateFrState f game = game { frState = f $ frState game }


removePill :: Point -> Game -> Game
removePill p = updatePills (Map.delete p) 


frighten :: Game -> Game
frighten game = game { modeOrder = Just FRIGHTENED
                     , frState = FrightenedState { timeLeft = 7 * 60
                                                 , enemiesEaten = 0
                                                 }
                     }


changeModes :: EnemyMode -> Game -> Game
changeModes mo = updateEnemies (Map.map (\en -> alter (mode en) mo en)) 
  where alter :: EnemyMode -> EnemyMode -> Enemy -> Enemy
        alter RETURN _  = setMode RETURN
        alter CHASE  to = setMode to . setPendingReverse True
        alter _      to = setMode to


setAlive :: Bool -> Game -> Game
setAlive a game = game { alive = a }


addEvent :: Event -> Game -> Game
addEvent e game = game { pendingEvents = pendingEvents game ++ [e] }


event :: Event -> State Game ()
event = modify . addEvent


dump :: State Game [Event]
dump = state $ \game -> (pendingEvents game, game { pendingEvents = [] })


msg :: String -> State Game ()
msg = event . Message


stepPlayer :: State Game ()
stepPlayer = do
  game <- get
  let lev = level game
  let plr = player game
  let d = nextTurn game
  when (canTurn lev plr d) $
    modify $ updatePlayer $ turn d
  modify $ updatePlayer $ moveActor lev


stepChomp :: State Game ()
stepChomp = do
  plr <- gets player
  pls <- gets pills
  let p = toTile $ pos plr
  let mChomped = Map.lookup p pls
  when (isJust mChomped) $ do
    let chomped = fromJust mChomped
    modify $ removePill p
    when (chomped == ENERGIZER) $
      modify frighten
    event $ AtePill chomped


stepEnter :: EnemyType -> Point -> State Game () 
stepEnter enType _ = do
  ens <- gets enemies
  let mustReverse = pendingReverse $ ens Map.! enType
  void . when mustReverse $
    modify $ updateEnemies (Map.adjust (setPendingReverse False . updateActor reverseDirection) enType)


stepCollisions :: State Game ()
stepCollisions = do
  ens <- gets enemies
  plr <- gets player
  let tilePos = toTile . pos
  let plrTile = tilePos plr
  let colliding = Map.filter ((plrTile ==) . tilePos . actor) ens
  let (harmful, harmless) = Map.partition (harmsPlayer . mode) colliding
  let frightened = Map.filter ((FRIGHTENED == ) . mode) harmless
  unless (Map.null harmful) $ do
    modify $ setAlive False 
    event Eaten
  Data.Foldable.sequence_ . Map.mapWithKey hitFrightened $ frightened
  return ()
  where hitFrightened eType _ = do
                  modify $ updateEnemy (setMode RETURN) eType
                  modify $ updateFrState (updateEnemiesEaten (+1))
                  event $ AteEnemy eType
                  fr <- gets frState
                  event . EnergizerStreak . enemiesEaten $ fr 


step :: State Game Output
step = do
  game <- get
  put $ game { modeOrder = Nothing }

  let oldPlrTile = toTile . pos $ player game
  stepPlayer
  stepChomp

  game <- get
  let ft = timeLeft . frState $ game
  let frightEnds = ft == 1
  put $ if ft > 0 then 
    game { frState = (frState game) { timeLeft = ft - 1 } }
  else
    game { timeInPhase = timeInPhase game + 1 }

  when (ft <= 1) $ do
    let mNewPhase = liftM2 changedPhase (gets phase) (gets timeInPhase)
    gotNewPhase <- liftM isJust mNewPhase
    newPhase <- liftM2 fromMaybe (gets phase) mNewPhase 
    let eMode = fst $ phases !! newPhase
    let changedPhases = gotNewPhase || frightEnds
    game <- get
    put $ game { phase = newPhase
               , timeInPhase = if changedPhases then 1 else timeInPhase game + 1
               , modeOrder = if changedPhases then Just eMode else Nothing
               }

  mOrder <- gets modeOrder
  when (isJust mOrder) $ do
    let order = fromJust mOrder 
    modify $ changeModes order
    event $ Order order

  plr <- gets player
  ens <- gets enemies 
  let targets = findTargets plr ens

  let tilePos = toTile . pos . actor
  let oldTiles = Map.map tilePos ens

  t <- gets ticks
  lev <- gets level
  when (even t) $
    modify $ updateEnemies $ refreshEnemies lev targets

  ens <- gets enemies
  let tileChangers = Map.differenceWith (\a b -> if a == b then Nothing else Just b) oldTiles (Map.map tilePos ens)

  Data.Foldable.sequence_ . Map.mapWithKey stepEnter $ tileChangers

  plr <- gets player
  let playerChangedTiles = oldPlrTile /= (toTile . pos $ plr)
  let playerCollisionPossible = playerChangedTiles || (not . Map.null $ tileChangers)
  when playerCollisionPossible
    stepCollisions

  game <- get
  put $ game { ticks = ticks game + 1
             }
  evs <- dump

  return Output { enemyTargets = targets
                , events = evs
                }


changedPhase :: Int -> Integer -> Maybe Int
changedPhase oldPhase phaseTime = if phaseTime > phaseTimeLimit
                                    then Just newPhase
                                    else Nothing
  where phaseTimeLimit = snd $ phases !! oldPhase
        newPhase = (oldPhase + 1) `rem` length phases
                                  

changedMode :: EnemyMode -> EnemyMode -> EnemyMode
changedMode newMode oldMode = if oldMode == FRIGHTENED then oldMode else newMode


findTargets :: Actor -> Enemies -> Map.Map EnemyType Point
findTargets plr ens = Map.mapWithKey findTarget . Map.map mode $ ens 
  where findTarget enType SCATTER = scatterTarget enType
        findTarget enType CHASE = chaseTarget enType 
        findTarget _      RETURN = houseEntrance
        findTarget _ _ = (0, 0)
        pTile = toTile $ pos plr
        pDir = dir plr
        errVec n = add pTile $ case pDir of
          UP -> add (-n, 0) $ delta n UP
          _ -> delta n pDir 
        chaseTarget BLINKY = pTile
        chaseTarget PINKY = errVec 4
        chaseTarget INKY = add pTile . mul 2 $ vector blinkyTile (errVec 2)
          where blinkyTile = toTile . pos $ actor $ ens Map.! BLINKY
        chaseTarget CLYDE = if dist > 8 then pTile else scatterTarget CLYDE 
          where dist = tileDistance (toTile $ pos $ actor clyde) pTile 
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
        movedEnemies = Map.map (updateActor $ moveActor lev) turnedEnemies
        applyAI eType en = turn (decideTurn lev (targets Map.! eType) en) en 
        turnEnemy eType en = if atJunction (actor en)
                               then updateActor (applyAI eType) en
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

