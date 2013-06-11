
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
                 , points :: Integer
                 , pills :: Pills
                 , nextTurn :: Direction
                 , enemies :: Enemies
                 , phase :: Int
                 , frState :: FrightenedState
                 , modeOrder :: Maybe EnemyMode
                 , timeInPhase :: Integer
                 , pendingEvents :: [(Integer, Event)]
                 , lastTargets :: Map.Map EnemyType Point
                 } deriving (Show)


type Phase = (EnemyMode, Integer)
--phases :: Map.Map EnemyMode Integer
phases :: [Phase]
phases = [(SCATTER, 7 * 60)
         ,(CHASE, 10 * 20 * 60)
         ]

data Output = Output { events :: [(Integer, Event)]
                     }

setNextTurn :: Direction -> Game -> Game 
setNextTurn d game = game { Game.nextTurn = d }


setPlayer :: Actor -> Game -> Game
setPlayer plr game = game { player = plr }


updatePlayer :: (Actor -> Actor) -> Game -> Game
updatePlayer f game = game { player = f $ player game }


updateEnemies :: (Enemies -> Enemies) -> Game -> Game
updateEnemies f game = game { enemies = f $ enemies game }


updateEnemy :: (Enemy -> Enemy) -> EnemyType -> Game -> Game
updateEnemy f eType = updateEnemies (Map.adjust f eType)


updatePoints :: (Integer -> Integer) -> Game -> Game
updatePoints f game = game { points = f $ points game }


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
addEvent e game = game { pendingEvents = pendingEvents game ++ [(ticks game, e)] }


event :: Event -> State Game ()
event = modify . addEvent


dump :: State Game [(Integer, Event)]
dump = state $ \game -> (pendingEvents game, game { pendingEvents = [] })


msg :: String -> State Game ()
msg = event . Message


stepPlayer :: State Game ()
stepPlayer = do
  game <- get
  let lev = level game
  let plr = player game
  let d = Game.nextTurn game 
  when (atJunction plr && canTurn lev (toTile . pos $ plr) d) $
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


stepEnter :: EnemyType -> Enemy -> Point -> State Game () 
stepEnter enType en tile = do
  let mustReverse = pendingReverse en

  -- event $ EnemyEntered enType tile  

  when mustReverse $
    modify $ updateEnemy (setPendingReverse False . updateActor (reverseDirection . Actor.setNextTurn (opposite $ dir $ actor en))) enType

  ens <- gets enemies
  lev <- gets level
  plr <- gets player
  let ac = actor $ ens Map.! enType
  let nextTile = add tile (delta 1 (Actor.nextTurn ac)) 
  let targetTile = findTarget ens plr enType (mode en) 
  event $ Targeted enType targetTile

  let d = decideTurn lev nextTile targetTile ac
  modify $ updateEnemy (updateActor (Actor.setNextTurn d)) enType

  return ()


-- refreshEnemies :: Level -> Map.Map EnemyType Point -> Enemies -> Enemies
-- refreshEnemies lev targets ens = movedEnemies
--   where 
--         turnedEnemies = Map.mapWithKey turnEnemy ens 
--         movedEnemies = Map.map (updateActor $ moveActor lev) turnedEnemies
--         applyAI eType en = turn (decideTurn lev (targets Map.! eType) en) en 
--         turnEnemy eType en = if atJunction (actor en)
--                                then updateActor (applyAI eType) en
--                                else en


decideTurn :: Level -> Point -> Point -> Actor -> Direction
decideTurn lev junction target ac = snd . minimum $ scoredTurns
  where
    scoredTurns = map score turns
    turns = allowedTurns lev junction (Actor.nextTurn ac)
    score d = (sqDistance (neighbor lev junction d) target, d)


tileDistance :: Point -> Point -> Int
tileDistance a b = round doubleSqrt 
  where doubleSqrt = sqrt $ fromIntegral $ sqDistance a b :: Double


sqDistance :: Point -> Point -> Int
sqDistance (a, b) (x, y) = square 
  where dx = a - x :: Int
        dy = b - y :: Int
        square = dx*dx + dy*dy


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


stepPoints :: [Event] -> State Game ()
stepPoints = mapM_ process 
  where process :: Event -> State Game ()
        process ev = do
        let mPts = pointsFor ev
        when (isJust mPts) $ do
          let pts = fromJust mPts
          modify $ updatePoints (+pts)
          event $ GotPoints ev pts


stepEnemies :: State Game ()
stepEnemies = do
  t <- gets ticks 
  lev <- gets level
  modify $ updateEnemies (Map.mapWithKey (\eType en -> stepEnemy lev (mode en) t en))


stepEnemy :: Level -> EnemyMode -> Integer -> Enemy -> Enemy
stepEnemy lev mo tick en = if shouldAct mo then act en else en
  where shouldAct FRIGHTENED = tick `rem` 4 == 0
        shouldAct _          = even tick 
        act = updateActor (tryMoving . tryTurning)
        tryTurning ac = if atJunction ac && (Actor.nextTurn ac `elem` allowedTurns lev (toTile . pos $ ac) (dir ac)) then applyNextTurn ac else ac
        tryMoving = moveActor lev


pointsFor :: Event -> Maybe Integer 
pointsFor (AtePill DOT) = return 10
pointsFor (AtePill ENERGIZER) = return 100
pointsFor (EnergizerStreak n) = return $ 100 * 2^n
pointsFor _ = mzero


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
  -- let targets = findTargets plr ens

  let tilePos = toTile . pos . actor
  let oldTiles = Map.map tilePos ens

  -- t <- gets ticks
  -- lev <- gets level
  -- when (even t) $
  --   modify $ updateEnemies $ refreshEnemies lev targets
  stepEnemies

  ens <- gets enemies
  let tileChangers = Map.differenceWith (\a b -> if a == b then Nothing else Just b) oldTiles (Map.map tilePos ens)

  Data.Foldable.sequence_ . Map.mapWithKey (\enType tile -> stepEnter enType (ens Map.! enType) tile) $ tileChangers

  plr <- gets player
  let playerChangedTiles = oldPlrTile /= (toTile . pos $ plr)
  let playerCollisionPossible = playerChangedTiles || (not . Map.null $ tileChangers)
  when playerCollisionPossible
    stepCollisions

  game <- get
  put $ game { ticks = ticks game + 1
             }

  evs <- dump
  stepPoints . map snd $ evs

  let getTargetings (Targeted enType tile) = Just (enType, tile)
      getTargetings _ = Nothing
  game <- get
  put $ game { lastTargets = (Map.fromList . mapMaybe (getTargetings . snd) $ evs) `Map.union` lastTargets game }
  
  return Output { events = evs }


changedPhase :: Int -> Integer -> Maybe Int
changedPhase oldPhase phaseTime = if phaseTime > phaseTimeLimit
                                    then Just newPhase
                                    else Nothing
  where phaseTimeLimit = snd $ phases !! oldPhase
        newPhase = (oldPhase + 1) `rem` length phases
                                  

changedMode :: EnemyMode -> EnemyMode -> EnemyMode
changedMode newMode oldMode = if oldMode == FRIGHTENED then oldMode else newMode


findTarget :: Enemies -> Actor -> EnemyType -> EnemyMode -> Point
findTarget ens plr = target
  where 
    target enType SCATTER = scatterTarget enType
    target enType CHASE = chaseTarget enType 
    target _      RETURN = houseEntrance
    target _ _ = (0, 0)
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


allowedTurns :: Level -> Point -> Direction -> [Direction]
allowedTurns lev p d = noReversing . possibleTurns lev $ p 
  where noReversing = filter (/= (opposite d)) 


possibleTurns :: Level -> Point -> [Direction]
possibleTurns lev p = filter (canTurn lev p) Dir.all


canTurn :: Level -> Point -> Direction -> Bool
canTurn lev p d = walkable lev targetTile
  where targetTile = wrap lev . add p . delta 1 $ d


atJunction :: Actor -> Bool
atJunction ac = centered x && centered y 
  where centered u = (u - tileR) `mod` tileSize == 0
        (x, y) = pos ac

