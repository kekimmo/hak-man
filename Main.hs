
module Main where

import Control.Monad.State
import Control.Monad.Reader
import Data.Tuple

import qualified Data.Traversable

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time

import Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDL_Image
import Graphics.UI.SDL.TTF as TTF

import Level as L
import qualified Draw 
import qualified Direction as Dir
import Actor
import Game
import Base
import Enemy
import Event
import Point


main :: IO ()
main = withInit [InitEverything] $ do
  ttfOk <- TTF.init
  unless ttfOk $
    error "Failed to initialize fonts!"
  font <- TTF.openFont "ttf/DejaVuSansMono.ttf" 12
  (lev, pls) <- L.load "lev" 
  let screenW = 800
  let screenH = 600
  screen <- setVideoMode screenW screenH 32 [SWSurface]
  let sprite file = SDL_Image.load $ "sprites/" ++ file ++ ".png"
  let dirSprite file d = sprite $ file ++ "-" ++ show d
  sBg <- sprite "level-2"
  sWall <- sprite "wall"
  sFloor <- sprite "floor"
  sPlayer <- Data.Traversable.sequence $ Map.fromSet (dirSprite "player") Dir.allSet
  -- sMark <- sprite "mark"
  let typeSet = Set.fromList enemyTypes
  sEnemy <- Data.Traversable.sequence $ Map.fromSet (sprite . ("enemy-" ++) . show) typeSet
  sFrightened <- sprite "enemy-frightened"
  sTarget <- Data.Traversable.sequence $ Map.fromSet (sprite . ("mark-" ++) . show) typeSet
  sEnemyDir <- Data.Traversable.sequence $ Map.fromSet (dirSprite "enemy-direction") Dir.allSet
  sPill <- Data.Traversable.sequence $ Map.fromSet (sprite . ("pill-" ++) . show) pillSet
  sNumber <- mapM (sprite . show) [0..9::Int]

  let (levelW, levelH) = mul tileSize $ dimensions lev
  let border = 32
  let levelR = Rect 0 0 levelW levelH
  let msgBottom = screenH - border
  let msgH = 5 * border
  let msgR = Rect (levelW + border) (msgBottom - msgH) (screenW - levelW - border * 2) msgH
  let pointsX = levelW + border
  let pointsW = screenW - border - pointsX
  let pointsR = Rect pointsX border pointsW numberSize 

  let conf = Draw.DrawConfig { Draw.surface = screen
                             , Draw.font = font
                             , Draw.levelR = levelR
                             , Draw.msgR = msgR 
                             , Draw.pointsR = pointsR 
                             , Draw.spriteBg = sBg
                             , Draw.spriteWall = sWall
                             , Draw.spriteFloor = sFloor
                             , Draw.spritesPlayer = sPlayer
                             , Draw.spritesEnemy = sEnemy
                             , Draw.spriteFrightened = sFrightened
                             , Draw.spritesTarget = sTarget
                             , Draw.spritesEnemyDir = sEnemyDir
                             , Draw.spritesPill = sPill
                             , Draw.spritesNumber = sNumber
                             }

  now <- getCurrentTime
  let ds = Draw.DrawState { Draw.msgBuffer = []
                          , Draw.lastDrawn = now }

  let game = Game { ticks = 0
                  , player = Actor (14 * 16, 25 * 16 + 8) Dir.LEFT Dir.LEFT
                  , alive = True
                  , level = lev
                  , points = 0
                  , pills = pls
                  , Game.nextTurn = Dir.LEFT
                  , enemies = Map.fromSet (\t -> Enemy { mode = SCATTER
                                                       , actor = createEnemy t
                                                       , pendingReverse = False
                                                       }) typeSet
                  , phase = 0
                  , timeInPhase = 0
                  , frState = FrightenedState { timeLeft = 0
                                              , enemiesEaten = 0
                                              }
                  , modeOrder = Nothing
                  , pendingEvents = []
                  , lastTargets = Map.empty
                  }

  play conf ds game 
  return ()


createEnemy :: EnemyType -> Actor
createEnemy BLINKY = Actor (14 * tileSize, 13 * tileSize + 8) Dir.LEFT Dir.LEFT
createEnemy INKY = Actor (12 * tileSize, 13 * tileSize + 8) Dir.LEFT Dir.LEFT
createEnemy PINKY = Actor (14 * tileSize, 13 * tileSize + 8) Dir.LEFT Dir.LEFT
createEnemy CLYDE = Actor (16 * tileSize, 13 * tileSize + 8) Dir.LEFT Dir.LEFT


play :: Draw.DrawConfig -> Draw.DrawState -> Game -> IO ()
play conf origDs origGame = do
  startTime <- getCurrentTime
  eventLoop startTime origDs origGame
  where eventLoop :: UTCTime -> Draw.DrawState -> Game -> IO ()
        eventLoop noBefore ds game = do
          ev <- pollEvent
          checkEvent ev
          where 
            checkEvent (NoEvent) = do
              now <- getCurrentTime
              if now < noBefore then 
                eventLoop noBefore ds game
              else do
                let (output, newGame) = runState step game
                (_, newDs) <- Draw.run (drawAll newGame output) conf ds
                eventLoop (addUTCTime (1/60) noBefore) newDs newGame

            checkEvent (KeyDown (Keysym key _ _)) = case key of
              SDLK_ESCAPE -> pushEvent Quit 
              SDLK_LEFT -> trn Dir.LEFT
              SDLK_RIGHT -> trn Dir.RIGHT
              SDLK_UP -> trn Dir.UP
              SDLK_DOWN -> trn Dir.DOWN
              _ -> eventLoop noBefore ds game
              where trn d = eventLoop noBefore ds $ Game.setNextTurn d game

            checkEvent (Quit) = return ()

            checkEvent _ = eventLoop noBefore ds game


drawAll :: Game -> Output -> Draw.Draw () 
drawAll game output = do
  let lev = level game
      ens = enemies game
      formatEvent (t, ev) = "[" ++ show t ++ "] " ++ show ev
      display (AtePill DOT) = False
      display (GotPoints (AtePill DOT) _) = False
      display (Targeted _ _) = False
      display _ = True
      filteredEvents = filter (display . snd) $ events output
      fmtdEvents = map formatEvent filteredEvents
      pureEvents = map snd $ events output
      drawPills = mapM_ (uncurry Draw.pill . swap) . Map.assocs . pills $ game
      drawPlayer = Draw.player (player game) lev
      drawEnemy (eType, en) = Draw.enemy eType en lev
      drawEnemies = mapM_ drawEnemy $ Map.assocs ens 
      drawTarget (enType, t) = Draw.target enType t lev
      drawTargets = mapM_ drawTarget $ Map.assocs $ lastTargets game 
      drawPoints = Draw.points $ points game 
      drawMessages = Draw.messages fmtdEvents 

  Draw.bg
  drawPills
  drawPlayer
  drawEnemies
  drawTargets
  drawPoints
  drawMessages
  Draw.info

  conf <- ask
  liftIO $ SDL.flip (Draw.surface conf)
  liftIO $ mapM_ putStrLn fmtdEvents

