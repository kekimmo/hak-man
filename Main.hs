
module Main where

import Control.Monad.State
import Control.Monad.Reader
import Data.Tuple

import qualified Data.Traversable

import qualified Data.Map as Map
import qualified Data.Set as Set

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

  let (levelW, levelH) = mul tileSize $ dimensions lev
  let border = 32
  let levelR = Rect 0 0 levelW levelH
  let msgR = Rect (levelW + border) border 128 128

  let conf = Draw.DrawConfig { Draw.surface = screen
                             , Draw.font = font
                             , Draw.levelR = levelR
                             , Draw.msgR = msgR 
                             , Draw.spriteBg = sBg
                             , Draw.spriteWall = sWall
                             , Draw.spriteFloor = sFloor
                             , Draw.spritesPlayer = sPlayer
                             , Draw.spritesEnemy = sEnemy
                             , Draw.spriteFrightened = sFrightened
                             , Draw.spritesTarget = sTarget
                             , Draw.spritesEnemyDir = sEnemyDir
                             , Draw.spritesPill = sPill
                             }

  let ds = Draw.DrawState { Draw.cursorLine = 0 }

  let game = Game { ticks = 0
                  , player = Actor (14 * 16, 25 * 16 + 8) Dir.LEFT
                  , alive = True
                  , level = lev
                  , pills = pls
                  , nextTurn = Dir.LEFT
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
                  }

  play conf ds game 
  -- void $ TTF.closeFont font
  -- void TTF.quit
  return ()


createEnemy :: EnemyType -> Actor
createEnemy BLINKY = Actor (14 * tileSize, 13 * tileSize + 8) Dir.LEFT 
createEnemy INKY = Actor (12 * tileSize, 13 * tileSize + 8) Dir.LEFT
createEnemy PINKY = Actor (14 * tileSize, 13 * tileSize + 8) Dir.LEFT
createEnemy CLYDE = Actor (16 * tileSize, 13 * tileSize + 8) Dir.LEFT


play :: Draw.DrawConfig -> Draw.DrawState -> Game -> IO ()
play conf = eventLoop 
  where eventLoop ds game = do
          ev <- pollEvent
          checkEvent ev
          where 
            checkEvent (NoEvent) = do
              let (output, newGame) = runState step game
              (_, newDs) <- Draw.run (drawAll newGame output) conf ds
              eventLoop newDs newGame

            checkEvent (KeyDown (Keysym key _ _)) = case key of
              SDLK_ESCAPE -> pushEvent Quit 
              SDLK_LEFT -> trn Dir.LEFT
              SDLK_RIGHT -> trn Dir.RIGHT
              SDLK_UP -> trn Dir.UP
              SDLK_DOWN -> trn Dir.DOWN
              _ -> eventLoop ds game
              where trn d = eventLoop ds $ setNextTurn d game

            checkEvent (Quit) = return ()

            checkEvent _ = eventLoop ds game


drawAll :: Game -> Output -> Draw.Draw () 
drawAll game output = do
  let lev = level game
      ens = enemies game
      drawPills = mapM_ (uncurry Draw.pill . swap) . Map.assocs . pills $ game
      drawPlayer = Draw.player (player game) lev
      drawEnemy (eType, en) = Draw.enemy eType en lev
      drawEnemies = mapM_ drawEnemy $ Map.assocs ens 
      drawTarget (eType, t) = Draw.target eType t lev
      drawTargets = mapM_ drawTarget $ Map.assocs $ enemyTargets output
      drawMessages = Draw.messages . map show $ events output

  Draw.bg
  drawPills
  drawPlayer
  drawEnemies
  drawTargets
  drawMessages

  conf <- ask
  liftIO $ SDL.flip (Draw.surface conf)
  liftIO $ mapM_ print . filter (/= AtePill DOT) $ events output

