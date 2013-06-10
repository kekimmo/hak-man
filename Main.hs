
module Main where

import Control.Monad.State

import qualified Data.Traversable

import qualified Data.Map as Map
import qualified Data.Set as Set

import Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDL_Image

import Level as L
import qualified Draw 
import qualified Direction as Dir
import Actor
import Game
import Base


main :: IO ()
main = withInit [InitEverything] $ do
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
  sTarget <- Data.Traversable.sequence $ Map.fromSet (sprite . ("mark-" ++) . show) typeSet
  sEnemyDir <- Data.Traversable.sequence $ Map.fromSet (dirSprite "enemy-direction") Dir.allSet
  let pillSet = Set.fromList [REGULAR, POWER]
  sPill <- Data.Traversable.sequence $ Map.fromSet (sprite . ("pill-" ++) . show) pillSet
  let defs = Draw.Defs { Draw.surface = screen
                       , Draw.areaW = screenW
                       , Draw.areaH = screenH
                       , Draw.spriteBg = sBg
                       , Draw.spriteWall = sWall
                       , Draw.spriteFloor = sFloor
                       , Draw.spritesPlayer = sPlayer
                       , Draw.spritesEnemy = sEnemy
                       , Draw.spritesTarget = sTarget
                       , Draw.spritesEnemyDir = sEnemyDir
                       , Draw.spritesPill = sPill
                       }
  let game = Game { ticks = 0
                  , player = Actor (14 * 16, 25 * 16 + 8) Dir.LEFT
                  , level = lev
                  , pills = pls
                  , nextTurn = Dir.LEFT
                  , enemies = Map.fromSet createEnemy typeSet
                  , phase = 0
                  , timeInPhase = 0
                  }

  play defs game
  return ()


createEnemy :: EnemyType -> Actor
createEnemy BLINKY = Actor (14 * tileSize, 13 * tileSize + 8) Dir.LEFT 
createEnemy INKY = Actor (12 * tileSize, 13 * tileSize + 8) Dir.LEFT
createEnemy PINKY = Actor (14 * tileSize, 13 * tileSize + 8) Dir.LEFT
createEnemy CLYDE = Actor (16 * tileSize, 13 * tileSize + 8) Dir.LEFT


play :: Draw.Defs -> Game -> IO ()
play defs = eventLoop 
  where eventLoop game = do
          event <- pollEvent
          checkEvent event
          where 
            checkEvent (NoEvent) = do
              let (output, newGame) = runState step game 
              Draw.bg defs
              -- Draw.level defs (level newGame)
              mapM_ (\(p, pl) -> Draw.pill defs pl p) . Map.assocs . pills $ newGame
              let drawEnemy (eType, en) = Draw.enemy defs eType en (level newGame)
              let drawTarget (eType, t) = Draw.target defs eType t (level newGame)
              mapM_ drawEnemy $ Map.assocs $ enemies newGame
              Draw.player defs (player newGame) (level newGame)
              mapM_ drawTarget $ Map.assocs $ enemyTargets output
              mapM_ print $ messages output
              -- print $ enemyTargets output Map.! INKY
              
              SDL.flip (Draw.surface defs)
              eventLoop newGame

            checkEvent (KeyDown (Keysym key _ _)) = case key of
              SDLK_ESCAPE -> pushEvent Quit 
              SDLK_LEFT -> trn Dir.LEFT
              SDLK_RIGHT -> trn Dir.RIGHT
              SDLK_UP -> trn Dir.UP
              SDLK_DOWN -> trn Dir.DOWN
              _ -> eventLoop game
              where trn d = eventLoop $ setNextTurn d game

            checkEvent (Quit) = return ()

            checkEvent _ = eventLoop game


