
module Main where

import Control.Monad.State

import qualified Data.Traversable

import qualified Data.Map as Map
import qualified Data.Set as Set

import Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDL_Image

import qualified Level as L
import qualified Draw 
import qualified Direction as Dir
import Actor
import Game
import Base


main :: IO ()
main = withInit [InitEverything] $ do
  lev <- L.load "lev" 
  let screenW = 800
  let screenH = 600
  screen <- setVideoMode screenW screenH 32 [SWSurface]
  let sprite file = SDL_Image.load $ "sprites/" ++ file ++ ".png"
  let dirSprite file d = sprite $ file ++ "-" ++ show d
  sWall <- sprite "wall"
  sFloor <- sprite "floor"
  sPlayer <- Data.Traversable.sequence $ Map.fromSet (dirSprite "player") Dir.allSet
  -- sMark <- sprite "mark"
  let typeSet = Set.fromList enemyTypes
  sEnemies <- Data.Traversable.sequence $ Map.fromSet (sprite . ("enemy-" ++) . show) typeSet
  sTargets <- Data.Traversable.sequence $ Map.fromSet (sprite . ("mark-" ++) . show) typeSet
  sEnemyDirs <- Data.Traversable.sequence $ Map.fromSet (dirSprite "enemy-direction") Dir.allSet
  let defs = Draw.Defs { Draw.surface = screen
                       , Draw.areaW = screenW
                       , Draw.areaH = screenH
                       , Draw.spriteWall = sWall
                       , Draw.spriteFloor = sFloor
                       , Draw.spritesPlayer = sPlayer
                       , Draw.spriteEnemies = sEnemies
                       , Draw.spriteTargets = sTargets
                       , Draw.spriteEnemyDirs = sEnemyDirs
                       }
  let game = Game { ticks = 0
                  , player = Actor (14 * 16, 25 * 16 + 8) Dir.LEFT
                  , level = lev
                  , nextTurn = Dir.LEFT
                  , enemies = Map.fromSet createEnemy typeSet
                  }

  play defs game
  return ()


createEnemy :: EnemyType -> Actor
createEnemy BLINKY = Actor (14 * tileSize, 13 * tileSize + 8) Dir.LEFT 
createEnemy PINKY = Actor (16 * tileSize, 13 * tileSize + 8) Dir.LEFT
createEnemy INKY = Actor (18 * tileSize, 13 * tileSize + 8) Dir.LEFT
createEnemy CLYDE = Actor (12 * tileSize, 13 * tileSize + 8) Dir.LEFT


play :: Draw.Defs -> Game -> IO ()
play defs = eventLoop 
  where eventLoop game = do
          event <- pollEvent
          checkEvent event
          where 
            checkEvent (NoEvent) = do
              let (output, newGame) = runState step game 
              Draw.level defs (level game)
              let drawEnemy (eType, en) = Draw.enemy defs eType en (level newGame)
              let drawTarget (eType, t) = Draw.mark defs eType t (level newGame)
              mapM_ drawEnemy $ Map.assocs $ enemies newGame
              Draw.player defs (player newGame) (level newGame)
              mapM_ drawTarget $ Map.assocs $ enemyTargets output
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


