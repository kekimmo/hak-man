
module Main where

import Control.Monad.State

import Data.Traversable

import qualified Data.Map as Map

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
  sMark <- sprite "mark"
  sEnemy <- sprite "enemy"
  let defs = Draw.Defs { Draw.surface = screen
                       , Draw.areaW = screenW
                       , Draw.areaH = screenH
                       , Draw.spriteWall = sWall
                       , Draw.spriteFloor = sFloor
                       , Draw.spritesPlayer = sPlayer
                       , Draw.spriteEnemy = sEnemy
                       , Draw.spriteMark = sMark
                       }
  let game = Game { ticks = 0
                  , player = Actor (14 * 16, 25 * 16 + 8) Dir.LEFT
                  , level = lev
                  , nextTurn = Dir.LEFT
                  , enemies = [Actor (1 * tileSize + 8, 4 * tileSize + 9) Dir.DOWN]
                  }

  play defs game
  return ()


play :: Draw.Defs -> Game -> IO ()
play defs = eventLoop 
  where eventLoop game = do
          event <- pollEvent
          checkEvent event
          where 
            checkEvent (NoEvent) = do
              let (_, newGame) = runState step game 
              Draw.level defs (level game)
              mapM_ (\en -> Draw.enemy defs en (level game)) (enemies newGame) 
              Draw.player defs (player newGame) (level newGame)
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


