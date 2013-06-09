
module Main where

import Control.Monad

import qualified Level as L
import qualified Draw 
import Graphics.UI.SDL
import qualified Graphics.UI.SDL.Image as SDL_Image


main :: IO ()
main = withInit [InitEverything] $ do
  lev <- L.load "lev" 
  screen <- setVideoMode 800 600 32 [SWSurface]
  sWall <- SDL_Image.load "sprites/wall.png"
  sFloor <- SDL_Image.load "sprites/floor.png"
  let defs = Draw.Defs { Draw.surface = screen
                       , Draw.areaW = 800
                       , Draw.areaH = 600
                       , Draw.spriteSize = 32
                       , Draw.spriteWall = sWall
                       , Draw.spriteFloor = sFloor
                       }
  game defs lev
  return ()


game :: Draw.Defs -> L.Level -> IO ()
game defs lev = eventLoop
  where eventLoop = do
          event <- pollEvent
          checkEvent event

        checkEvent (NoEvent) = do
          Draw.level defs lev
          waitEvent >>= checkEvent

