
module Main where

import Control.Monad

import Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDL_Image

import qualified Level as L
import qualified Draw 


main :: IO ()
main = withInit [InitEverything] $ do
  lev <- L.load "lev" 
  let screenW = 800
  let screenH = 600
  screen <- setVideoMode screenW screenH 32 [SWSurface]
  sWall <- SDL_Image.load "sprites/wall.png"
  sFloor <- SDL_Image.load "sprites/floor.png"
  let defs = Draw.Defs { Draw.surface = screen
                       , Draw.areaW = screenW
                       , Draw.areaH = screenH
                       , Draw.tileSize = 16
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
          SDL.flip (Draw.surface defs)
          waitEvent >>= checkEvent

        checkEvent (KeyDown (Keysym key _ _)) = case key of
          SDLK_ESCAPE -> pushEvent Quit 
          _ -> eventLoop

        checkEvent (Quit) = return ()

        checkEvent _ = eventLoop

