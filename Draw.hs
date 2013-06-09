
module Draw (Defs(..), level) where

import Graphics.UI.SDL 
import Level as L


data Defs = Defs { surface :: Surface
                 , areaW :: Int
                 , areaH :: Int
                 , spriteSize :: Int
                 , spriteWall :: Surface
                 , spriteFloor :: Surface
                 } deriving (Show)


level :: Defs -> Level -> IO Bool
level defs = L.fold (\acc p t -> acc >> tile defs p t) (return True) 


tile :: Defs -> Point -> Tile -> IO Bool 
tile defs (x, y) isFloor = blitSurface sprite srcRect dest destRect
  where srcRect = Nothing -- use entire sprite
        size = spriteSize defs
        destRect = Just $ Rect (x * size) (y * size) 0 0
        dest = surface defs
        sprite = (if isFloor then spriteFloor else spriteWall) defs

