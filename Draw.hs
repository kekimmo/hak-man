
module Draw (Defs(..), level, player, enemy, mark) where

import qualified Data.Map as Map

import qualified Actor 
import Direction
import Graphics.UI.SDL 
import Level as L
import Point
import Base


data Defs = Defs { surface :: Surface
                 , areaW :: Int
                 , areaH :: Int
                 , spriteWall :: Surface
                 , spriteFloor :: Surface
                 , spritesPlayer :: Map.Map Direction Surface
                 , spriteEnemy :: Surface
                 , spriteTargets :: [Surface]
                 } deriving (Show)


player :: Defs -> Actor.Actor -> L.Level -> IO Bool
player defs ac = actor defs ac sprite
  where sprite = spritesPlayer defs Map.! Actor.dir ac


enemy :: Defs -> Actor.Actor -> L.Level -> IO Bool
enemy defs ac = actor defs ac sprite
  where sprite = spriteEnemy defs
 

actor :: Defs -> Actor.Actor -> Surface -> L.Level -> IO Bool
actor defs ac sprite lev = do
  blitSurface sprite srcRect1 dest destRect1
  if ox > 0 || oy > 0
    then blitSurface sprite srcRect2 dest destRect2
    else return True
  --blitSurface (spriteMark defs) Nothing dest (tileRect t)
  where 
        dims@(w, h) = mul tileSize $ dimensions lev
        (cx, cy) = wrapActor dims . Actor.corner $ ac
        overlap a limit = max 0 (a + actorSize - limit)
        (ox, oy) = (overlap cx w, overlap cy h)
        srcRect1 = Just $ Rect 0 0 (actorSize - ox) (actorSize - oy)
        destRect1 = Just $ Rect cx cy 0 0
        offset a = if a > 0 then actorSize - a else 0
        srcRect2 = Just $ Rect (offset ox) (offset oy) actorSize actorSize
        destRect2 = Just $ Rect (if ox > 0 then 0 else cx)
                                (if oy > 0 then 0 else cy)
                                0 0
        dest = surface defs
        --(x, y) = Actor.pos ac
        --t = (x `div` tileSize, y `div` tileSize)


mark :: Defs -> Int -> Point -> IO Bool
mark defs n p = blitSurface sprite Nothing (surface defs) (tileRect p)
  where sprite = spriteTargets defs !! n


level :: Defs -> Level -> IO Bool
level defs = L.fold (\acc p t -> acc >> tile defs p t) (return True) 


tile :: Defs -> Point -> Tile -> IO Bool 
tile defs p isFloor = blitSurface sprite srcRect dest destRect
  where srcRect = Nothing -- use entire sprite
        destRect = tileRect p
        dest = surface defs
        sprite = (if isFloor then spriteFloor else spriteWall) defs


tileRect :: Point -> Maybe Rect
tileRect (x, y) = Just $ Rect (x * s) (y * s) 0 0
  where s = tileSize


