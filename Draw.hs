
module Draw (Defs(..), bg, level, player, enemy, target, pill) where

import qualified Data.Map as Map

import qualified Actor 
import Direction
import Graphics.UI.SDL 
import Level
import Point
import Base


data Defs = Defs { surface :: Surface
                 , areaW :: Int
                 , areaH :: Int
                 , spriteBg :: Surface
                 , spriteWall :: Surface
                 , spriteFloor :: Surface
                 , spritesPlayer :: Map.Map Direction Surface
                 , spritesEnemy :: Map.Map EnemyType Surface
                 , spritesTarget :: Map.Map EnemyType Surface
                 , spritesEnemyDir :: Map.Map Direction Surface
                 , spritesPill :: Map.Map Pill Surface
                 } deriving (Show)


bg :: Defs -> IO Bool
bg defs = blitSurface (spriteBg defs) Nothing (surface defs) (tileRect (0, 0))


player :: Defs -> Actor.Actor -> Level -> IO Bool
player defs ac = actor defs ac sprite
  where sprite = spritesPlayer defs Map.! Actor.dir ac


enemy :: Defs -> EnemyType -> Actor.Actor -> Level -> IO Bool
enemy defs eType ac lev = do actor defs ac baseSprite lev
                             actor defs ac directionSprite lev
  where baseSprite = spritesEnemy defs Map.! eType
        directionSprite = spritesEnemyDir defs Map.! Actor.dir ac


pill :: Defs -> Pill -> Point -> IO Bool
pill defs pl p = blitSurface sprite Nothing (surface defs) (tileRect p)  
  where sprite = spritesPill defs Map.! pl
 

actor :: Defs -> Actor.Actor -> Surface -> Level -> IO Bool
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


target :: Defs -> EnemyType -> Point -> Level -> IO Bool
target defs eType p lev = if within lev p then yes else no
  where sprite = spritesTarget defs Map.! eType
        yes = blitSurface sprite Nothing (surface defs) (tileRect p)
        no = return True

level :: Defs -> Level -> IO Bool
level defs = Level.fold (\acc p t -> acc >> tile defs p t) (return True) 


tile :: Defs -> Point -> Tile -> IO Bool 
tile defs p isFloor = blitSurface sprite srcRect dest destRect
  where srcRect = Nothing -- use entire sprite
        destRect = tileRect p
        dest = surface defs
        sprite = (if isFloor then spriteFloor else spriteWall) defs


tileRect :: Point -> Maybe Rect
tileRect (x, y) = Just $ Rect (x * s) (y * s) 0 0
  where s = tileSize


