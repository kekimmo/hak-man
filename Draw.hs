
module Draw where 

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map

import qualified Actor 
import Direction
import Graphics.UI.SDL 
import qualified Graphics.UI.SDL.TTF as TTF
import Level
import Point
import Base
import qualified Enemy


data DrawConfig = DrawConfig { surface :: Surface
                             , font :: TTF.Font
                             , levelR :: Rect
                             , msgR :: Rect
                             , spriteBg :: Surface
                             , spriteWall :: Surface
                             , spriteFloor :: Surface
                             , spritesPlayer :: Map.Map Direction Surface
                             , spritesEnemy :: Map.Map Enemy.EnemyType Surface
                             , spriteFrightened :: Surface
                             , spritesTarget :: Map.Map Enemy.EnemyType Surface
                             , spritesEnemyDir :: Map.Map Direction Surface
                             , spritesPill :: Map.Map Pill Surface
                             } deriving (Show)


data DrawState = DrawState { cursorLine :: Int }


type Draw = ReaderT DrawConfig (StateT DrawState IO) 


run :: Draw.Draw a -> Draw.DrawConfig -> Draw.DrawState -> IO (a, Draw.DrawState)
run draw conf = runStateT (runReaderT draw conf) 


bg :: Draw Bool
bg = do
  conf <- ask
  liftIO $ blitSurface (spriteBg conf) Nothing (surface conf) (tileRect (0, 0))


player :: Actor.Actor -> Level -> Draw Bool
player ac lev = do
  conf <- ask
  let sprite = spritesPlayer conf Map.! Actor.dir ac
  actor ac sprite lev


messages :: [String] -> Draw Bool
messages [] = return True
messages msgs = do
  conf <- ask
  let color = Color 255 255 255
  texture <- liftIO $ TTF.tryRenderUTF8Solid (font conf) (head msgs) color
  liftIO $ if isJust texture
    then blitSurface (fromJust texture) Nothing (surface conf) (Just $ msgR conf)
    else return False


enemy :: Enemy.EnemyType -> Enemy.Enemy -> Level -> Draw Bool
enemy eType en lev = do
  conf <- ask
  let baseSprite = case mo of
        Enemy.FRIGHTENED -> Just $ spriteFrightened conf
        Enemy.RETURN     -> Nothing
        _                -> Just $ spritesEnemy conf Map.! eType
  let directionSprite = spritesEnemyDir conf Map.! Actor.dir ac
  when (isJust baseSprite) $
    void $ actor ac (fromJust baseSprite) lev
  actor ac directionSprite lev
  where mo = Enemy.mode en
        ac = Enemy.actor en


pill :: Pill -> Point -> Draw Bool
pill pl p = do
  conf <- ask
  let sprite = spritesPill conf Map.! pl
  liftIO $ blitSurface sprite Nothing (surface conf) (tileRect p)  
 

actor :: Actor.Actor -> Surface -> Level -> Draw Bool
actor ac sprite lev = do
  conf <- ask
  let dest = surface conf
  liftIO $ do
    blitSurface sprite srcRect1 dest destRect1
    if ox > 0 || oy > 0
      then blitSurface sprite srcRect2 dest destRect2
      else return True
  --blitSurface (spriteMark conf) Nothing dest (tileRect t)
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
        --(x, y) = Actor.pos ac
        --t = (x `div` tileSize, y `div` tileSize)


target :: Enemy.EnemyType -> Point -> Level -> Draw Bool
target eType p lev = do
  conf <- ask
  let sprite = spritesTarget conf Map.! eType
      yes = blitSurface sprite Nothing (surface conf) (tileRect p)
      no = return True
  liftIO $ if within lev p then yes else no


level :: Level -> Draw Bool
level = Level.fold (\acc p t -> acc >> tile p t) (return True) 


tile :: Point -> Tile -> Draw Bool
tile p isFloor = do
  conf <- ask
  let srcRect = Nothing -- use entire sprite
      destRect = tileRect p
      dest = surface conf
      sprite = (if isFloor then spriteFloor else spriteWall) conf
  liftIO $ blitSurface sprite srcRect dest destRect


tileRect :: Point -> Maybe Rect
tileRect (x, y) = Just $ Rect (x * s) (y * s) 0 0
  where s = tileSize


