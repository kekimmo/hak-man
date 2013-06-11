
module Draw where 

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.Digits
import qualified Data.Map as Map
import Data.Time
import Text.Printf

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
                             , pointsR :: Rect
                             , spriteBg :: Surface
                             , spriteWall :: Surface
                             , spriteFloor :: Surface
                             , spritesPlayer :: Map.Map Direction Surface
                             , spritesEnemy :: Map.Map Enemy.EnemyType Surface
                             , spriteFrightened :: Surface
                             , spritesTarget :: Map.Map Enemy.EnemyType Surface
                             , spritesEnemyDir :: Map.Map Direction Surface
                             , spritesPill :: Map.Map Pill Surface
                             , spritesNumber :: [Surface]
                             } deriving (Show)


data DrawState = DrawState { msgBuffer :: [String]
                           , lastDrawn :: UTCTime
                           }


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


points :: Integer -> Draw Bool
points pts = do
  area <- asks pointsR
  dest <- asks surface
  let ptsDigits = map fromIntegral $ digits 10 pts
  let leftmost = rectX area + rectW area - length ptsDigits * numberSize
  bgColor <- liftIO $ mapRGB (surfaceGetPixelFormat dest) 0 0 50
  liftIO $ fillRect dest (Just area) bgColor
  liftM and $ zipWithM (\i digit -> number (leftmost + i * numberSize, rectY area) digit) [0..] ptsDigits


number :: Point -> Int -> Draw Bool
number (x, y) i = do
  when (i < 0 || i > 9) $
    error "Number out of range"
  dest <- asks surface
  numbers <- asks spritesNumber
  liftIO $ blitSurface (numbers !! i) Nothing dest (Just $ Rect x y 0 0) 


info :: Draw Bool
info = do
  ds <- get
  let prev = lastDrawn ds
  now <- liftIO getCurrentTime
  let elapsed = diffUTCTime now prev 
  let fps = round $ recip elapsed :: Int
  put $ ds { lastDrawn = now }
  text (printf "FPS: %d" fps)  (0, 0) 


addMessage :: String -> Draw ()
addMessage "" = return ()
addMessage msg = do
  ds <- get
  put $ ds { msgBuffer = msg : msgBuffer ds } 


messages :: Draw Bool
messages = do
  area <- asks msgR
  fn <- asks font
  dest <- asks surface
  lineH <- liftIO $ TTF.fontLineSkip fn
  color <- liftIO $ mapRGB (surfaceGetPixelFormat dest) 0 0 50
  buffer <- gets msgBuffer 

  -- Assume monospace
  (letterW, _) <- liftIO $ TTF.utf8Size fn "a"

  let maxW = rectW area `div` letterW

  let breakLine "" = []
      breakLine s = let (a, b) = splitAt maxW s in a : breakLine b

  let brokenLines = foldr (\m ms -> ms ++ breakLine m) [] buffer

  let maxLines = rectH area `div` lineH
  let visibleLines = reverse . take maxLines . reverse $ brokenLines

  liftIO $ fillRect dest (Just area) color

  let draws = zipWith (\i s -> text s (rectX area, rectY area + i * lineH)) [0..] visibleLines
  liftM and $ sequence draws 


text :: String -> Point -> Draw Bool
text s p = do
  let color = Color 255 255 255
  let (x, y) = p
  let destRect = Just $ Rect x y 0 0 

  dest <- asks surface
  fn <- asks font
  texture <- liftIO $ TTF.tryRenderUTF8Blended fn s color

  success <- liftIO $ if isJust texture
    then blitSurface (fromJust texture) Nothing dest destRect
    else return False
  
  liftIO $ maybe (return ()) freeSurface texture

  return success


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


