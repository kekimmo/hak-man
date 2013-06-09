
module Level where

import Data.Array.Unboxed 
import Data.Tuple

import Direction as Dir
import Point


type Tile = Bool
type TileArray = UArray Point Tile
data Level = Level { tiles :: TileArray }

instance Show Level where
  show lev = fst $ fold cat ("", 0) lev
    where cat :: (String, Int) -> Point -> Tile -> (String, Int)
          cat (s, prevY) (_, y) tile = (s ++ separator ++ mark, y)
            where separator = if y > prevY then "\n" else ""
                  mark = if tile then " " else "#"


load :: FilePath -> IO Level
load file = do
  raw <- readFile file
  let charLevel = filter (`elem` "# ") raw 
  let tileArr = listArray ((0, 0), (35, 27)) (map charToTile charLevel)
  return Level { tiles = tileArr }
  where
    charToTile '#' = False
    charToTile ' ' = True


fold :: (a -> Point -> Tile -> a) -> a -> Level -> a
fold f acc lev = foldl g acc (assocs (tiles lev))
  where g acc_g ((y, x), tile) = f acc_g (x, y) tile


within :: Level -> Point -> Bool
within lev p = inRange (bounds $ tiles lev) (swap p)


dimensions :: Level -> Point
dimensions lev = swap . add (1, 1) . snd . bounds . tiles $ lev


walkable :: Level -> Point -> Bool
walkable lev p = inRange (bounds t) levP && t ! levP
  where t = tiles lev
        levP = swap p


wrap :: Level -> Point -> Point
wrap lev (x, y) = (x `mod` w, y `mod` h)
  where (w, h) = dimensions lev 


neighbors :: Point -> Level -> [Point]
neighbors p lev
  | within lev p = map (wrap lev . add p . delta 1) Dir.all
  | otherwise = error "Out of bounds"


neighbor :: Level -> Point -> Direction -> Point
neighbor lev p d = wrap lev . add p . delta 1 $ d

