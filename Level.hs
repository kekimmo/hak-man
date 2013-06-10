
module Level where

import Data.Array.Unboxed 
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple

import Direction as Dir
import Point


data Pill = DOT | ENERGIZER deriving (Show, Eq, Ord)
pillSet :: Set.Set Pill
pillSet = Set.fromList [DOT, ENERGIZER]

type Pills = Map.Map Point Pill

type Tile = Bool
type TileArray = UArray Point Tile

data Level = Level { tiles :: TileArray }


instance Show Level where
  show lev = fst $ fold cat ("", 0) lev
    where cat :: (String, Int) -> Point -> Tile -> (String, Int)
          cat (s, prevY) (_, y) tile = (s ++ separator ++ mark, y)
            where separator = if y > prevY then "\n" else ""
                  mark = if tile then " " else "#"


load :: FilePath -> IO (Level, Pills)
load file = do
  raw <- readFile file
  let charLevel = filter (`elem` "# .O") raw 
  let rawArr = listArray ((0, 0), (35, 27)) charLevel
  let tileArr = amap charToTile rawArr 
  let lev = Level { tiles = tileArr }
  let pls = Map.mapKeys swap . Map.mapMaybe charToPill . Map.fromList . assocs $ rawArr
  return (lev, pls)
  where
    charToTile '#' = False
    charToTile _ = True
    charToPill '.' = Just DOT
    charToPill 'O' = Just ENERGIZER
    charToPill _ = Nothing


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

