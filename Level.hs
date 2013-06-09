
module Level where


import Data.Array.Unboxed 


type Point = (Int, Int)
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

