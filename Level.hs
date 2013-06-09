
module Level where


import Data.Array.Unboxed 


type TileArray = UArray (Int, Int) Bool
data Level = Level { tiles :: TileArray } deriving (Show)


load :: FilePath -> IO Level
load file = do
  raw <- readFile file
  let charLevel = filter (`elem` "# ") raw 
  let tileArr = listArray ((0, 0), (36, 28)) (map charToTile charLevel)
  return Level { tiles = tileArr }
  where
    charToTile '#' = False
    charToTile ' ' = True

