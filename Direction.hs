
module Direction where

import qualified Data.Set as Set

data Direction = UP | LEFT | DOWN | RIGHT deriving (Show, Eq, Ord)

all :: [Direction]
all = [UP, LEFT, DOWN, RIGHT]

allSet :: Set.Set Direction 
allSet = Set.fromList Direction.all

delta :: Int -> Direction -> (Int, Int)
delta x LEFT = (-x, 0)
delta x RIGHT = (x, 0)
delta x UP = (0, -x)
delta x DOWN = (0, x)


opposite :: Direction -> Direction
opposite LEFT = RIGHT
opposite RIGHT = LEFT
opposite UP = DOWN
opposite DOWN = UP

