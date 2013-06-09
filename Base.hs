
module Base where

import Level


actorSize :: Int
actorSize = 32

actorR :: Int
actorR = actorSize `div` 2

tileSize :: Int
tileSize = 16

tileR :: Int
tileR = tileSize `div` 2


toTile (x, y) = (x `div` tileSize, y `div` tileSize)


wrapActor (w, h) (x, y) = (x `mod` w, y `mod` h)

