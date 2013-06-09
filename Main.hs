
module Main where

import qualified Level as L


main :: IO ()
main = do
  lev <- L.load "lev" 
  print lev

