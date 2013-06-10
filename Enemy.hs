
module Enemy where

import Control.Arrow

import Actor


data EnemyMode = SCATTER | CHASE | FRIGHTENED | RETURN deriving (Show, Eq)
type Enemy = (EnemyMode, Actor) 

mode :: Enemy -> EnemyMode
mode = fst

actor :: Enemy -> Actor
actor = snd 

updateMode :: (EnemyMode -> EnemyMode) -> Enemy -> Enemy
updateMode = first 

updateActor :: (Actor -> Actor) -> Enemy -> Enemy
updateActor = second

