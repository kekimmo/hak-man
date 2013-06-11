
module Event where

import Enemy 
import Level (Pill)
import Point


data Event = Message String
           | Order EnemyMode
           | Eaten
           | AteEnemy EnemyType
           | AtePill Pill
           | EnergizerStreak Int
           | GotPoints Event Integer
           | Targeted EnemyType Point
           | EnemyEntered EnemyType Point
           deriving (Show, Eq)

