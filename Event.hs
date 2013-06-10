
module Event where

import Enemy 
import Level (Pill)



data Event = Message String
           | Order EnemyMode
           | Eaten
           | AteEnemy EnemyType
           | AtePill Pill
           | EnergizerStreak Int
           deriving (Show, Eq)

