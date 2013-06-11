
module Event where

import Enemy 
import Level (Pill(..))
import Point
import Text.Printf


data Event = Message String
           | Order EnemyMode
           | Eaten
           | AteEnemy EnemyType
           | AtePill Pill
           | EnergizerStreak Int
           | GotPoints Event Integer
           | Targeted EnemyType Point
           | EnemyEntered EnemyType Point
           deriving (Eq, Show)


toString :: Event -> String
toString (Message s) = s
toString (Order mo) = printf "Enemy mode: %s." $ show mo
toString Eaten = "Got eaten!"
toString (AteEnemy enType) = printf "Ate %s." $ show enType
toString (AtePill DOT) = printf "Ate a dot."
toString (AtePill ENERGIZER) = "Ate an energizer."
toString (EnergizerStreak n) = printf "Energizer streak of %d!" n
toString (GotPoints ev pts) = printf "Got %d points for %s." pts (show ev)
toString ev = show ev
