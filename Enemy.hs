
module Enemy where

import Actor


data EnemyMode = SCATTER | CHASE | FRIGHTENED | RETURN deriving (Show, Eq)
data Enemy = Enemy { mode :: EnemyMode
                   , actor :: Actor
                   , pendingReverse :: Bool
                   } deriving (Show)

setMode :: EnemyMode -> Enemy -> Enemy
setMode = updateMode . const 

updateMode :: (EnemyMode -> EnemyMode) -> Enemy -> Enemy
updateMode f en = en { mode = f $ mode en }

updateActor :: (Actor -> Actor) -> Enemy -> Enemy
updateActor f en = en { actor = f $ actor en }

setPendingReverse :: Bool -> Enemy -> Enemy
setPendingReverse a en = en { pendingReverse = a }

harmsPlayer :: EnemyMode -> Bool
harmsPlayer SCATTER = True
harmsPlayer CHASE = True
harmsPlayer FRIGHTENED = False
harmsPlayer RETURN = False

