-- | This module contains the data types
--   which represent the state of the game
module Model where

import Datatypes

startingPlayer = Plyr (Pt (-200.0) 0.0)
startingProjectiles = []
startingEnemies = [Enemy Cat 1 (Pt 200.0 0.0) (HBox (Pt 0.0 0.0) (Pt 15.0 (-20.0))) (Spd (-50.0)) (Score 20)]

initialState :: MetalDogGame
initialState = Game startingPlayer startingProjectiles startingEnemies

movePlayer :: Player -> (Float, Float) -> Player
movePlayer player (x,y) = Plyr (Pt (currentX + x) (currentY + y))
  where currentX = pX (pPosition player)
        currentY = pY (pPosition player)

moveProjectiles :: Float -> [Projectile] -> [Projectile]
moveProjectiles _ [] = []
moveProjectiles time (x:xs) 
  |newX > ((fst windowSizeFloat) / 2) = moveProjectiles time xs
  |otherwise = (Prjtl (Spd speedofX) newProjectilePosition sizeOfProjectile) : moveProjectiles time xs
  where projectilePosition = position x
        projectilePositionX = pX projectilePosition
        projectilePositionY = pY projectilePosition
        sizeOfProjectile = size x
        speedofX = speedPerTickX (speed x)
        newX = time * speedofX + (pX projectilePosition)
        newProjectilePosition = Pt newX projectilePositionY

moveEnemies :: Float -> [Enemy] -> [Enemy]
moveEnemies _ [] = []
moveEnemies time (x:xs) 
  |newX > ((fst windowSizeFloat) / 2) = moveEnemies time xs
  |otherwise = (Enemy enemyKindX eHealth newEnemyPosition newHitboxOfEnemy (Spd speedofX) rewardX) : moveEnemies time xs
  where enemyKindX = enemyKind x
        eHealth = health x
        enemyPosition = ePosition x
        enemyPositionX = pX enemyPosition
        enemyPositionY = pY enemyPosition
        hitboxOfEnemy = eHitbox x
        newHitboxOfEnemy = (HBox (Pt ((pX (topleft hitboxOfEnemy)) + time * speedofX) (pY (topleft hitboxOfEnemy))) 
                                 (Pt ((pX (bottomRight hitboxOfEnemy)) + time * speedofX) (pY (bottomRight hitboxOfEnemy))))
        speedofX = speedPerTickX (eSpeed x)
        rewardX = reward x
        newX = time * speedofX + (pX enemyPosition)
        newEnemyPosition = Pt newX enemyPositionY

fireBullet :: MetalDogGame -> MetalDogGame
fireBullet (Game (Plyr(point)) listOfProjectiles listOfEnemies) = Game (Plyr point) newListOfProjectiles listOfEnemies
     where
        newListOfProjectiles = (Prjtl (Spd 200.0) point 5) : listOfProjectiles