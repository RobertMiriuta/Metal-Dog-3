-- | This module contains the data types
--   which represent the state of the game
module Model where

import GenericTypes
import Player
import Enemy
import Projectile
import GameTypes
import Config

initialState :: MetalDogGame
initialState = initialGame

movePlayer :: Player -> (Float, Float) -> Player
movePlayer player (x,y) = Plyr (Pt (currentX + x) (currentY + y))
  where currentX = pX (pPosition player)
        currentY = pY (pPosition player)

moveProjectiles :: Float -> [Projectile] -> [Projectile]
moveProjectiles _ [] = []
moveProjectiles time (x:xs)
  |newX > boundaryX = moveProjectiles time xs
  |otherwise = (Prjtl (Spd speedofX) newProjectilePosition sizeOfProjectile) : moveProjectiles time xs
  where projectilePosition = getPos x
        projectilePositionX = pX projectilePosition
        projectilePositionY = pY projectilePosition
        sizeOfProjectile = size x
        speedofX = speedPerTickX (getSpeed x)
        boundaryX = (fst windowSizeFloat) / 2
        newX = time * speedofX + (pX projectilePosition)
        newProjectilePosition = Pt newX projectilePositionY

moveEnemies :: Float -> [Enemy] -> [Enemy]
moveEnemies _ [] = []
moveEnemies time (x:xs)
  |newX < boundaryX = moveEnemies time xs
  |otherwise = (Enemy enemyKindX eHealth newEnemyPosition newHitboxOfEnemy (Spd speedofX) rewardX) : moveEnemies time xs
  where enemyKindX = enemyKind x
        eHealth = getHealth x
        enemyPosition = ePosition x
        enemyPositionX = pX enemyPosition
        enemyPositionY = pY enemyPosition
        boundaryX = (fst windowSizeFloat) / (-2)
        hitboxOfEnemy = eHitbox x
        newHitboxOfEnemy = (HBox (Pt ((pX (topLeft hitboxOfEnemy)) + time * speedofX) (pY (topLeft hitboxOfEnemy)))
                                 (Pt ((pX (bottomRight hitboxOfEnemy)) + time * speedofX) (pY (bottomRight hitboxOfEnemy))))
        speedofX = speedPerTickX (eSpeed x)
        rewardX = reward x
        newX = time * speedofX + (pX enemyPosition)
        newEnemyPosition = Pt newX enemyPositionY

fireBullet :: MetalDogGame -> MetalDogGame
fireBullet (Game (Plyr(point)) listOfProjectiles listOfEnemies) = Game (Plyr point) newListOfProjectiles listOfEnemies
     where
        newListOfProjectiles = standardProjectile point : listOfProjectiles
