-- | This module contains the data types
--   which represent the state of the game
module Model where

import GenericTypes
import Player
import Enemy
import Projectile
import GameTypes
import Config
import Graphics.Gloss.Interface.Pure.Game (SpecialKey (KeyUp, KeyDown, KeyLeft, KeyRight, KeySpace))


initialState :: MetalDogGame
initialState = initialGame

movePlayerWithVector :: Player -> (Float, Float) -> Player
movePlayerWithVector player (x,y) = move player moveVector
  where moveVector = Vctr x y

movePlayer :: Player -> [SpecialKey] -> Player
movePlayer player [] = player
movePlayer player (key:listOfKeys)
  |isIllegalMove  = movePlayer player listOfKeys
  |otherwise      = movePlayer movedPlayer listOfKeys
    where movedPlayer   = repositionPlayer player key
          isIllegalMove = isOutOfBounds movedPlayer windowSizeFloat

repositionPlayer :: Player -> SpecialKey -> Player
repositionPlayer player x
  |x == KeyUp     = movePlayerWithVector player (0.0, 1.0)
  |x == KeyDown   = movePlayerWithVector player (0.0, (-1.0))
  |x == KeyLeft   = movePlayerWithVector player ((-1.0), 0.0)
  |x == KeyRight  = movePlayerWithVector player (1.0, 0.0) 
  |otherwise = player

moveProjectiles :: Float -> [Projectile] -> [Projectile]
moveProjectiles _ [] = []
moveProjectiles time (x:xs)
  |canBeRemoved       = moveProjectiles time xs
  |otherwise          = movedProjectile : moveProjectiles time xs
  where projectilePosition = getPos x
        projectilePositionY = yP projectilePosition
        projectileMoveVector = Vctr time projectilePositionY
        movedProjectile = move x projectileMoveVector
        canBeRemoved = isOutOfBounds movedProjectile windowSizeFloat

moveEnemies :: Float -> [Enemy] -> [Enemy]
moveEnemies _ [] = []
moveEnemies time (x:xs)
  |canBeRemoved       = moveEnemies time xs
  |otherwise          = movedEnemy: moveEnemies time xs
  where enemyPosition = getPos x
        enemyPositionX = xP enemyPosition
        enemyPositionY = yP enemyPosition
        enemyMoveVector = Vctr time enemyPositionY
        movedEnemy = move x enemyMoveVector
        canBeRemoved = isOutOfBounds movedEnemy windowSizeFloat


fireBullet :: Player -> [SpecialKey] -> [Projectile]
fireBullet player [] = []
fireBullet player (x:xs)
  |x == KeySpace = [standardProjectile (getPos player)]
  |otherwise = fireBullet player xs
