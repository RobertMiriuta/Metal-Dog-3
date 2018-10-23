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
movePlayer player (x:xs)
  |x == KeyUp     = movePlayer (movePlayerWithVector player (0.0, 1.0)) xs
  |x == KeyDown   = movePlayer (movePlayerWithVector player (0.0, (-1.0))) xs
  |x == KeyLeft   = movePlayer (movePlayerWithVector player ((-1.0), 0.0)) xs
  |x == KeyRight  = movePlayer (movePlayerWithVector player (1.0, 0.0)) xs
  |otherwise = movePlayer player xs 

moveProjectiles :: Float -> [Projectile] -> [Projectile]
moveProjectiles _ [] = []
moveProjectiles time (x:xs)
  |newX > boundaryX   = moveProjectiles time xs
  |otherwise          = movedProjectile : moveProjectiles time xs
  where projectilePosition = getPos x
        projectilePositionY = yP projectilePosition
        projectileMoveVector = Vctr time projectilePositionY
        movedProjectile = move x projectileMoveVector
        boundaryX = (fst windowSizeFloat) / 2
        newX = xP (getPos movedProjectile)

moveEnemies :: Float -> [Enemy] -> [Enemy]
moveEnemies _ [] = []
moveEnemies time (x:xs)
  |newX < boundaryX   = moveEnemies time xs
  |otherwise          = movedEnemy: moveEnemies time xs
  where enemyPosition = getPos x
        enemyPositionX = xP enemyPosition
        enemyPositionY = yP enemyPosition
        boundaryX = (fst windowSizeFloat) / (-2)
        enemyMoveVector = Vctr time enemyPositionY
        movedEnemy = move x enemyMoveVector
        newX = xP (getPos movedEnemy)


fireBullet :: Player -> [SpecialKey] -> [Projectile]
fireBullet player [] = []
fireBullet player (x:xs)
  |x == KeySpace = [standardProjectile (getPos player)]
  |otherwise = fireBullet player xs

