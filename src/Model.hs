-- | This module contains the data types
--   which represent the state of the game
module Model where

import GenericTypes
import Player
import Enemy
import Projectile
import GameTypes
import Config
import System.Random
import Graphics.Gloss.Interface.Pure.Game (SpecialKey (KeyUp, KeyDown, KeyLeft, KeyRight, KeySpace))

initialState :: StdGen -> MetalDogGame
initialState gen = initialGame gen

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

movePlayerWithVector :: Player -> (Float, Float) -> Player
movePlayerWithVector player (x,y) = move player moveVector
  where moveVector = Vctr x y

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
        enemyMoveVector = Vctr time time
        movedEnemy = move x enemyMoveVector
        canBeRemoved = isOutOfBounds movedEnemy windowSizeFloat

didPlayerGetHit :: [Enemy] -> Player -> ([Enemy], Player)
didPlayerGetHit [] player = ([], player)
didPlayerGetHit (x:xs) player
  |isHit && (remainingPlayer == Nothing) = (xs, deadPlayer)
  |isHit && (remainingPlayer == (Just damagedPlayer)) = didPlayerGetHit xs (damagedPlayer {status = "hit"})
  |otherwise = insertEnemyIntoTuple x (didPlayerGetHit xs player)
    where isHit = isHitBy player x
          currentHealth = getHealth player
          damagedHealth = currentHealth - 1
          damagedPlayer = player {Player.health = damagedHealth}
          remainingPlayer = takeDamage player 1
          deadPlayer = player {Player.health = 0, status = "dead"}

didEnemyGetHit :: [Projectile] -> [Enemy] -> ([Projectile], [Enemy])
didEnemyGetHit [] xs = ([], xs)
didEnemyGetHit [lastprojectile] lOE
  | areEnemiesKilled = ([], enemiesStillAlive)
  | otherwise = ([lastprojectile], enemiesStillAlive)
    where enemiesStillAlive = didProjectileHitEnemies lastprojectile lOE
          areEnemiesKilled = (length enemiesStillAlive /= length lOE)
didEnemyGetHit xs [] = (xs, [])
didEnemyGetHit (projectile:nextProjectile:lOP) lOE
  | areEnemiesKilled = didEnemyGetHit (nextProjectile:lOP) enemiesStillAlive
  | otherwise = insertProjectileIntoTuple projectile (didEnemyGetHit (nextProjectile:lOP) enemiesStillAlive)
    where enemiesStillAlive = didProjectileHitEnemies projectile lOE
          areEnemiesKilled = (length enemiesStillAlive /= length lOE)

insertProjectileIntoTuple :: Projectile -> ([Projectile], [Enemy]) -> ([Projectile], [Enemy])
insertProjectileIntoTuple p (projectiles, enemies) = (p:projectiles, enemies)

insertEnemyIntoTuple :: Enemy -> ([Enemy], Player) -> ([Enemy], Player)
insertEnemyIntoTuple e (enemies, player) = (e:enemies, player)

didProjectileHitEnemies :: Projectile -> [Enemy] -> [Enemy]
didProjectileHitEnemies _ [] = []
didProjectileHitEnemies p (x:xs)
  |isHit = didProjectileHitEnemies p xs
  |otherwise = x : didProjectileHitEnemies p xs
    where isHit = isHitBy p x

fireBullet :: Player -> [SpecialKey] -> [Projectile]
fireBullet player [] = []
fireBullet player (x:xs)
  |x == KeySpace = [standardProjectile firingPoint]
  |otherwise = fireBullet player xs
    where firingPoint = Pt ((xP (getSize player)) - 4) ((yP (getSize player)) + 9)

createRandomEnemyKind :: StdGen -> (EnemyKind, StdGen)
createRandomEnemyKind seed
    | num == 0 = (Firework, newGen)
    | num == 1 = (Cat, newGen)
    | num == 2 = (Postman, newGen)
    | num == 3 = (Car, newGen)
    | otherwise = (VacuumCleaner, newGen)
      where ranGen1 = randomR (0, amountEnemyTypes) seed
            newGen = snd ranGen1
            numF = abs (fst ranGen1)
            num = numF - (numF `mod` 1)

--Player is passed to get position information for heat seaking missiles
createRandomEnemy :: Player -> (EnemyKind, StdGen) -> (Enemy, StdGen)
createRandomEnemy player (kind, seed)
    | kind == Firework     = ((enemyFirework ranPos){Enemy.speed = newFireworkSpeed}, newSeed)
    | kind == Cat          = (enemyCat ranPos, newSeed)
    | kind == Postman      = (enemyPostman ranPos, newSeed)
    | kind == Car          = (enemyCar ranPos, newSeed)
    | otherwise = (enemyVacuumCleaner ranPos, newSeed)
      where ranGen = randomR spawnBoundY seed
            posX = (fst windowSizeFloat)/2
            ranPosY = fst ranGen
            ranPos = Pt posX ranPosY
            newSeed = snd ranGen
            newFireworkSpeed = calcSpeedToPoint (enemyFirework ranPos) (getPos player)

--Player is passed to get position information for heat seaking missiles
generateEnemy :: Player -> StdGen -> [Enemy] -> ([Enemy], StdGen)
generateEnemy player seed xs
    | length xs < difficulty = returnTuple
    | otherwise = ([], seed)
      where newEnem = createRandomEnemy player (createRandomEnemyKind seed)
            returnTuple = ([fst newEnem], snd newEnem)

getReward :: [Enemy] -> Score
getReward [] = Score 0
getReward (x:xs) = (reward x) `additionScore` (getReward xs)
