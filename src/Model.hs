-- This module contains the data types
-- which represent the state of the game
module Model where

import GenericTypes
import Player
import Enemy
import Weapon
import Data.Ord
import Parser
import Particle
import Projectile
import GameTypes
import Config
import System.Random
import Data.List
import Graphics.Gloss.Interface.Pure.Game (SpecialKey (KeyUp, KeyDown, KeyLeft, KeyRight, KeySpace))

initialState :: StdGen -> String -> [Highscore] -> MetalDogGame
initialState = initialGame

gameOverLogic :: MetalDogGame -> IO MetalDogGame
gameOverLogic game = do updateFile <- writeJsonFile newhighscore
                        return game {highscore = newhighscore}
                          where currentplayer        = player game
                                playerscore          = getIntFromScore (currentScore game)
                                playername           = Player.name currentplayer
                                currenthighscorelist = highscore game
                                playerhighscoreentry = HScore playername playerscore
                                checkedhighscore     = filter (/= playerhighscoreentry) currenthighscorelist
                                newhighscore         = sortOn Data.Ord.Down (playerhighscoreentry : checkedhighscore)

movePlayer :: Player -> [SpecialKey] -> Player
movePlayer player [] = player
movePlayer player (key:listOfKeys)
  |isIllegalMove  = movePlayer player listOfKeys
  |otherwise      = movePlayer movedPlayer listOfKeys
    where movedPlayer   = repositionPlayer player key
          isIllegalMove = isOutOfBounds movedPlayer windowSizeFloat

-- moves player 1 frame forward in given direction
repositionPlayer :: Player -> SpecialKey -> Player
repositionPlayer player x
  |x == KeyUp     = movePlayerWithVector player (0.0,  1.0)
  |x == KeyDown   = movePlayerWithVector player (0.0, -1.0)
  |x == KeyLeft   = movePlayerWithVector player (-1.0, 0.0)
  |x == KeyRight  = movePlayerWithVector player (1.0,  0.0)
  |otherwise      = player

-- moves player with a given movement vector
movePlayerWithVector :: Player -> (Float, Float) -> Player
movePlayerWithVector player (x,y) = move player moveVector
  where moveVector = Vctr x y

moveProjectiles :: Float -> [Projectile] -> [Projectile]
moveProjectiles _ [] = []
moveProjectiles time (x:xs)
  |canBeRemoved       = moveProjectiles time xs
  |otherwise          = updatedProjectile : moveProjectiles time xs
  where projectilePosition   = getPos x
        projectilePositionY  = yP projectilePosition
        projectileMoveVector = Vctr time projectilePositionY
        movedProjectile      = move x projectileMoveVector
        oldAge               = Projectile.age x
        newAge               = oldAge + time
        updatedProjectile    = movedProjectile {Projectile.age = newAge}
        canBeRemoved         = isOutOfBounds movedProjectile windowSizeFloat

moveEnemies :: Float -> [Enemy] -> [Enemy]
moveEnemies _ [] = []
moveEnemies time (x:xs)
  |canBeRemoved       = moveEnemies time xs
  |otherwise          = movedEnemy: moveEnemies time xs
  where enemyPosition   = getPos x
        enemyPositionX  = xP enemyPosition
        enemyPositionY  = yP enemyPosition
        enemyMoveVector = Vctr time time
        movedEnemy      = move x enemyMoveVector
        canBeRemoved    = isOutOfBounds movedEnemy windowSizeFloat

didPlayerGetHit :: [Enemy] -> Player -> ([Enemy], Player)
didPlayerGetHit [] player = ([], player)
didPlayerGetHit (x:xs) player
  |isHit && checkIfPlayerIsDead remainingPlayer                   = (xs, deadPlayer)
  |isHit && checkIfIsDamagedPlayer remainingPlayer damagedPlayer  = didPlayerGetHit xs (damagedPlayer {status = "hit"})
  |otherwise                                                      = insertEnemyIntoTuple x (didPlayerGetHit xs player)
    where isHit           = isHitBy player x
          currentHealth   = getHealth player
          damagedHealth   = currentHealth - 1
          damagedPlayer   = player {Player.health = damagedHealth}
          remainingPlayer = takeDamage player 1
          deadPlayer      = player {Player.health = 0, status = "dead"}

checkIfIsDamagedPlayer :: Maybe Player -> Player -> Bool
checkIfIsDamagedPlayer Nothing _                = False
checkIfIsDamagedPlayer justplayer damagedPlayer = justplayer == Just damagedPlayer

checkIfPlayerIsDead :: Maybe Player -> Bool
checkIfPlayerIsDead Nothing = True
checkIfPlayerIsDead _       = False


--Traverses the projectiles, if a projectile did not kill an enemy, continue on the list,
--and add (insert) the projectile to the result. B 
didEnemyGetHit :: [Projectile] -> [Enemy] -> ([Projectile], [Enemy])
didEnemyGetHit [] xs = ([], xs)
didEnemyGetHit xs [] = (xs, [])
didEnemyGetHit [lastprojectile] lOE
  | areEnemiesKilled  = ([], enemiesStillAlive)
  | otherwise         = ([lastprojectile], enemiesStillAlive)
    where enemiesStillAlive = didProjectileHitEnemies lastprojectile lOE
          areEnemiesKilled  = length enemiesStillAlive /= length lOE
didEnemyGetHit (projectile:nextProjectile:lOP) lOE
  | areEnemiesKilled  = didEnemyGetHit (nextProjectile:lOP) enemiesStillAlive
  | otherwise         = insertProjectileIntoTuple projectile (didEnemyGetHit (nextProjectile:lOP) enemiesStillAlive)
    where enemiesStillAlive = didProjectileHitEnemies projectile lOE
          areEnemiesKilled  = length enemiesStillAlive /= length lOE

-- helper function for removal loop
insertProjectileIntoTuple :: Projectile -> ([Projectile], [Enemy]) -> ([Projectile], [Enemy])
insertProjectileIntoTuple p (projectiles, enemies) = (p:projectiles, enemies)

-- helper function for removal loop
insertEnemyIntoTuple :: Enemy -> ([Enemy], Player) -> ([Enemy], Player)
insertEnemyIntoTuple e (enemies, player) = (e:enemies, player)

--Calculates if a projectile hit an enemy
didProjectileHitEnemies :: Projectile -> [Enemy] -> [Enemy]
didProjectileHitEnemies _ [] = []
didProjectileHitEnemies p (x:xs)
  |isHit     = didProjectileHitEnemies p xs
  |otherwise = x : didProjectileHitEnemies p xs
    where isHit = isHitBy p x

--loops over pressed keys, fires a projectile when a space is found.
fireBullet :: Player -> [SpecialKey] -> ([Projectile], Player)
fireBullet player [] = ([], player)
fireBullet player (x:xs)
  |x == KeySpace && readyToFire = ([createProjectileAt firingPoint], updatedPlayer)
  |otherwise                    = fireBullet player xs
    where currentWeapon             = activeWeapon player
          usedWeapon                = currentWeapon {passedTime = 0.0}
          readyToFire               = passedTime currentWeapon > rechargeTime currentWeapon
          updatedPlayer             = player {activeWeapon = usedWeapon}
          firingPoint               = Pt (xP (getSize player) - 4) (yP (getSize player) + 9)
          playerWeapon              = activeWeapon player
          createProjectileAt point  = createProjectile playerWeapon point

-- random enemy generation

createRandomEnemyKind :: StdGen -> (EnemyKind, StdGen)
createRandomEnemyKind seed
    | num == 0  = (Firework, newGen)
    | num == 1  = (Cat, newGen)
    | num == 2  = (Postman, newGen)
    | num == 3  = (Car, newGen)
    | otherwise = (VacuumCleaner, newGen)
      where ranGen1 = randomR (0, amountEnemyTypes) seed
            newGen  = snd ranGen1
            numF    = abs (fst ranGen1)
            num     = numF - (numF `mod` 1)

--Player is passed to get position information for heat seaking missiles
createRandomEnemy :: Player -> (EnemyKind, StdGen) -> (Enemy, StdGen)
createRandomEnemy player (kind, seed)
    | kind == Firework     = (createdFirework, newSeed)
    | kind == Cat          = (enemyCat ranPos, newSeed)
    | kind == Postman      = (enemyPostman ranPos, newSeed)
    | kind == Car          = (enemyCar ranPos, newSeed)
    | otherwise            = (enemyVacuumCleaner ranPos, newSeed)
      where ranGen = randomR spawnBoundY seed
            posX = fst windowSizeFloat/2
            ranPosY = fst ranGen
            ranPos = Pt posX ranPosY
            newSeed = snd ranGen
            newFireworkSpeed = calcSpeedToPoint (enemyFirework ranPos enemyFireworkSpeed) (getPos player)
            createdFirework = enemyFirework ranPos newFireworkSpeed

--Player is passed to get position information for heat seaking missiles
generateEnemy :: Player -> StdGen -> [Enemy] -> Float -> ([Enemy], StdGen)
generateEnemy player seed xs multiplierfloat
    | length xs < difficultyMultiplier = returnTuple
    | otherwise = ([], seed)
      where newEnem = createRandomEnemy player (createRandomEnemyKind seed)
            returnTuple = ([fst newEnem], snd newEnem)
            difficultyMultiplier = difficulty + multiplier
            multiplier = round (multiplierfloat/multiplierIncrement)

--Calculates the reward based on a list of enemies
getReward :: [Enemy] -> Score
getReward = foldr (iAdd . reward) (Score 0)

--Updates the players' weapon.
updatedPlayerWeapon :: Player -> Float -> Player
updatedPlayerWeapon player time = player {activeWeapon = newWeapon}
  where oldWeapon   = activeWeapon player
        oldTime     = passedTime oldWeapon
        newTime     = oldTime + time
        newWeapon   = oldWeapon {passedTime = newTime}

--Creates particles based on the projectiles
createParticles :: [Projectile] -> [Particle]
createParticles [] = []
createParticles (x:xs) = newParticle : createParticles xs
    where projectilePosition  = getPos x
          newParticle         = standardParticle projectilePosition

