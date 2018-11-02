-- The Config module contains all the important numbers about our game
-- and enables an easy way to change all of the games settings
-- contains default enemy types and values, player values, projectiles
-- weapons, window size and inital variables
module Config where

import Graphics.Gloss hiding (Point)
import GameTypes
import GenericTypes hiding (standardPlayerHitbox)
import Projectile
import Weapon
import Enemy
import Player
import Particle
import System.Random

--Window settings

windowSizeFloat :: (Float, Float)
windowSizeFloat = (800.0, 400.0)

windowSizeInt :: (Int, Int)
windowSizeInt = (800, 400)

spawnBoundY :: (Float, Float)
spawnBoundY = (-165.0, 200.0)

--Enemies:
--Enemy appearance and other values
--top left corner of the picture has to match the top left corner of the hitbox

enemyColor = green

amountEnemyTypes :: Int
amountEnemyTypes = 5

-- starting difficulty
difficulty :: Int
difficulty = 10

multiplierIncrement :: Float
multiplierIncrement = 1.0

--Firework
enemyFireworkPicture :: Picture
enemyFireworkPicture = color enemyColor $ Polygon [(0,-10),(10,0),(10,-5),(25,-5),(25,-15),(10,-15),(10,-20)]

enemyFireworkHitbox :: Hitbox
enemyFireworkHitbox = HBox (Pt 0.0 0.0) (Pt 25 (-20))

enemyFireworkHealth :: Int
enemyFireworkHealth = 1

enemyFireworkSpeed :: Speed
enemyFireworkSpeed = (Spd (-150.0) 0.0)

enemyFireworkReward :: Score
enemyFireworkReward = Score 50

enemyFirework :: Point -> Speed -> Enemy
enemyFirework pt spd = Enemy Firework enemyFireworkHealth pt newHitbox spd enemyFireworkReward
    where newHitbox = HBox newTL newBR
          newTL     = iAdd pt (topLeft enemyFireworkHitbox)
          newBR     = iAdd pt (bottomRight enemyFireworkHitbox)

--Cat
enemyCatPicture :: Picture
enemyCatPicture = color enemyColor $ Pictures [Polygon [(0,0),(0,-5),(5,-5)],
                                               Polygon[(10,-5),(15,0),(15,-5)],
                                               Polygon[(0,-5),(15,-5),(15,-15),(10,-20),(5,-20),(0,-15)]]

enemyCatHitbox :: Hitbox
enemyCatHitbox = HBox (Pt 0.0 0.0) (Pt 15.0 (-20.0))

enemyCatHealth :: Int
enemyCatHealth = 1

enemyCatSpeed :: Speed
enemyCatSpeed = Spd (-50.0) 0.0

enemyCatReward :: Score
enemyCatReward = Score 20

enemyCat :: Point -> Enemy
enemyCat pt = Enemy Cat enemyCatHealth pt newHitbox enemyCatSpeed enemyCatReward
    where newHitbox = HBox newTL newBR
          newTL     = iAdd pt (topLeft enemyCatHitbox)
          newBR     = iAdd pt (bottomRight enemyCatHitbox)

--Postman
enemyPostmanPicture :: Picture
enemyPostmanPicture = color enemyColor $ Pictures [(translate 10 (-5) $ Circle 5),
                                                   Polygon[(0,-10),(0,-25),(3,-25),(3,-15),(5,-15),(5,-35),(8,-35),(8,-25),(12,-25),(12,-35),(15,-35),(15,-15),(17,-15),(17,-25),(20,-25),(20,-10)]]

enemyPostmanHealth :: Int
enemyPostmanHealth = 1

enemyPostmanHitbox :: Hitbox
enemyPostmanHitbox = HBox (Pt 0.0 0.0) (Pt 20.0 (-35.0))

enemyPostmanSpeed :: Speed
enemyPostmanSpeed = Spd (-35.0) (-5.0)

enemyPostmanReward :: Score
enemyPostmanReward = Score 40

enemyPostman :: Point -> Enemy
enemyPostman pt = Enemy Postman enemyPostmanHealth pt newHitbox enemyPostmanSpeed enemyPostmanReward
    where newHitbox = HBox newTL newBR
          newTL     = iAdd pt (topLeft enemyPostmanHitbox)
          newBR     = iAdd pt (bottomRight enemyPostmanHitbox)

--Car
enemyCarPicture :: Picture
enemyCarPicture = color enemyColor $ Pictures [Polygon[(0,-5),(0,-10),(25,-10),(25,-5),(20,-5),(15,0),(10,0),(5,-5)],
                                               (translate 8 (-10) $ Circle 3),
                                               (translate 17 (-10) $ Circle 3)]

enemyCarHealth :: Int
enemyCarHealth = 1

enemyCarHitbox :: Hitbox
enemyCarHitbox = HBox (Pt 0.0 0.0) (Pt 25.0 (-13.0))

enemyCarSpeed :: Speed
enemyCarSpeed = Spd (-35.0) 5.0

enemyCarReward :: Score
enemyCarReward = Score 40

enemyCar :: Point -> Enemy
enemyCar pt = Enemy Car enemyCarHealth pt newHitbox enemyCarSpeed enemyCarReward
  where newHitbox = HBox newTL newBR
        newTL     = iAdd pt (topLeft enemyCarHitbox)
        newBR     = iAdd pt (bottomRight enemyCarHitbox)

--Vacuum Cleaner
enemyVacuumCleanerPicture :: Picture
enemyVacuumCleanerPicture = color enemyColor $ Polygon[(0,-20),(0,-25),(10,-25),(10,-20),(20,-15),(20,-5),(25,-5),(25,0),(20,0),(15,-5),(5,-20),(0,-20)]

enemyVacuumCleanerHealth :: Int
enemyVacuumCleanerHealth = 5

enemyVacuumCleanerHitbox :: Hitbox
enemyVacuumCleanerHitbox = HBox (Pt 0.0 0.0) (Pt 25 (-25))

enemyVacuumCleanerSpeed :: Speed
enemyVacuumCleanerSpeed = Spd (-20.0) 0.0

enemyVacuumCleanerReward :: Score
enemyVacuumCleanerReward = Score 100

enemyVacuumCleaner :: Point -> Enemy
enemyVacuumCleaner pt = Enemy VacuumCleaner enemyVacuumCleanerHealth pt newHitbox enemyVacuumCleanerSpeed enemyVacuumCleanerReward
    where newHitbox = HBox newTL newBR
          newTL     = iAdd pt (topLeft enemyVacuumCleanerHitbox)
          newBR     = iAdd pt (bottomRight enemyVacuumCleanerHitbox)

--Player appearance and other values

playerPicture :: Picture
playerPicture = Pictures[Polygon[(0,-10),(10,-20),(20,-20),(30,-10)], translate 15 (-10) $ scale 1 2 $ Circle 5]

playerSpawnCoordinates :: Point
playerSpawnCoordinates = (Pt spawnX spawnY)
  where spawnX = (-(fst windowSizeFloat) / 2) + 50 --50 pixels off of the left border
        spawnY = 0.0 --middle of the screen

standardPlayerSpeed :: Speed
standardPlayerSpeed = Spd 6.0 6.0

standardPlayerHitbox :: Hitbox
standardPlayerHitbox = HBox (iAdd spawn (Pt 0.0 10.0)) (iAdd spawn (Pt 30.0 (-10.0)))
  where spawnX = (-(fst windowSizeFloat) / 2) + 50
        spawnY = 0.0
        spawn  = Pt spawnX spawnY

standardPlayerHealth :: Int
standardPlayerHealth = 2

standardPlayerWeaponRechargeRate :: Float
standardPlayerWeaponRechargeRate = 0.25

standardPlayerWeapon :: Weapon
standardPlayerWeapon = Wpn standardProjectile 1.0 standardPlayerWeaponRechargeRate

--Projectile appearance and other values

projectileColor = red

projectilePicture :: Picture
projectilePicture = color projectileColor $ translate halfsize (-halfsize) $ rectangleSolid size size
  where halfsize = standardProjectileSizeFloat / 2
        size = standardProjectileSizeFloat

standardProjectile :: Point -> Projectile
standardProjectile point = Prjtl standardProjectileSpeed point standardProjectileSize actualHitbox 0.0
  where actualHitbox = HBox newTopLeft newBottomRight
        newTopLeft = iAdd point (topLeft standardProjectileHitbox)
        newBottomRight = iAdd point (bottomRight standardProjectileHitbox)

standardProjectileSize :: Int
standardProjectileSize = 10

standardProjectileSizeFloat :: Float
standardProjectileSizeFloat = fromIntegral standardProjectileSize

standardProjectileSpeed :: Speed
standardProjectileSpeed = Spd 200.0 0.0

standardProjectileHitbox :: Hitbox
standardProjectileHitbox = HBox (Pt 0 0) (Pt size (-size))
  where size = standardProjectileSizeFloat

-- particles

standardParticle :: Point -> Particle
standardParticle position = Prtcl 0.0 0.5 position True

particlePicture :: Float -> Picture
particlePicture age = translate halfsize (-halfsize) $ rectangleSolid size size
  where halfsize = standardProjectileSizeFloat / 4
        sumthing = age * standardProjectileSizeFloat
        size = standardProjectileSizeFloat - sumthing

-- initial game values
startingPlayer = Plyr playerSpawnCoordinates standardPlayerSpeed Config.standardPlayerHitbox standardPlayerHealth "alive" standardPlayerWeapon
startingProjectiles = []
startingEnemies = [enemyCar (Pt 150.0 0.0), enemyPostman (Pt 0.0 0.0), enemyFirework (Pt 150.0 50.0) enemyFireworkSpeed]
startingKeys = []
startingScore = Score 0
startingState = Playing
startingGameTime = 0.0
startingParticles = []

initialGame :: StdGen -> MetalDogGame
initialGame seed = Game startingPlayer startingProjectiles startingEnemies startingKeys seed startingScore startingState startingGameTime startingParticles
