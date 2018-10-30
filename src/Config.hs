module Config where

import Graphics.Gloss hiding (Point)
import GameTypes
import GenericTypes hiding (standardPlayerHitbox)
import Projectile
import Enemy
import Player
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

difficulty :: Int
difficulty = 30

--Firework
enemyFireworkPicture :: Picture
enemyFireworkPicture = color enemyColor $ Polygon [(0,-10),(10,0),(10,-5),(25,-5),(25,-15),(10,-15),(10,-20)]

enemyFireworkHitbox :: Hitbox
enemyFireworkHitbox = HBox (Pt 0.0 0.0) (Pt 25 (-20))

enemyFireworkHealth :: Int
enemyFireworkHealth = 1

enemyFireworkSpeed :: Speed
enemyFireworkSpeed = (Spd (-100.0) 0.0)

enemyFireworkReward :: Score
enemyFireworkReward = Score 50

enemyFirework :: Point -> Enemy
enemyFirework pt = Enemy Firework enemyFireworkHealth pt newHitbox enemyFireworkSpeed enemyFireworkReward
    where newHitbox = HBox newTL newBR
          newTL     = pointAdd pt (topLeft enemyFireworkHitbox)
          newBR     = pointAdd pt (bottomRight enemyFireworkHitbox)

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
          newTL     = pointAdd pt (topLeft enemyCatHitbox)
          newBR     = pointAdd pt (bottomRight enemyCatHitbox)

--Postman
enemyPostmanPicture :: Picture
enemyPostmanPicture = color enemyColor $ Pictures [(translate 10 (-5) $ Circle 5),
                                                   Polygon[(0,-10),(0,-25),(3,-25),(3,-15),(5,-15),(5,-35),(8,-35),(8,-25),(12,-25),(12,-35),(15,-35),(15,-15),(17,-15),(17,-25),(20,-25),(20,-10)]]

enemyPostmanHealth :: Int
enemyPostmanHealth = 1

enemyPostmanHitbox :: Hitbox
enemyPostmanHitbox = HBox (Pt 0.0 0.0) (Pt 20.0 (-35.0))

enemyPostmanSpeed :: Speed
enemyPostmanSpeed = Spd (-35.0) 0.0

enemyPostmanReward :: Score
enemyPostmanReward = Score 40

enemyPostman :: Point -> Enemy
enemyPostman pt = Enemy Postman enemyPostmanHealth pt newHitbox enemyPostmanSpeed enemyPostmanReward
    where newHitbox = HBox newTL newBR
          newTL     = pointAdd pt (topLeft enemyPostmanHitbox)
          newBR     = pointAdd pt (bottomRight enemyPostmanHitbox)

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
enemyCarSpeed = Spd (-35.0) 0.0

enemyCarReward :: Score
enemyCarReward = Score 40

enemyCar :: Point -> Enemy
enemyCar pt = Enemy Car enemyCarHealth pt newHitbox enemyCarSpeed enemyCarReward
  where newHitbox = HBox newTL newBR
        newTL     = pointAdd pt (topLeft enemyCarHitbox)
        newBR     = pointAdd pt (bottomRight enemyCarHitbox)

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
          newTL     = pointAdd pt (topLeft enemyVacuumCleanerHitbox)
          newBR     = pointAdd pt (bottomRight enemyVacuumCleanerHitbox)
--Player appearance and other values and ini

playerPicture :: Picture
playerPicture = Pictures[Polygon[(0,-10),(10,-20),(20,-20),(30,-10)], translate 15 (-10) $ scale 1 2 $ Circle 5]

--Projectile appearance and other values

projectileColor = red

projectilePicture :: Picture
projectilePicture = color projectileColor $ translate 2 (-2) $ rectangleSolid 4 4

standardProjectile :: Point -> Projectile
standardProjectile point = Prjtl standardProjectileSpeed point standardProjectileSize actualHitbox
  where actualHitbox = HBox newTopLeft newBottomRight
        newTopLeft = pointAdd point (topLeft standardProjectileHitbox)
        newBottomRight = pointAdd point (bottomRight standardProjectileHitbox)

standardProjectileSize :: Int
standardProjectileSize = 4

standardProjectileSpeed :: Speed
standardProjectileSpeed = Spd 200.0 0.0

standardProjectileHitbox :: Hitbox
standardProjectileHitbox = HBox (Pt 0 0) (Pt 4.0 (-4.0))

--initial values
playerSpawnCoordinates :: Point
playerSpawnCoordinates = (Pt spawnX spawnY)
  where spawnX = (-(fst windowSizeFloat) / 2) + 50 --50 pixels off of the left border
        spawnY = 0.0 --middle of the screen

standardPlayerSpeed :: Speed
standardPlayerSpeed = Spd 6.0 6.0

standardPlayerHitbox :: Hitbox
standardPlayerHitbox = HBox (pointAdd spawn (Pt 0.0 10.0)) (pointAdd spawn (Pt 30.0 (-10.0)))
  where spawnX = (-(fst windowSizeFloat) / 2) + 50
        spawnY = 0.0
        spawn  = Pt spawnX spawnY

standardPlayerHealth :: Int
standardPlayerHealth = 2

-- initial values
startingPlayer = Plyr playerSpawnCoordinates standardPlayerSpeed standardPlayerHitbox standardPlayerHealth "alive"
startingProjectiles = []
startingEnemies = [enemyCar (Pt 150.0 0.0), enemyPostman (Pt 0.0 0.0), enemyFirework (Pt 150.0 50.0)]
startingKeys = []
startingScore = Score 0
startingState = Playing

initialGame :: StdGen -> MetalDogGame
initialGame seed = Game startingPlayer startingProjectiles startingEnemies startingKeys seed startingScore startingState
