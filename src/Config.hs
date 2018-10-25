module Config where

import Graphics.Gloss
import GameTypes
import GenericTypes hiding (standardPlayerHitbox)
import Projectile
import Enemy
import Player

--Window settings

windowSizeFloat :: (Float, Float)
windowSizeFloat = (800.0, 400.0)

windowSizeInt :: (Int, Int)
windowSizeInt = (800, 400)


--Enemy appearance and other values
--top left corner of the picture has to match the top left corner of the hitbox

enemyColor = green

enemyFireWorkPicture :: Picture
enemyFireWorkPicture = color enemyColor $ Polygon [(0,-10),(10,0),(10,-5),(25,-5),(25,-15),(10,-15),(10,-20)]

enemyCatPicture :: Picture
enemyCatPicture = color enemyColor $ Pictures [Polygon [(0,0),(0,-5),(5,-5)],
                                               Polygon[(10,-5),(15,0),(15,-5)],
                                               Polygon[(0,-5),(15,-5),(15,-15),(10,-20),(5,-20),(0,-15)]]
enemyPostmanPicture :: Picture
enemyPostmanPicture = color enemyColor $ Pictures [(translate 10 (-5) $ Circle 5),
                                                   Polygon[(0,-10),(0,-25),(3,-25),(3,-15),(5,-15),(5,-35),(8,-35),(8,-25),(12,-25),(12,-35),(15,-35),(15,-15),(17,-15),(17,-25),(20,-25),(20,-10)]]

enemyCarPicture :: Picture
enemyCarPicture = color enemyColor $ Pictures [Polygon[(0,-5),(0,-10),(25,-10),(25,-5),(20,-5),(15,0),(10,0),(5,-5)],
                                               (translate 8 (-10) $ Circle 3),
                                               (translate 17 (-10) $ Circle 3)]

enemyVacuumCleanerPicture :: Picture
enemyVacuumCleanerPicture = Polygon[(0,-20),(0,-25),(10,-25),(10,-20),(20,-15),(20,-5),(25,-5),(25,0),(20,0),(15,-5),(5,-20),(0,-20)]


--Player appearance and other values and ini

playerColor = light (light blue)

playerPicture :: Picture
playerPicture = color playerColor $ Pictures[Polygon[(0,-10),(10,-20),(20,-20),(30,-10)], translate 15 (-10) $ scale 1 2 $ Circle 5]

--Projectile appearance and other values

projectileColor = red

projectilePicture :: Picture
projectilePicture = color projectileColor $ rectangleSolid 4 4

standardProjectile :: GenericTypes.Point -> Projectile
standardProjectile point = Prjtl standardProjectileSpeed point standardProjectileSize actualHitbox
  where actualHitbox = HBox newTopLeft newBottomRight
        newTopLeft = pointAdd point (topLeft standardProjectileHitbox)
        newBottomRight = pointAdd point (bottomRight standardProjectileHitbox)

standardProjectileSize :: Int
standardProjectileSize = 4

standardProjectileSpeed :: Speed
standardProjectileSpeed = Spd 200.0 0.0

standardProjectileHitbox :: Hitbox
standardProjectileHitbox = HBox (Pt (-2.0) (-2.0)) (Pt 2.0 2.0)

--initial values
playerSpawnCoordinates :: GenericTypes.Point
playerSpawnCoordinates = (Pt spawnX spawnY)
  where spawnX = (-(fst windowSizeFloat) / 2) + 50 --50 pixels off of the left border
        spawnY = 0.0 --middle of the screen

standardPlayerSpeed :: Speed
standardPlayerSpeed = Spd 2.0 2.0

standardPlayerHitbox :: Hitbox
standardPlayerHitbox = HBox (pointAdd spawn (Pt 0.0 10.0)) (pointAdd spawn (Pt 30.0 (-10.0)))
  where spawnX = (-(fst windowSizeFloat) / 2) + 50
        spawnY = 0.0
        spawn  = Pt spawnX spawnY

standardPlayerHealth :: Int
standardPlayerHealth = 1

-- initial values
startingPlayer = Plyr playerSpawnCoordinates standardPlayerSpeed standardPlayerHitbox standardPlayerHealth
startingProjectiles = []
startingEnemies = [Enemy Cat 1 (Pt 200.0 0.0) (HBox (Pt 0.0 0.0) (Pt 15.0 (-20.0))) (Spd (-50.0) 0.0) (Score 20)]
startingKeys = []

initialGame = Game startingPlayer startingProjectiles startingEnemies startingKeys
