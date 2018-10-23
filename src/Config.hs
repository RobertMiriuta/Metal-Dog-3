module Config where

import Graphics.Gloss
import Datatypes
--Window settings

windowSizeFloat :: (Float, Float)
windowSizeFloat = (800.0, 400.0)

windowSizeInt :: (Int, Int)
windowSizeInt = (800, 400)


--Enemy appearance and other values

enemyColor = green

enemyFireWorkPicture :: Picture
enemyFireWorkPicture = color enemyColor $ Polygon [(0,0),(10,10),(10,5),(25,5),(25,-5),(10,-5),(10,-10)]

enemyCatPicture :: Picture
enemyCatPicture = color enemyColor $ Pictures [Polygon [(0,0),(0,-5),(5,-5)], 
                                               Polygon[(10,-5),(15,0),(15,-5)], 
                                               Polygon[(0,-5),(15,-5),(15,-15),(10,-20),(5,-20),(0,-15)]]
enemyPostmanPicture :: Picture 
enemyPostmanPicture = color enemyColor $ Pictures [(translate 10 5 $ Circle 5), 
                                                   Polygon[(0,0),(0,-15),(3,-15),(3,-5),(5,-5),(5,-25),(8,-25),(8,-15),(12,-15),(12,-25),(15,-25),(15,-5),(17,-5),(17,-15),(20,-15),(20,0)]]

enemyCarPicture :: Picture
enemyCarPicture = color enemyColor $ Pictures [Polygon[(0,0),(0,(-5)),(25,(-5)),(25,0),(20,0),(15,5),(10,5),(5,0)],
                                               (translate 8 (-5) $ Circle 3), 
                                               (translate 17 (-5) $ Circle 3)]

enemyVacuumCleanerPicture :: Picture
enemyVacuumCleanerPicture = Polygon[(0,0),(0,-5),(10,-5),(10,0),(20,5),(20,15),(25,15),(25,20),(20,20),(15,15),(5,0),(0,0)]


--Player appearance and other values and ini

playerColor = light (light blue)

playerPicture :: Picture
playerPicture = color playerColor $ Pictures[Polygon[(0,0),(10,-10),(20,-10),(30,0)], translate 15 0 $ scale 1 2 $ Circle 5]

--Projectile appearance and other values

projectileColor = red

projectilePicture :: Picture
projectilePicture = color projectileColor $ rectangleSolid 4 4

standardProjectile :: Datatypes.Point -> Projectile
standardProjectile point = Prjtl standardProjectileSpeed point standardProjectileSize

standardProjectileSize :: Int
standardProjectileSize = 4

standardProjectileSpeed :: Speed
standardProjectileSpeed = Spd 200.0


--initial values
playerSpawnCoordinates :: Datatypes.Point
playerSpawnCoordinates = (Pt spawnX spawnY)
  where spawnX = (-(fst windowSizeFloat) / 2) + 50 --50 pixels off of the left border
        spawnY = 0.0 --middle of the screen

startingPlayer = Plyr playerSpawnCoordinates
startingProjectiles = []
startingEnemies = [Enemy Cat 1 (Pt 200.0 0.0) (HBox (Pt 0.0 0.0) (Pt 15.0 (-20.0))) (Spd (-50.0)) (Score 20)]

initialGame = Game startingPlayer startingProjectiles startingEnemies