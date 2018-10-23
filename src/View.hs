-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Datatypes
import Config

view :: MetalDogGame -> IO Picture
view = return . viewPure

viewPure :: MetalDogGame -> Picture
viewPure (Game player listOfProjectiles listOfEnemies) = pics
  where playerShip = renderPlayer player
        projectiles = renderProjectiles listOfProjectiles
        enemies = renderEnemies listOfEnemies
        pics = pictures ([playerShip] ++ projectiles ++ enemies)
        
renderPlayer :: Player -> Picture
renderPlayer (Plyr (Pt x y)) = translate x y $ drawPlayer

renderProjectiles :: [Projectile] -> [Picture]
renderProjectiles [] = []
renderProjectiles (x:xs) = (translate projectilepositionX projectilepositionY $ drawProjectile) : renderProjectiles xs
  where projectilePosition = position x
        projectilepositionX = pX projectilePosition
        projectilepositionY = pY projectilePosition

renderEnemies :: [Enemy] -> [Picture]
renderEnemies [] = []
renderEnemies (x:xs) = (translate enemyPositionX enemyPositionY $ (drawEnemy (enemyKind x))) : renderEnemies xs
  where enemyPosition = ePosition x
        enemyPositionX = pX enemyPosition
        enemyPositionY = pY enemyPosition

drawEnemy :: EnemyKind -> Picture 
drawEnemy Firework = enemyFireWorkPicture
drawEnemy Cat = enemyCatPicture
drawEnemy Postman = enemyPostmanPicture
drawEnemy Car = enemyCarPicture
drawEnemy VacuumCleaner = enemyVacuumCleanerPicture

drawPlayer :: Picture
drawPlayer = playerPicture

drawProjectile :: Picture
drawProjectile = projectilePicture