-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Datatypes

view :: MetalDogGame -> IO Picture
view = return . viewPure

viewPure :: MetalDogGame -> Picture
viewPure (Game (Plyr (Pt x y)) listOfProjectiles listOfEnemies) = pics
  where playerShip = (translate x y $ color paddleColor $ drawPlayer)
        projectiles = renderProjectiles listOfProjectiles
        enemies = renderEnemies listOfEnemies
        pics = pictures ([playerShip] ++ projectiles ++ enemies)
        paddleColor = light (light blue)

renderProjectiles :: [Projectile] -> [Picture]
renderProjectiles [] = []
renderProjectiles (x:xs) = (translate projectilepositionX projectilepositionY $ color projectileColor $ rectangleSolid 2 2) : renderProjectiles xs
  where projectileColor = red
        projectilePosition = position x
        projectilepositionX = pX projectilePosition
        projectilepositionY = pY projectilePosition

renderEnemies :: [Enemy] -> [Picture]
renderEnemies [] = []
renderEnemies (x:xs) = (translate projectilepositionX projectilepositionY $ color enemyColor $ (drawEnemy (enemyKind x))) : renderEnemies xs
  where enemyColor = green
        projectilePosition = ePosition x
        projectilepositionX = pX projectilePosition
        projectilepositionY = pY projectilePosition

drawEnemy :: EnemyKind -> Picture 
drawEnemy Firework = Polygon [(0,0),(10,10),(10,5),(25,5),(25,-5),(10,-5),(10,-10)]
drawEnemy Cat = Pictures [Polygon [(0,0),(0,-5),(5,-5)], Polygon[(10,-5),(15,0),(15,-5)], Polygon[(0,-5),(15,-5),(15,-15),(10,-20),(5,-20),(0,-15)]]
drawEnemy Postman = Pictures [(translate 10 5 $ Circle 5), Polygon[(0,0),(0,-15),(3,-15),(3,-5),(5,-5),(5,-25),(8,-25),(8,-15)
                                                                        ,(12,-15),(12,-25),(15,-25),(15,-5),(17,-5),(17,-15),(20,-15),(20,0)]]
drawEnemy Car = Pictures [Polygon[(0,0),(0,(-5)),(25,(-5)),(25,0),(20,0),(15,5),(10,5),(5,0)],
                                                    (translate 8 (-5) $ Circle 3), (translate 17 (-5) $ Circle 3)]
drawEnemy VacuumCleaner = Polygon[(0,0),(0,-5),(10,-5),(10,0),(20,5),(20,15),(25,15),(25,20),(20,20),(15,15),(5,0),(0,0)]

drawPlayer :: Picture
drawPlayer = Pictures[Polygon[(0,0),(10,-10),(20,-10),(30,0)], translate 15 0 $ scale 1 2 $ Circle 5]