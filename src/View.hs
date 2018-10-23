-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import GameTypes
import GenericTypes
import Config
import Player
import Projectile
import Enemy

view :: MetalDogGame -> IO Picture
view = return . viewPure

viewPure :: MetalDogGame -> Picture
viewPure game = pics
  where currentPlayer = player game
        listOfProjectiles = projectiles game
        listOfEnemies = enemies game
        renderedplayerShip = renderPlayer currentPlayer
        renderedprojectiles = renderProjectiles listOfProjectiles
        renderedenemies = renderEnemies listOfEnemies
        pics = pictures ([renderedplayerShip] ++ renderedprojectiles ++ renderedenemies)
        
renderPlayer :: Player -> Picture
renderPlayer player = translate xTrans yTrans $ drawPlayer
  where xTrans = xP (getPos player)
        yTrans = yP (getPos player)

renderProjectiles :: [Projectile] -> [Picture]
renderProjectiles [] = []
renderProjectiles (x:xs) = (translate projectilepositionX projectilepositionY $ drawProjectile) : renderProjectiles xs
  where projectilePosition = getPos x
        projectilepositionX = xP projectilePosition
        projectilepositionY = yP projectilePosition

renderEnemies :: [Enemy] -> [Picture]
renderEnemies [] = []
renderEnemies (x:xs) = (translate enemyPositionX enemyPositionY $ (drawEnemy (enemyKind x))) : renderEnemies xs
  where enemyPosition = getPos x
        enemyPositionX = xP enemyPosition
        enemyPositionY = yP enemyPosition

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