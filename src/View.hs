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
        activeArea = renderActiveArea
        pics = pictures ([renderedplayerShip] ++ renderedprojectiles ++ renderedenemies ++ [activeArea])

renderActiveArea :: Picture
renderActiveArea = Pictures [boundary,axis]
  where halfSizeX = (fst windowSizeFloat) / 2
        halfSizeY = (snd windowSizeFloat) / 2
        leftTop = ((-halfSizeX), halfSizeY)
        rightTop = (halfSizeX, halfSizeY)
        rightBottom = (halfSizeX, (-halfSizeY))
        leftBottom = ((-halfSizeX), (-halfSizeY))
        boundary = color yellow $ Line [leftTop, rightTop, rightBottom, leftBottom, leftTop]
        axis = color yellow $ Line [(0,0), (0,halfSizeY), (0,0), ((-halfSizeX), 0), (0,0), (0,(-halfSizeY)), (0,0), ((halfSizeX), 0)]

renderPlayer :: Player -> Picture
renderPlayer player = Pictures[translate xTrans yTrans $ drawPlayer, drawHitBox player]
  where xTrans = xP (getPos player)
        yTrans = yP (getPos player)

drawHitBox :: Moveable a => a -> Picture
drawHitBox a = color blue $ Line [(xP ptTopLeft, yP ptTopLeft), ptTopRight, (xP ptBottomRight, yP ptBottomRight), ptBottomLeft, (xP ptTopLeft, yP ptTopLeft)]
  where ptTopLeft = topLeft (getHitbox a)
        ptTopRight = (xP ptBottomRight, yP ptTopLeft)
        ptBottomRight = bottomRight (getHitbox a)
        ptBottomLeft = (xP ptTopLeft, yP ptBottomRight)

renderProjectiles :: [Projectile] -> [Picture]
renderProjectiles [] = []
renderProjectiles (x:xs) = (translate projectilepositionX projectilepositionY $ drawProjectile) : renderProjectiles xs
  where projectilePosition = getPos x
        projectilepositionX = xP projectilePosition
        projectilepositionY = yP projectilePosition

renderEnemies :: [Enemy] -> [Picture]
renderEnemies [] = []
renderEnemies (x:xs) = Pictures[(translate enemyPositionX enemyPositionY $ (drawEnemy (enemyKind x))) , drawHitBox x] : renderEnemies xs
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
