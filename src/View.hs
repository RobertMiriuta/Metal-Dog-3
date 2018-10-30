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
viewPure game
    |isPlaying = pics
    |otherwise = picsPaused
  where isPlaying           = (gameState game) == Playing
        currentPlayer       = player game
        halfSizeX           = (fst windowSizeFloat) / 2
        halfSizeY           = (snd windowSizeFloat) / 2
        listOfProjectiles   = projectiles game
        listOfEnemies       = enemies game
        renderedplayerShip  = renderPlayer currentPlayer
        renderedprojectiles = renderProjectiles listOfProjectiles
        scr                 = currentScore game
        renderedenemies     = renderEnemies listOfEnemies
        activeArea          = renderActiveArea
        scorePic            = scale 0.15 0.15.color orange.text $ show scr
        score               = translate (halfSizeX-200) ((-halfSizeY)+20) scorePic
        pausePic            = scale 0.5 0.5.color orange.text $ show Paused
        pause               = translate (-halfSizeX) (halfSizeY-60) pausePic
        pics                = pictures ([renderedplayerShip] ++ renderedprojectiles ++ renderedenemies ++ [activeArea] ++ [score])
        picsPaused          = pictures ([renderedplayerShip] ++ renderedprojectiles ++ renderedenemies ++ [activeArea] ++ [score] ++ [pause])

renderActiveArea :: Picture
renderActiveArea    = Pictures [boundary,axis]
  where halfSizeX   = (fst windowSizeFloat) / 2
        halfSizeY   = (snd windowSizeFloat) / 2
        leftTop     = ((-halfSizeX), halfSizeY)
        rightTop    = (halfSizeX, halfSizeY)
        rightBottom = (halfSizeX, (-halfSizeY))
        leftBottom  = ((-halfSizeX), (-halfSizeY))
        boundary    = color yellow $ Line [leftTop, rightTop, rightBottom, leftBottom, leftTop]
        axis        = color yellow $ Line [(0,0), (0,halfSizeY), (0,0), ((-halfSizeX), 0), (0,0), (0,(-halfSizeY)), (0,0), ((halfSizeX), 0)]

renderPlayer :: Player -> Picture
renderPlayer player = Pictures[translate xTrans yTrans $ (drawPlayer player), drawHitBox player]
  where xTrans = xP (getPos player)
        yTrans = yP (getPos player)

drawHitBox :: Moveable a => a -> Picture
drawHitBox a = color blue $ Line [(xP ptTopLeft, yP ptTopLeft), ptTopRight, (xP ptBottomRight, yP ptBottomRight), ptBottomLeft, (xP ptTopLeft, yP ptTopLeft)]
  where ptTopLeft       = topLeft (getHitbox a)
        ptTopRight      = (xP ptBottomRight, yP ptTopLeft)
        ptBottomRight   = bottomRight (getHitbox a)
        ptBottomLeft    = (xP ptTopLeft, yP ptBottomRight)

renderProjectiles :: [Projectile] -> [Picture]
renderProjectiles [] = []
renderProjectiles (x:xs) = Pictures[(translate projectilepositionX projectilepositionY $ drawProjectile), drawHitBox x] : renderProjectiles xs
  where projectilePosition  = getPos x
        projectilepositionX = xP projectilePosition
        projectilepositionY = yP projectilePosition

renderEnemies :: [Enemy] -> [Picture]
renderEnemies [] = []
renderEnemies (x:xs) = Pictures[(translate enemyPositionX enemyPositionY $ (drawEnemy (enemyKind x))) , drawHitBox x] : renderEnemies xs
  where enemyPosition  = getPos x
        enemyPositionX = xP enemyPosition
        enemyPositionY = yP enemyPosition

drawEnemy :: EnemyKind -> Picture
drawEnemy Firework      = enemyFireworkPicture
drawEnemy Cat           = enemyCatPicture
drawEnemy Postman       = enemyPostmanPicture
drawEnemy Car           = enemyCarPicture
drawEnemy VacuumCleaner = enemyVacuumCleanerPicture

drawPlayer :: Player -> Picture
drawPlayer player = color selectedColor $ playerPicture
  where statusColor "alive" = light (light blue)
        statusColor "hit"   = white
        statusColor "dead"  = black
        statusColor x       = red
        selectedColor       = statusColor (status player)

drawProjectile :: Picture
drawProjectile = projectilePicture
