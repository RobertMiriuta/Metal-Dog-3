-- This module defines how to turn
-- the game state into a picture
module View where

import Graphics.Gloss
import Model
import GameTypes
import GenericTypes
import Config
import Player
import Projectile
import Enemy
import Particle

view :: MetalDogGame -> IO Picture
view = return . viewPure

viewPure :: MetalDogGame -> Picture
viewPure game
    |isPlaying  = pics
    |isGameOver = picsGameOver
    |otherwise  = picsPaused
  where isPlaying           = (gameState game) == Playing
        currentPlayer       = player game
        isGameOver          = (gameState game) == GameOver
        halfSizeX           = (fst windowSizeFloat) / 2
        halfSizeY           = (snd windowSizeFloat) / 2
        listOfProjectiles   = projectiles game
        listOfEnemies       = enemies game
        listOfParticles     = particles game
        renderedplayerShip  = renderPlayer currentPlayer
        renderedprojectiles = renderProjectiles listOfProjectiles
        renderedParticles   = renderParticles listOfParticles
        scr                 = currentScore game
        renderedenemies     = renderEnemies listOfEnemies
        activeArea          = renderActiveArea
        scorePic            = scale 0.15 0.15.color orange.text $ show scr
        score               = translate (halfSizeX-200) ((-halfSizeY)+20) scorePic
        pausePic            = scale 0.5 0.5.color orange.text $ show Paused
        pause               = translate (-halfSizeX) (halfSizeY-60) pausePic
        gameOverPic         = scale 1.0 1.0.color orange.text $ show GameOver
        gameover            = translate (-halfSizeX) (-50.0) $ gameOverPic
        pics                = pictures ([renderedplayerShip] ++ renderedprojectiles ++ renderedenemies ++ renderedParticles ++ [activeArea] ++ [score])
        picsPaused          = pictures ([renderedplayerShip] ++ renderedprojectiles ++ renderedenemies ++ renderedParticles ++ [activeArea] ++ [score] ++ [pause])
        picsGameOver        = pictures ([gameover] ++ [score])

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

-- used for programming purposes, won't be used in final release
drawHitBox :: Moveable a => a -> Picture
drawHitBox a = color blue $ Line [(xP ptTopLeft, yP ptTopLeft), ptTopRight, (xP ptBottomRight, yP ptBottomRight), ptBottomLeft, (xP ptTopLeft, yP ptTopLeft)]
  where ptTopLeft       = topLeft (getHitbox a)
        ptTopRight      = (xP ptBottomRight, yP ptTopLeft)
        ptBottomRight   = bottomRight (getHitbox a)
        ptBottomLeft    = (xP ptTopLeft, yP ptBottomRight)

renderParticles :: [Particle] -> [Picture]
renderParticles [] = []
renderParticles (x:xs) = (translate particlepositionX particlepositionY $ renderedParticle) : renderParticles xs
    where particleposition  = Particle.position x
          particlepositionX = xP particleposition
          particlepositionY = yP particleposition
          particleAge       = Particle.age x
          renderedParticle  = drawParticle particleAge x

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

-- returns the picture for every given enemy
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

drawParticle :: Float -> Particle -> Picture
drawParticle age particle = color (colorAdjust (alpha)) $ adjustedPicture
    where alpha = (Particle.age particle) / (lifespan particle)
          colorAdjust a = makeColor 1 1 0 (1-(getAlpha a))
          getAlpha a
            | alpha > 0 = alpha
            | otherwise = 0
                where alpha = (0.9 - a)
          adjustedPicture = particlePicture age
