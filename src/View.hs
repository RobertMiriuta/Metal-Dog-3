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
import Data.List

view :: MetalDogGame -> IO Picture
view = return . viewPure

viewPure :: MetalDogGame -> Picture
viewPure game
    |isPlaying  = pics
    |isGameOver = picsGameOver
    |otherwise  = picsPaused
  where isPlaying           = gameState game == Playing
        currentPlayer       = player game
        isGameOver          = gameState game == GameOver
        halfSizeX           = fst windowSizeFloat/2
        halfSizeY           = snd windowSizeFloat/2
        listOfProjectiles   = projectiles game
        listOfEnemies       = enemies game
        listOfParticles     = particles game
        renderedplayerShip  = renderPlayer currentPlayer
        renderedprojectiles = renderProjectiles listOfProjectiles
        renderedParticles   = renderParticles listOfParticles
        playerscore         = currentScore game
        renderedenemies     = renderEnemies listOfEnemies
        scorePic            = scale 0.15 0.15.color orange.text $ show playerscore
        translatedScorePic  = translate (halfSizeX-200) ((-halfSizeY)+20) scorePic
        pausePic            = scale 0.5 0.5.color orange.text $ show Paused
        translatedPausePic  = translate (-halfSizeX) (halfSizeY-60) pausePic
        gameOverPic         = scale 1.0 1.0.color orange.text $ show GameOver
        gameover            = translate (-halfSizeX) (-50.0) gameOverPic 
        scoreboard          = drawScoreboard (highscore game)
        pics                = pictures ([renderedplayerShip] ++ renderedprojectiles ++ renderedenemies ++ renderedParticles ++ [translatedScorePic])
        picsPaused          = pictures ([renderedplayerShip] ++ renderedprojectiles ++ renderedenemies ++ renderedParticles ++ [translatedScorePic] ++ [translatedPausePic])
        picsGameOver        = pictures ([gameover] ++ [translatedScorePic] ++ [scoreboard])

renderPlayer :: Player -> Picture
renderPlayer player = translate xTrans yTrans $ drawPlayer player
  where xTrans = xP (getPos player)
        yTrans = yP (getPos player)

drawScoreboard :: [Highscore] -> Picture
drawScoreboard []           = scale 1.0 1.0.color orange.text $ show "nothing"
drawScoreboard [x]          = singleElement x
drawScoreboard [x,y]        = twoElement x y
drawScoreboard (x:y:z:xs)   = threeElement x y z
 
threeElement :: Highscore -> Highscore -> Highscore -> Picture 
threeElement a b c = pictures [translate (-50) 50.0 $ scale 0.20 0.20 . color orange . text $ show ("3." ++ GenericTypes.name c ++ " - " ++ show (score c)), twoElement a b ]

twoElement :: Highscore -> Highscore -> Picture
twoElement a b = pictures [translate (-50) 75.0 $ scale 0.20 0.20 . color orange . text $ show ("2." ++ GenericTypes.name b ++ " - " ++ show (score b)), singleElement a]

singleElement :: Highscore -> Picture
singleElement a = translate (-50) 100.0 $ scale 0.20 0.20 . color orange . text $ show ("1." ++ GenericTypes.name a ++ " - " ++ show (score a))

renderParticles :: [Particle] -> [Picture]
renderParticles [] = []
renderParticles (x:xs) = translate particlepositionX particlepositionY renderedParticle : renderParticles xs
    where particleposition  = Particle.position x
          particlepositionX = xP particleposition
          particlepositionY = yP particleposition
          particleAge       = Particle.age x
          renderedParticle  = drawParticle particleAge x

renderProjectiles :: [Projectile] -> [Picture]
renderProjectiles [] = []
renderProjectiles (x:xs) = translate projectilepositionX projectilepositionY drawProjectile : renderProjectiles xs
  where projectilePosition  = getPos x
        projectilepositionX = xP projectilePosition
        projectilepositionY = yP projectilePosition

renderEnemies :: [Enemy] -> [Picture]
renderEnemies [] = []
renderEnemies (x:xs) = translate enemyPositionX enemyPositionY (drawEnemy (enemyKind x)) : renderEnemies xs
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
drawPlayer player = color selectedColor playerPicture
  where statusColor "alive" = light (light blue)
        statusColor "hit"   = white
        statusColor "dead"  = black
        statusColor x       = red
        selectedColor       = statusColor (status player)

drawProjectile :: Picture
drawProjectile = projectilePicture

drawParticle :: Float -> Particle -> Picture
drawParticle age particle   = color (colorAdjust alpha) adjustedPicture
    where alpha           = Particle.age particle / lifespan particle
          colorAdjust a   = makeColor 1 1 0 (1-getAlpha a)
          getAlpha a
            | alpha > 0 = alpha
            | otherwise = 0
                where alpha = 0.9 - a
          adjustedPicture = particlePicture age
