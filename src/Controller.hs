-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import GameTypes
import GenericTypes
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List

-- | Handle one iteration of the game
step :: Float -> MetalDogGame -> IO MetalDogGame
step time game
  |isPlaying   = return updatedGame
  |otherwise   = return game
    where isPlaying                       = (gameState game) == Playing
          listOfEnemies                   = enemies game
          listOfProjectiles               = projectiles game
          currentPlayer                   = player game
          allPressedKeys                  = keysPressed game
          newProjectiles                  = fireBullet currentPlayer allPressedKeys
          movedPlayer                     = movePlayer currentPlayer allPressedKeys
          movedEnemies                    = moveEnemies time listOfEnemies
          movedProjectiles                = moveProjectiles time listOfProjectiles
          remainingObjects                = didEnemyGetHit movedProjectiles movedEnemies
          remaningProjectiles             = (fst remainingObjects) ++ newProjectiles
          remainingEnemiesAfterKills      = snd remainingObjects
          damagedPlayerEnemies            = didPlayerGetHit remainingEnemiesAfterKills movedPlayer
          remainingEnemiesAfterCollision  = fst damagedPlayerEnemies
          remainingPlayer                 = snd damagedPlayerEnemies
          deadEnemies                     = movedEnemies \\ remainingEnemiesAfterKills
          generatedEnemies                = fst enemySeedList
          generatedSeed                   = snd enemySeedList
          enemySeedList                   = generateEnemy (seed game) remainingEnemiesAfterCollision
          updatedEnemyList                = remainingEnemiesAfterCollision ++ generatedEnemies
          updatedScore                    = (currentScore game) `additionScore` (getReward deadEnemies)
          updatedGame                     = game {player = remainingPlayer, enemies = updatedEnemyList, projectiles = remaningProjectiles, currentScore = updatedScore, seed = generatedSeed}

-- | Handle user input
input :: Event -> MetalDogGame -> IO MetalDogGame
input e gstate = return (inputKey e gstate)

inputKey :: Event -> MetalDogGame -> MetalDogGame
inputKey (EventKey (SpecialKey key) Down _ _) game = case key of
        -- Handles the events for the arrow keys
        KeySpace -> game {keysPressed = newKeys}
        KeyUp    -> game {keysPressed = newKeys}
        KeyDown  -> game {keysPressed = newKeys}
        KeyLeft  -> game {keysPressed = newKeys}
        KeyRight -> game {keysPressed = newKeys}
        _ -> game
        where originalKeys = keysPressed game
              newKeys = key : originalKeys
inputKey (EventKey (SpecialKey key) Up _ _) game = case key of
        -- Handles the events for the arrow keys
        KeySpace -> game {keysPressed = removeSpace}
        KeyUp    -> game {keysPressed = removeUp}
        KeyDown  -> game {keysPressed = removeDown}
        KeyLeft  -> game {keysPressed = removeLeft}
        KeyRight -> game {keysPressed = removeRight}
        _ -> game
        where originalKeys = keysPressed game
              newKeys a   = filter (\x -> x /= a) originalKeys
              removeSpace = newKeys KeySpace
              removeUp    = newKeys KeyUp
              removeDown  = newKeys KeyDown
              removeLeft  = newKeys KeyLeft
              removeRight = newKeys KeyRight
inputKey (EventKey (Char 'p') Down _ _) game
  |isPlaying = game {gameState = Paused}
  |otherwise = game {gameState = Playing}
    where isPlaying = (gameState game) == Playing
inputKey (EventKey (Char 'r') Down _ _) game = initialState (seed game) --Resets the game
inputKey _ game = game -- Otherwise keep the same
