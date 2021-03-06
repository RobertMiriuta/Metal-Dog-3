-- This module defines how the state changes
-- in response to time and user input
module Controller where

import Model
import GameTypes
import GenericTypes
import Particle
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List
import Player

-- | Handle one iteration of the game
step :: Float -> MetalDogGame -> IO MetalDogGame
step time game
  |isDead                = gameOverLogic gameOverGame
  |isPlaying             = return updatedGame
  |otherwise             = return game
    where isPlaying                       = gameState game == Playing
          playerStatus                    = status currentPlayer
          isDead                          = playerStatus == "dead"
          listOfEnemies                   = enemies game
          listOfProjectiles               = projectiles game
          listOfParticles                 = particles game
          updatedParticles                = map (growParticle time) listOfParticles
          remainingParticles              = cleanUpParticles updatedParticles
          currentPlayer                   = player game
          updatedPlayer                   = updatedPlayerWeapon currentPlayer time
          allPressedKeys                  = keysPressed game
          newProjectilesAndPlayer         = fireBullet updatedPlayer allPressedKeys
          newProjectiles                  = fst newProjectilesAndPlayer
          newPlayer                       = snd newProjectilesAndPlayer
          movedPlayer                     = movePlayer newPlayer allPressedKeys
          movedEnemies                    = moveEnemies time listOfEnemies
          movedProjectiles                = moveProjectiles time listOfProjectiles
          allProjectiles                  = listOfProjectiles ++ newProjectiles
          newListOfParticles              = createParticles allProjectiles ++ remainingParticles
          remainingObjects                = didEnemyGetHit movedProjectiles movedEnemies
          remaningProjectiles             = fst remainingObjects ++ newProjectiles
          remainingEnemiesAfterKills      = snd remainingObjects
          damagedPlayerEnemies            = didPlayerGetHit remainingEnemiesAfterKills movedPlayer
          remainingEnemiesAfterCollision  = fst damagedPlayerEnemies
          remainingPlayer                 = snd damagedPlayerEnemies
          deadEnemies                     = movedEnemies \\ remainingEnemiesAfterKills
          generatedEnemies                = fst enemySeedList
          generatedSeed                   = snd enemySeedList
          enemySeedList                   = generateEnemy currentPlayer (seed game) remainingEnemiesAfterCollision updatedGameTime
          updatedEnemyList                = remainingEnemiesAfterCollision ++ generatedEnemies
          updatedScore                    = currentScore game `iAdd` getReward deadEnemies
          oldGameTime                     = gameTime game
          updatedGameTime                 = oldGameTime + time
          updatedGame                     = game {player = remainingPlayer, enemies = updatedEnemyList, projectiles = remaningProjectiles, currentScore = updatedScore, seed = generatedSeed, gameTime = updatedGameTime, particles = newListOfParticles}
          gameOverGame                    = game {gameState = GameOver}

-- Handle user input
input :: Event -> MetalDogGame -> IO MetalDogGame
input e gstate = return (inputKey e gstate)

-- handles key inputs for movement, shooting, pausing and restarting
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
        KeySpace -> removeKeyFromGame KeySpace game
        KeyUp    -> removeKeyFromGame KeyUp game
        KeyDown  -> removeKeyFromGame KeyDown game
        KeyLeft  -> removeKeyFromGame KeyLeft game
        KeyRight -> removeKeyFromGame KeyRight game
        _ -> game
inputKey (EventKey (Char 'p') Down _ _) game
  |isPlaying = game {gameState = Paused}
  |otherwise = game {gameState = Playing}
    where isPlaying = gameState game == Playing
inputKey (EventKey (Char 'r') Down _ _) game = initialState (seed game) (Player.name (player game)) (highscore game) --Resets the game
inputKey _ game = game -- Otherwise keep the same


removeKeyFromGame :: SpecialKey -> MetalDogGame -> MetalDogGame
removeKeyFromGame key game = game {keysPressed = updatedKeys}
  where originalKeys = keysPressed game
        newKeys a   = filter (/= a) originalKeys
        updatedKeys = newKeys key