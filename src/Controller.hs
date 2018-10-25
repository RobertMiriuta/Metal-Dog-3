-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import GameTypes
import GenericTypes
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> MetalDogGame -> IO MetalDogGame
step time game = return updatedGame
  where listOfEnemies = enemies game
        newProjectiles = fireBullet currentPlayer allPressedKeys
        listOfProjectiles = newProjectiles ++ (projectiles game)
        currentPlayer = player game
        allPressedKeys = keysPressed game
        movedPlayer = movePlayer currentPlayer allPressedKeys
        movedEnemies = moveEnemies time listOfEnemies
        movedProjectiles = moveProjectiles time listOfProjectiles
        remainingObjects = didAnyoneGetHit listOfProjectiles listOfEnemies
        updatedGame = game {player = movedPlayer, enemies = movedEnemies, projectiles = movedProjectiles}

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
              newKeys a = filter (\x -> x /= a) originalKeys
              removeSpace = newKeys KeySpace
              removeUp = newKeys KeyUp
              removeDown = newKeys KeyDown
              removeLeft = newKeys KeyLeft
              removeRight = newKeys KeyRight

inputKey _ game = game -- Otherwise keep the same

