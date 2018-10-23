-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Datatypes

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> MetalDogGame -> IO MetalDogGame
step time (Game player listOfProjectiles listOfEnemies) = return (Game player (moveProjectiles time listOfProjectiles) (moveEnemies time listOfEnemies))

-- | Handle user input
input :: Event -> MetalDogGame -> IO MetalDogGame
input e gstate = return (inputKey e gstate)

inputKey :: Event -> MetalDogGame -> MetalDogGame
inputKey (EventKey (SpecialKey key) Down _ _) game@(Game player listOfProjectiles listOfEnemies) = case key of
        -- Handles the events for the arrow keys
        KeySpace -> fireBullet game
        KeyUp    -> (Game (movePlayer player (0.0, 1.0)) listOfProjectiles listOfEnemies)
        KeyDown  -> (Game (movePlayer player (0.0, (-1.0))) listOfProjectiles listOfEnemies)
        KeyLeft  -> (Game (movePlayer player ((-1.0), 0.0)) listOfProjectiles listOfEnemies)
        KeyRight -> (Game (movePlayer player (1.0, 0.0)) listOfProjectiles listOfEnemies)
        _ -> game
inputKey _ game = game -- Otherwise keep the same

