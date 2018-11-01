module GameTypes where

import Player
import Enemy
import Projectile
import Particle
import System.Random
import GenericTypes
import Graphics.Gloss.Interface.Pure.Game (SpecialKey)

data MetalDogGame = Game {player::Player,
                          projectiles::[Projectile],
                          enemies :: [Enemy],
                          keysPressed :: [SpecialKey],
                          seed::StdGen,
                          currentScore::Score,
                          gameState :: Gamestate,
                          gameTime :: Float,
                          particles::[Particle]
                          --highscore::Score
                         }

data Gamestate = Playing
                |Paused
                |GameOver
                deriving (Eq,Show)
