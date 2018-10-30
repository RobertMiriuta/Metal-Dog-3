module GameTypes where

import Player
import Enemy
import Projectile
import GenericTypes
import Graphics.Gloss.Interface.Pure.Game (SpecialKey)

data MetalDogGame = Game {player::Player,
                          projectiles::[Projectile],
                          enemies :: [Enemy],
                          keysPressed :: [SpecialKey],
                          currentScore::Score,
                          gameState :: Gamestate
                          --highscore::Score
                         }

data Gamestate = Playing
                |Paused
                deriving (Eq,Show)
