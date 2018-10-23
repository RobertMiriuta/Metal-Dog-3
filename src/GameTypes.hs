module GameTypes where

import Player
import Enemy
import Projectile
import Graphics.Gloss.Interface.Pure.Game (SpecialKey)

data MetalDogGame = Game {player::Player,
                          projectiles::[Projectile],
                          enemies :: [Enemy],
                          keysPressed :: [SpecialKey]
                          --currentScore::Score,
                          --highscore::Score
                         }

data Gamestate = Playing
                |Paused
