module GameTypes where

import Player
import Enemy
import Projectile
import System.Random
import GenericTypes
import Graphics.Gloss.Interface.Pure.Game (SpecialKey)

data MetalDogGame = Game {player::Player,
                          projectiles::[Projectile],
                          enemies :: [Enemy],
                          keysPressed :: [SpecialKey],
                          --currentScore::Score,
                          seed::StdGen
                          --highscore::Score
                         }

data Gamestate = Playing
                |Paused
