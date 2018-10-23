module GameTypes where

import Player
import Enemy
import Projectile

data MetalDogGame = Game {player::Player,
                          projectiles::[Projectile],
                          enemies :: [Enemy]
                          --currentScore::Score,
                          --highscore::Score
                         }

data Gamestate = Playing
                |Paused
