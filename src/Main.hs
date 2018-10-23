module Main where

import Controller
import Model
import View
import Config

import Graphics.Gloss.Interface.IO.Game


main :: IO ()
main = playIO (InWindow "Metal Dog 3" windowSizeInt (0, 0)) -- Or FullScreen
              black            -- Background color
              30               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function