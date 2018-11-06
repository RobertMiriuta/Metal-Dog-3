module Main where

import Controller
import Model
import View
import Parser
import Config
import System.Random
import GenericTypes
import Data.Aeson

import Graphics.Gloss.Interface.IO.Game
import qualified Data.ByteString.Lazy as B
import Data.Either

main :: IO ()
main = do  ran <- getStdGen
           highscore <- getJsonFile
           putStrLn "###################################################################"
           putStrLn "INSTRUCTIONS:"
           putStrLn "Use arrow keys to move. Press 'r' to restart from the beginning."
           putStrLn "Press the space bar to shoot the bad guys! And press 'p' to pause"
           putStrLn "###################################################################"
           putStrLn "Enter your name and press ENTER to start."
           nameGiven <- getLine
           case highscore of 
            Left err  ->  playIO (InWindow "Metal Dog 3" windowSizeInt (0, 0)) -- Or FullScreen
                          black                     -- Background color
                          30                        -- Frames per second
                          (initialState ran nameGiven [])     -- Initial state
                          view                      -- View function
                          input                     -- Event function
                          step                      -- Step function
            Right ps  ->  playIO (InWindow "Metal Dog 3" windowSizeInt (0, 0)) -- Or FullScreen
                          black                     -- Background color
                          30                        -- Frames per second
                          (initialState ran nameGiven ps)     -- Initial state
                          view                      -- View function
                          input                     -- Event function
                          step                      -- Step function
                 