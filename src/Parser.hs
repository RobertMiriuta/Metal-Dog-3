{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Aeson
import GenericTypes
import Data.Either
import qualified Data.ByteString.Lazy as B
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy.IO as I

--Name of the file
jsonFile :: FilePath
jsonFile = "Highscores.json"

--Reads the file
getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

--Converts the file to json
getJsonFile :: IO (Either String [Highscore])
getJsonFile = eitherDecode <$> getJSON

--Writes a json file
writeJsonFile :: [Highscore] -> IO ()
writeJsonFile scores = I.writeFile jsonFile (encodeToLazyText scores)


getIntFromScore :: Score -> Int
getIntFromScore (Score b) = b