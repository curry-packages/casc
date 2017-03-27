{- |
    Module      :  Config.ReadConfig
    Description :  Read config file

    This module contains functions for reading the config file.
    The information we have to get out of the config file are the following:
    * checkList - which features are to be checked
    * corrList  - which features are to be corrected automatically
    * maxLength - maximum line length
-}
module Config.ReadConfig
  ( autoCorr
  , checkList
  , corrList
  , maxLength
  , shallCheck
  , shallCorrect
  ) where

import Directory    (doesFileExist, getHomeDirectory)
import FilePath     ((</>))
import Global
import List         (isInfixOf, isPrefixOf, isSuffixOf, last)
import Read         (readInt)
import ReadShowTerm (readQTerm)
import Unsafe       (unsafePerformIO)

import Utils        (fst4, snd4, thrd4, frth4)
import Config.Types (Check (..), cvCheck, help, configName, defaultLgth
                    ,defaultCfg, defaultAutoC)

-- |'True' if check is marked with "to check" (1) or "to correct" (2).
-- |'False' if check is marked with "don't check" (0).
shallCheck :: Check -> Bool
shallCheck = flip elem checkList

-- |'True' if check is marked with "to correct" (2).
shallCorrect :: Check -> Bool
shallCorrect = flip elem corrList

-- |Lift list of the checks and their configuration (0-2) out of IO monad
config :: [(Check, Int)]
config = unsafePerformIO $ liftIO fst4 getConfig

-- |List of Checks which have to be checked, which are all those
-- |which are marked with "to check" (1) and "to correct" (2).
checkList :: [Check]
checkList = map fst $ filter (\p -> snd p `elem` [1,2]) config

-- |List of Checks which have to be autocorrected,
-- |which are all those which are marked with "to correct" (2).
corrList :: [Check]
corrList = map fst $ filter (\p -> snd p == 2) config

-- |Lift the number for the maximum line length out of IO monad
maxLength :: Int
maxLength = unsafePerformIO $ liftIO snd4 getConfig

-- |Lift the string with the autocorrection parameter out of IO monad
autoCorr :: String
autoCorr = unsafePerformIO $ liftIO frth4 getConfig

--- Global variable to store the values of the configuration file
--- in order to read the file only once.
currConfig :: Global (Maybe ([(Check, Int)], Int, String, String))
currConfig = global Nothing Temporary

getConfig :: IO ([(Check, Int)], Int, String, String)
getConfig = do
  readGlobal currConfig >>= maybe readConfigFile return

-- |Read config file and return (and store globally) relevant informations
-- |which are a list of checks and their configuration (0-2),
-- |the maximum line length, the path for the pakcs libraries
-- |and the parameter for autocorrection.
readConfigFile :: IO ([(Check, Int)], Int, String, String)
readConfigFile = do
  hasLocalConfigFile <- doesFileExist configName
  configFile <- if hasLocalConfigFile
                  then return configName
                  else getHomeDirectory >>= return . (</> configName)
  cfgExists <- doesFileExist configFile
  currconf <- if cfgExists
                then do srcL <- readFile configFile >>= return . lines
                        return (readCfg srcL, readMaxLength srcL,
                                "", readAutoCorrect srcL)
                else return (defaultCfg, defaultLgth, "", defaultAutoC)
  writeGlobal currConfig (Just currconf)
  return currconf

-- |Parse config file line for line
-- |and return a list of checks and their configuration.
-- |If there is a parse error, show help.
readCfg :: [String] -> [(Check, Int)]
readCfg                      [] = []
readCfg (c:cs)
  | null                      c = readCfg cs
  | isPrefixOf "--"           c = readCfg cs
  | isPrefixOf "maxLength"    c = readCfg cs
  | isPrefixOf "autoCorrect"  c = readCfg cs
  | isSuffixOf "0"            c = (cvCheck c, 0) : readCfg cs
  | isSuffixOf "1"            c = (cvCheck c, 1) : readCfg cs
  | isSuffixOf "2"            c = (cvCheck c, 2) : readCfg cs
  | otherwise                   = error help

-- |Read parameter maxLength by taking last element of that line.
readMaxLength :: [String] -> Int
readMaxLength [] = defaultLgth
readMaxLength (c:cs)
  | isPrefixOf "maxLength" c = readInt $ last $ words c
  | otherwise                = readMaxLength cs

-- |Read parameter autoCorrect by taking last word of that line
readAutoCorrect :: [String] -> String
readAutoCorrect [] = defaultAutoC
readAutoCorrect (c:cs)
  | isPrefixOf "autoCorrect" c = last $ words c
  | otherwise                  = readAutoCorrect cs
