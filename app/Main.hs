module Main where
import Text.CSV
import Data.Either
import qualified Data.Vector as V
import Statistics.Function
import Statistics.Sample
import System.Directory
import System.Environment
import Data.Maybe

import Lib

main :: IO ()
main = do
  args <- getArgs
  progname  <- getProgName
  if length args < 1
    then
      putStrLn  (  progname ++ "\n"
                ++ "Usage: \n"
                ++ "\t$ " ++ progname ++ " {threshold} {data directory or file}\n"
                )
    else do
      let
        dataFileName' = if args == [] then dataFileName else head args where
          dataFileName  = "./data/test.csv"
        threshold = if length args < 2 then 1.0 else read $ head $ tail args
      flagDoesFile <- doesFileExist dataFileName'
      flagDoesDirectory <- doesDirectoryExist dataFileName'
      let
        dataFileNameType = if flagDoesFile then File else if flagDoesDirectory then Directory else FNothing
      dataFiles <- case dataFileNameType of
        File      -> return (dataFileName':[])
        Directory -> fmap (\x -> fmap (\y -> (dataFileName' ++ "/" ++ y)) x) $ listDirectory dataFileName'
        FNothing  -> return []
      print dataFiles
      d <- parseCSVFromFile dataFileName'
      let
        d' = map parseCSVFromFile dataFiles
        ds = head $ rights $ d:[]
        --ds' = map (\x -> do y <- x; return (rights y)) d'
        difflist = toDiffList $ catMaybes $ toXWithThresholdMaybe threshold $ (mapPoints read $ toPoints ds :: [Point Double])
        --difflist' = toDiffList $ catMaybes $ toXWithThresholdMaybe threshold $ (mapPoints read $ toPoints ds :: [Point Double])
        vec = V.fromList difflist
      putStrLn  (  "This is " ++ progname ++ "\n"
                ++ "File: " ++ dataFileName' ++ "\n"
                ++ "sample num: " ++ (show $ V.length vec) ++ "\n"
                ++ "mean: " ++ (show $ mean vec) ++ "\n"
                ++ "stdDev: " ++ (show $ stdDev vec) ++ ".\n"
                ++ "min: " ++ (show $ fst $ minMax vec) ++ ".\n"
                ++ "max: " ++ (show $ snd $ minMax vec) ++ ".\n"
                )
  
data FilePathType = File | Directory | FNothing

