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
import TektronixFormat

main :: IO ()
main = do
  args <- getArgs
  progname  <- getProgName
  if length args < 4
    then
      putStrLn  (  progname ++ "\n"
                ++ "Usage: \n"
                ++ "\t$ " ++ progname ++ " {threshold1} {data1} {threshold2} {data2}\n"
                )
    else do
      let
        dataFileName1 = args !! 1
        dataFileName2 = args !! 3
        threshold1 = head args
        threshold2 = args !! 2
      flagDoesFile1 <- doesFileExist dataFileName1
      flagDoesFile2 <- doesFileExist dataFileName2
      dataFileNames <- case flagDoesFile1 && flagDoesFile2 of
        true  -> return [dataFileName1, dataFileName2]
        false -> return []
      --print dataFileNames
      f1 <- readTektronixFile (head dataFileNames)
      f2 <- readTektronixFile (dataFileNames !! 1)
      let
        xpoints1 = flatInTime $ findCrossPoints (read threshold1) ((takeTimeVoltageCurve . parseTektronixFormat) f1)
        xpoints2 = flatInTime $ findCrossPoints (read threshold2) ((takeTimeVoltageCurve . parseTektronixFormat) f2)
      --print xpoints1
      --print xpoints2
      print $ compCenterEdge xpoints1 xpoints2
        --difflist' = toDiffList $ catMaybes $ toXWithThresholdMaybe threshold $ (mapPoints read $ toPoints ds :: [Point Double])
      --putStrLn  (  "This is " ++ progname ++ "\n"
      --          ++ "File: " ++ concat dataFileNames ++ "\n"
      --          ++ "sample num: " ++ (show $ V.length vec) ++ "\n"
      --          ++ "mean: " ++ (show $ mean vec) ++ "\n"
      --          ++ "stdDev: " ++ (show $ stdDev vec) ++ ".\n"
      --          ++ "min: " ++ (show $ fst $ minMax vec) ++ ".\n"
      --          ++ "max: " ++ (show $ snd $ minMax vec) ++ ".\n"
      --          )
  
-- data FilePathType = File | Directory | FNothing

