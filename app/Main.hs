module Main where
import Text.CSV
import Data.Either
import Statistics.Sample
import Statistics.Function
import qualified Data.Vector as V
import System.Environment
import Data.Maybe

import Lib

main :: IO ()
main = do
  args <- getArgs
  progname  <- getProgName
  let dataFileName' = if args == [] then dataFileName else head args
  let threshold = if length args < 2 then 1.0 else read $ head $ tail args
  d <- parseCSVFromFile dataFileName'
  let ds = head $ rights $ d:[]
      difflist = toDiffList $ catMaybes $ toXWithThresholdMaybe threshold $ (mapPoints read $ toPoints ds :: [Point Double])
      vec = V.fromList difflist
  putStrLn ("This is " ++ progname ++ "\n"
                ++ "File: " ++ dataFileName' ++ "\n"
                ++ "sample num: " ++ (show $ V.length vec) ++ "\n"
                ++ "mean: " ++ (show $ mean vec) ++ "\n"
                ++ "stdDev: " ++ (show $ stdDev vec) ++ ".\n"
                ++ "min: " ++ (show $ fst $ minMax vec) ++ ".\n"
                ++ "max: " ++ (show $ snd $ minMax vec) ++ ".\n"
           )

dataFileName  = "./data/test.csv"
