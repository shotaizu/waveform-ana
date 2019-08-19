module Main where
import Text.CSV
import Data.Either
import Statistics.Sample as S
import qualified Data.Vector as V
import System.Environment

import Lib

main :: IO ()
main = do
  args <- getArgs
  progname  <- getProgName
  let dataFileName' = if args == [] then dataFileName else head args
  d <- parseCSVFromFile dataFileName'
  let ds = head $ rights $ d:[]
      difflist = getDiffList $ filterList (overThreshold 1.8) $ getDoubleRecord ds
      vec = V.fromList difflist
  putStrLn ("This is " ++ progname ++ "\n"
                ++ "File: " ++ dataFileName' ++ "\n"
                ++ "sample num: " ++ (show $ V.length vec) ++ "\n"
                ++ "mean: " ++ (show $ S.mean vec) ++ "\n"
                ++ "stdDev: " ++ (show $ S.stdDev vec) ++ ".\n")

dataFileName = "./data/test.csv"
