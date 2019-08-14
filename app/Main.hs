module Main where
import Text.CSV
import Data.Either
import Statistics.Sample
import qualified Data.Vector as V
import System.Environment


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
                ++ "mean: " ++ (show $ mean vec) ++ "\n"
                ++ "stdDev: " ++ (show $ stdDev vec) ++ ".\n")

dataFileName = "./data/test.csv"
getDoubleRecord xs = foldr (\x acc -> if x == [""] then acc else (map read x :: [Double]):acc) [] xs
overThreshold :: Ord a => a -> [[a]] -> Bool
overThreshold th (x:y:xs) = (head $ tail y) >= th && (head $ tail x) < th 
overThreshold th (x:[]) = False

filterList :: ([a] -> Bool) -> [a] -> [a]
filterList p [] = []
filterList p (x:xs) = if p (x:xs) then x:(filterList p xs) else filterList p xs
getDiffList :: Num a => [[a]] -> [a]
getDiffList (x:[]) = []
getDiffList (x:xs) = ((head $ head xs) - (head x)):(getDiffList xs)
