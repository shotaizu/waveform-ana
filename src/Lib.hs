module Lib
    ( someFunc
    , toDoubleRecord
    , toDiffList
    , overThreshold
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
toDoubleRecord xs = foldr (\x acc -> if x == [""] then acc else (map read x :: [Double]):acc) [] xs
overThreshold :: Ord a => a -> [[a]] -> Bool
overThreshold th (x:y:xs) = (head $ tail y) >= th && (head $ tail x) < th 
overThreshold th (x:[]) = False

filterList :: ([a] -> Bool) -> [a] -> [a]
filterList p [] = []
filterList p (x:xs) = if p (x:xs) then x:(filterList p xs) else filterList p xs
toDiffList :: Num a => [[a]] -> [a]
toDiffList (x:[]) = []
toDiffList (x:xs) = ((head $ head xs) - (head x)):(getDiffList xs)

data Sample = Sample a a
timeSample :: (Num a) => Sample a -> a
timeSample (Sample x y) = x

