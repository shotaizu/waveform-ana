module Lib
    where

import Data.Maybe

toDoubleRecord xs = foldr (\x acc -> if x == [""] then acc else (map read x :: [Double]):acc) [] xs
overThreshold :: Ord a => a -> [[a]] -> Bool
overThreshold th (x:y:xs) = (head . tail) y >= th && (head . tail) x < th 
overThreshold th [x] = False

filterList :: ([a] -> Bool) -> [a] -> [a]
filterList p [] = []
filterList p (x:xs) = if p (x:xs) then x : filterList p xs else filterList p xs

data Point a = Point a a deriving (Show, Eq, Ord)
instance Functor Point where
  fmap f (Point x y) = Point (f x) (f y)

fstPoint (Point x y) = x
scdPoint (Point x y) = y

listToPoint :: [a] -> Maybe (Point a)
listToPoint (x:y:xs) = Just (Point x y)
listToPoint [x]  = Nothing
listToPoint [] = Nothing
toPoints :: [[a]] -> [Point a]
toPoints = mapMaybe listToPoint 
mapPoints :: (a -> b) -> [Point a] -> [Point b]
mapPoints p = map (fmap p)
calcXThreshold :: (Fractional a, Ord a) => a -> Point a -> Point a -> Maybe a
calcXThreshold th (Point x0 y0) (Point x1 y1)
  | y0 > y1 = Nothing
  | th < y0 || th > y1 = Nothing
  | y1 == y0 && y1 == th = Just ((x1 - x0) / 2.0)
  | otherwise = Just (x0 + (th - y0) * (x1 - x0) / (y1 - y0))

toXWithThresholdMaybe :: (Fractional a, Ord a) => a -> [Point a] -> [Maybe a]
toXWithThresholdMaybe th (x:y:xs) = calcXThreshold th x y : toXWithThresholdMaybe th (y:xs)
toXWithThresholdMaybe th [x] = []
toXWithThresholdMaybe th [] = []
toDiffList :: (Num a) => [a] -> [a]
toDiffList (x:y:xs) = y - x : toDiffList (y:xs)
toDiffList [x] = []
toDiffList [] = []

test = [["2.0", "1.0"], ["2.1", "1.2"], [""]]
