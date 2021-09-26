-------------------------------
  -- Module: DataProcess
  -- Author: S. Izumiyama
  -- Date; Thu Sep 16 07:25:52 PM JST 2021
  -- Descriptions:
  --   This module collects functions to handle oscilloscope data in general
------------------------------
module DataProcess
  where

import Data.List
import TektronixFormat



class Smoothable a where 
  smoothing :: Int -> [a] -> [a]
  smoothing n xs = xs


smoothingDataPoint :: Int -> CurveBuffer DataPoint -> CurveBuffer DataPoint
smoothingDataPoint n (CurveBuffer ds) = CurveBuffer $ averagingInTime' n ds

mean xs = sum xs / fromIntegral (length xs)


-- This implementation is very slow might be O(N^{2 or more})
-- due to length calculation and drop
takeListAround pos n xs
  | l <= pos = []
  | otherwise = take (2*n+1) $ drop (pos - n) (header ++ xs ++ trailer)
    where l = length xs
          header  = if pos < n
                       then drop 1 $ [x | let x = head xs, i <- [0.. (n - pos)]] 
                       else []
          trailer = if l <= pos + n 
                       then drop 1 $ [x | let x = last xs, i <- [0.. (n + pos - l + 2)]]
                       else []

-- This is slow since it uses takeListAround
averagingInTime :: (Fractional a) => Int -> [a] -> [a]
averagingInTime n xs = map snd comb
  where
    comb = [(x,m) | i <- [0..(length xs - 1)]
                  , let x = xs !! i
                  , let m = mean $ takeListAround i n xs
           ]

-- This implementation is focused on only DataPoint type to make faster
averagingInTime' :: Int -> [DataPoint] -> [DataPoint]
averagingInTime' n ds = result
  where
    header = replicate n (head ds)
    trailer = replicate n (last ds)
    add' :: Double -> DataPoint -> Double
    add' s (_,y) = s + y
    sum' :: [DataPoint] -> Double
    sum' = foldl add' 0
    mean' l = sum' l / fromIntegral (length l)
    takeListAround' pos n xs = take (2*n+1) $ drop (pos - n) xs
    avg i (d:ds) = if i >= 2*n + 1
                    then (fst $ (d:ds) !! n, mean' $ takeListAround' n n (d:ds)) : avg (i - 1) ds
                    else []
    avg i [] = []
    result = avg (2*n + length ds) (header ++ ds ++ trailer)

