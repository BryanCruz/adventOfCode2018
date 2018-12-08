module Day2Part1
  (sol) where

import Util

-- Day 2 - Part 1
-- Inventory Management System
sol :: IO ()
sol = do
  content <- getContents
  let boxes  = lines content
      aux  = [count $ quicksort box | box <- boxes]
      aux2   = length $ filter (2 `elem`) aux
      aux3   = length $ filter (3 `elem`) aux
      result = aux2 * aux3
  putStrLn $ show result

  where
    count :: [Char] -> [Integer]
    count []   = []
    count (x:xs) = (countX+1) : (count remainingList)
      where
        (countX, remainingList) = count' x xs

    count' :: Char -> [Char] -> (Integer, [Char])
    count' _ []    = (0, [])
    count' y (x:xs)
      | y == x  = (1+nextCount, remainingList)
      | otherwise = (0, (x:xs))
      where
        (nextCount, remainingList) = count' x xs
