module Day2Part2
  (sol) where

import Util

-- Day 1 - Part 1
-- Inventory Management System
sol :: IO ()
sol = do
  content <- getContents
  let boxes    = lines content
      (box1, box2) = head $ filter (differByOne) $ combinations boxes
      result     = removeEqual (box1, box2)
  putStrLn result

  where
    differByOne ([], xs)
      | length xs == 1 = True
      | otherwise    = False
    differByOne (xs, [])
      | length xs == 1 = True
      | otherwise    = False
    differByOne ((x:xs), (y:ys))
      | x == y  = differByOne (xs, ys)
      | otherwise = xs == ys     

    removeEqual ([], []) = []
    removeEqual ((x:xs), (y:ys))
      | x == y  = x : removeEqual (xs, ys)
      | otherwise = xs
