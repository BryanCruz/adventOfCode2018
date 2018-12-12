module Day6
  (sol) where

import Util
import qualified Data.HashMap.Lazy as Map

-- Day 6 - Part 1 and 2
-- Chronal Coordinates
sol :: IO ()
sol = do
  content <- getContents
  let (result1, result2) = getResults content
  putStrLn result1
  putStrLn result2

type Point = (Int,Int)

sumPoints :: Point -> Point -> Point
sumPoints (x1, y1) (x2, y2)
  = (x1+x2, y1+y2)

-- Input points are like "152, 292"
parsePoint :: [Char] -> Point
parsePoint cs
  = (x', y')
  where
    x  = takeWhile (/= ',')  cs
    y  = drop (2 + length x) cs
    x' = read x :: Int
    y' = read y :: Int


getResults content = do
  let points  = map parsePoint $ lines content
      result1 = show 1
      result2 = show 2

  (result1, result2)

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2)
  = abs (x1 - x2) + abs (y1 - y2)

minimumX :: [Point] -> Int
minimumX points = minimum [x | (x, _) <- points]

minimumY :: [Point] -> Int
minimumY points = minimum [y | (_, y) <- points]

maximumX :: [Point] -> Int
maximumX points = maximum [x | (x, _) <- points]

maximumY :: [Point] -> Int
maximumY points = maximum [y | (_, y) <- points]

