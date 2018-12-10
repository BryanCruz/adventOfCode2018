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

getResults content = do
  let points         = map parsePoint $ lines content
      inferiorLimitP = Point (minimumX points) (minimumY points)
      superiorLimitP = Point (maximumX points) (maximumY points)
      result1 = show 1
      result2 = show 2

  (result1, result2)

manhattanDistance :: Point -> Point-> Int
manhattanDistance (Point x1 y1) (Point x2 y2)
  = abs (x1 - x2) + abs (y1 - y2)

minimumX :: [Point] -> Int
minimumX points = minimum [x | (Point x _) <- points]

minimumY :: [Point] -> Int
minimumY points = minimum [y | (Point _ y) <- points]

maximumX :: [Point] -> Int
maximumX points = maximum [x | (Point x _) <- points]

maximumY :: [Point] -> Int
maximumY points = maximum [y | (Point _ y) <- points]

-- Input points are like "152, 292"
parsePoint :: [Char] -> Point
parsePoint cs
  = Point x' y'
  where
    x  = takeWhile (/= ',')  cs
    y  = drop (2 + length x) cs
    x' = read x :: Int
    y' = read y :: Int

data Point = Point Int Int deriving (Show, Eq)

instance Num Point where
  (+) (Point x1 y1) (Point x2 y2)
    = Point (x1 + x2) (y1 + y2)

  (*) (Point x1 y1) (Point x2 y2)
    = Point (x1 * x2) (y1 * y2)

  abs (Point x y)    = Point (abs x) (abs y)

  signum (Point x y) = Point (signum x) (signum y)

  fromInteger a      = Point (fromInteger a) (fromInteger a)

  negate (Point x y) = Point (-x) (-y)

-- The Ord is implemented like this to help 
-- us to categorize the closest points to another
instance Ord Point where
  compare (Point x1 y1) (Point x2 y2)
    = compare (x1+y1) (x2+y2)
