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
  let points   = map parsePoint $ lines content

      minimumX = minimum [x | (x, _) <- points]
      minimumY = minimum [y | (_, y) <- points]
      maximumX = maximum [x | (x, _) <- points]
      maximumY = maximum [y | (_, y) <- points]
      
      sizeX    = maximumX - minimumX
      sizeY    = maximumY - minimumY

      allPoints = [(x, y) | x <- xRange, y <- yRange]
        where
          xRange = [minimumX..maximumX]
          yRange = [minimumY..maximumY]
   
      closestPoints  = [(p0, closestPoint p0) | p0 <- allPoints]
        where
          closestPoint p0 = do
            let allDistances        = [(pf, manhattanDistance p0 pf) | pf <- points]
                (potentialClosestPoint, minorDistance) = minimumSnd allDistances

            if   minorDistance `appearsOnce` [snd t | t <- allDistances]
            then Just potentialClosestPoint
            else Nothing

      -- Closest to Edge Points have infinite closest points, so we do not count them
      edgePoints          = [(x, y) | (x, y) <- allPoints, x `elem` [minimumX, maximumX] || y `elem` [minimumY, maximumY]]
      closestToEdgePoints = [pf | (p0, pf) <- closestPoints, pf /= Nothing, p0 `elem` edgePoints]
      largestArea         = [count p closestPoints | p <- points, not $ Just p `elem` closestToEdgePoints]
        where
          count a xs = length [1 | (_, x) <- xs, x == Just a]

    
      result1 = show $ maximum largestArea
      result2 = show largestArea
  (result1, result2)

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2)
  = abs (x1 - x2) + abs (y1 - y2)

minimumSnd []     = error "Empty list"
minimumSnd [t]    = t
minimumSnd (t:ts) = if v < v' then (k, v) else (k', v')
  where
    (k,   v)  = t
    (k',  v') = minimumSnd ts

appearsOnce a xs = length [x | x <- xs, x == a] == 1
