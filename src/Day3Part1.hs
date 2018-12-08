module Day3Part1
  (sol) where

import Util

-- Day 1 - Part 1
-- No Matter How You Slice It
sol :: IO ()
sol = do
  content <- getContents
  let result = getResult content
  putStrLn $ show result

type Point     = (Integer, Integer)
type Rectangle = (Point, Point)
type Query     = (Integer, Point, Point)

maxSize    = 1100
emptySpace = repeat $ [0,0..]

getResult content = do
  let queries    = map getQuery $ lines content 
      finalSpace = foldl fillSpace emptySpace queries
      count      = sum [1 | xs <- (take maxSize) finalSpace, x <- (take maxSize xs), x >= 2]
  show count

getQuery :: [Char] -> Query
getQuery xs = (id, margin, dimensions)
  where
    aux        = getIntegers xs
    id         =  aux !! 0
    margin     = (aux !! 1, aux !! 2)
    dimensions = (aux !! 3, aux !! 4)

fillSpace :: [[Integer]] -> Query -> [[Integer]]
fillSpace s q = do
  let (_, m, d) = q
      (mx, my)  = m
      (dx, dy)  = d
      part1     = take' mx s
      part2     = take' dx part3'
      part3     = drop' dx part3'
      part3'    = drop' mx s
      result    = part1 ++ [updateColumn column | column <- part2] ++ part3
        where
          updateColumn c = do
            let part1  = take' my c
                part2  = map (+1) $ take' dy part3'
                part3  = drop' dy part3'
                part3' = drop' my c
            part1 ++ part2 ++ part3

  result

getIntegers xs = map getInt $ sliceIntegers xs

sliceIntegers :: [Char] -> [[Char]]
sliceIntegers []     = []
sliceIntegers (x:xs)
  | isInteger x = currInteger : sliceIntegers remaining
  | otherwise   = sliceIntegers xs 
  where
    (currInteger, remaining) = sliceIntegers' [x] xs
    sliceIntegers' ys [] = (ys, [])
    sliceIntegers' ys (z:zs)
      | isInteger z = sliceIntegers' (ys++[z]) zs
      | otherwise   = (ys, (z:zs))

isInteger :: Char -> Bool
isInteger x = x >= '0' && x <= '9'


drop' :: Integer -> [a] -> [a]
drop' x xs = drop (fromInteger x) xs

take' :: Integer -> [a] -> [a]
take' x xs = take (fromInteger x) xs
