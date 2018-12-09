module Day5
  (sol) where

import Util
import Data.Char

-- Day 5 - Part 1 and 2
-- Alchemical Reduction
sol :: IO ()
sol = do
  content <- getContents
  let (result1, result2) = getResults content
  putStrLn result1
  putStrLn result2

getResults content = do
  let polymer        = head $ lines content
      result1        = show $ length $ maxReduce polymer
      result2        = show $ minimum [length $ maxReduce $ removeChar c polymer | c <- ['a'..'z']]
  (result1, result2)

reduce :: [Char] -> [Char]
reduce []       = []
reduce [x]      = [x]
reduce (x:y:ys)
  | isMatch x y = reduce ys
  | otherwise   = x : reduce (y:ys)
  where
    isMatch  x y = (toUpper x == toUpper y) && opposite x y
    opposite x y = ((isLower x) && (isUpper y)) || ((isLower y) && (isUpper x))

maxReduce :: [Char] -> [Char]
maxReduce xs
  | length ys == length zs = zs
  | otherwise              = maxReduce zs
  where
    ys = reduce xs
    zs = reduce ys

removeChar :: Char -> [Char] -> [Char]
removeChar _ []            = []
removeChar c (x:xs)
  | toUpper c == toUpper x =     removeChar c xs
  | otherwise              = x : removeChar c xs
