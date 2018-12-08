module Util 
 (getInt, 
  getNumbers,
  quicksort,
  combinations) 
  where

getInt :: [Char] -> Integer
getInt ('-':xs) = -(getInt xs)
getInt ('+':xs) = getInt xs
getInt    xs  = read xs :: Integer

getNumbers content = map getInt (lines content)

quicksort :: Ord a => [a] -> [a]
quicksort []   = []
quicksort (x:xs) = minors ++ [x] ++ majors
  where
    minors = quicksort [a | a <- xs, a <= x]
    majors = quicksort [a | a <- xs, a  > x]

combinations :: [a] -> [(a, a)]
combinations []   = []
combinations (x:xs) = [(x, y) | y <- xs] ++ (combinations xs)
