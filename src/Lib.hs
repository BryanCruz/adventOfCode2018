module Lib
    ( chronalCalibration1,
      chronalCalibration2,
      inventoryManagementSystem1,
      inventoryManagementSystem2
    ) where

import Debug.Trace

-- Day 1
--
--     Part 1
chronalCalibration1 :: IO ()
chronalCalibration1 = do
    content <- getContents
    let numbers = getNumbers content
        result  = sum numbers
    putStrLn $ show result

--     Part 2
chronalCalibration2 :: IO ()
chronalCalibration2 = do
    content <- getContents
    let numbers = getNumbers content
        result  = repeatedFrequency [0] (concat $ repeat numbers)
    putStrLn $ show result

    where
    repeatedFrequency alreadySeen numbers
        | currFreq `elem` alreadySeen = currFreq
        | otherwise                   = repeatedFrequency nextAlreadySeen nextNumbers
        where
            currValue       = head numbers
            currFreq        = head alreadySeen + currValue
            nextNumbers     = tail numbers
            nextAlreadySeen = currFreq : alreadySeen
-- Day 2
--
--     Part 1
inventoryManagementSystem1 :: IO ()
inventoryManagementSystem1 = do
    content <- getContents
    let boxes  = lines content
        aux    = [count $ quicksort box | box <- boxes]
        aux2   = length $ filter (2 `elem`) aux
        aux3   = length $ filter (3 `elem`) aux
        result = aux2 * aux3
    putStrLn $ show result

    where
        count :: [Char] -> [Integer]
        count []     = []
        count (x:xs) = (countX+1) : (count remainingList)
            where
                (countX, remainingList) = count' x xs

        count' :: Char -> [Char] -> (Integer, [Char])
        count' _ []      = (0, [])
        count' y (x:xs)
            | y == x    = (1+nextCount, remainingList)
            | otherwise = (0, (x:xs))
            where
                (nextCount, remainingList) = count' x xs

--    Part 2
inventoryManagementSystem2 :: IO ()
inventoryManagementSystem2 = do
    content <- getContents
    let boxes        = lines content
        (box1, box2) = head $ filter (differByOne) $ combinations boxes
        result       = removeEqual (box1, box2)
    putStrLn result

    where
        differByOne ([], xs)
            | length xs == 1 = True
            | otherwise      = False
        differByOne (xs, [])
            | length xs == 1 = True
            | otherwise      = False
        differByOne ((x:xs), (y:ys))
            | x == y    = differByOne (xs, ys)
            | otherwise = xs == ys       

        removeEqual ([], []) = []
        removeEqual ((x:xs), (y:ys))
            | x == y    = x : removeEqual (xs, ys)
            | otherwise = xs

-- Util
getInt :: [Char] -> Integer
getInt ('-':xs) = -(getInt xs)
getInt ('+':xs) = getInt xs
getInt      xs  = read xs :: Integer

getNumbers content = map getInt (lines content)

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = minors ++ [x] ++ majors
    where
        minors = quicksort [a | a <- xs, a <= x]
        majors = quicksort [a | a <- xs, a  > x]

combinations :: [a] -> [(a, a)]
combinations []     = []
combinations (x:xs) = [(x, y) | y <- xs] ++ (combinations xs)
