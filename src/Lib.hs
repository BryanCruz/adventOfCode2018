module Lib
    ( chronalCalibration1,
      chronalCalibration2
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

-- Util
getInt :: [Char] -> Integer
getInt ('-':xs) = -(getInt xs)
getInt ('+':xs) = getInt xs
getInt      xs  = read xs :: Integer

getNumbers content = map getInt (lines content)

