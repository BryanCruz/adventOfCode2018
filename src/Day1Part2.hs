module Day1Part2
  (sol) where

import Util
import qualified Data.Set as Set

-- Day 1 - Part 2
-- Chronal Calibration
sol :: IO ()
sol = do
  content <- getContents
  let numbers = getNumbers content
      result  = repeatedFrequency Set.empty (concat $ repeat numbers) 0
  putStrLn $ show result

  where
  repeatedFrequency :: Set.Set Integer -> [Integer] -> Integer -> Integer
  repeatedFrequency alreadySeen numbers lastFreq
    | currFreq `Set.member` alreadySeen = currFreq
    | otherwise                         = repeatedFrequency nextAlreadySeen nextNumbers currFreq
    where
      currValue       = head numbers
      currFreq        = lastFreq + currValue
      nextNumbers     = tail numbers
      nextAlreadySeen = Set.insert currFreq alreadySeen

