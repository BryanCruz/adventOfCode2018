module Day1Part1
  (sol) where

import Util

-- Day 1 - Part 1
-- Chronal Calibration
sol :: IO ()
sol = do
  content <- getContents
  let numbers = getNumbers content
      result  = sum numbers
  putStrLn $ show result
