module Day4Part1
  (sol) where

import Util
import Date
import Debug.Trace

-- Day 4 - Part 1
-- Repose Record
sol :: IO ()
sol = do
  content <- getContents
  let result = getResult content
  putStrLn $ show result

getResult content = do
  let entries = map getEntry $ lines content
      foo     = trace "aa" "bbb" 
  entries !! 0

getEntry :: [Char] -> (Date, [Char]) 
getEntry xs = (date, info)
  where
    date = parseDate $ take 18 xs
    info  = drop 19 xs


