module Day4Part1
  (sol) where

import Util
import Date
import Debug.Trace
import qualified Data.Map as Map

-- Day 4 - Part 1
-- Repose Record
sol :: IO ()
sol = do
  content <- getContents
  let result = getResult content
  putStrLn $ show result

data Entry = Entry { date :: Date, info :: Info Int} deriving (Show, Read)

data Info s = BeginsShift s | Sleep | WakesUp deriving (Show, Read)

instance Eq Entry where
  (==) e1 e2 
    = (==) (date e1) (date e2)

instance Ord Entry where
  compare e1 e2
    = compare (date e1) (date e2)

getResult content = do
  let entries = quicksort $ map getEntry $ lines content
  entries

getEntry :: [Char] -> Entry
getEntry xs = Entry date info
  where
    date    = parseDate $ take 18 xs
    info    = parseInfo $ drop 19 xs

parseInfo :: [Char] -> Info Int
parseInfo "wakes up"     = WakesUp
parseInfo "falls asleep" = Sleep
parseInfo ss             = BeginsShift guard
  where
    guard               = read (nextInt ss) :: Int
    nextInt (' ':'b':_) = []
    nextInt (x:xs)
      | isCharInt x     = x : nextInt xs
      | otherwise       = nextInt xs
    isCharInt x         = x >= '0' && x <= '9'
