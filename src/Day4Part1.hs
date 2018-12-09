module Day4Part1
  (sol) where

import Util
import Date
import Debug.Trace
import qualified Data.HashMap.Lazy as Map

-- Day 4 - Part 1
-- Repose Record
sol :: IO ()
sol = do
  content <- getContents
  let result = getResult content
  putStrLn $ show result

data Entry = Entry { date :: Date, info :: Info Int} deriving (Show, Read)

data Info s = BeginsShift s | Sleep | WakesUp deriving (Show, Read, Eq)

type Shift = [Entry]

instance Eq Entry where
  (==) e1 e2 
    = (==) (date e1) (date e2)

instance Ord Entry where
  compare e1 e2
    = compare (date e1) (date e2)

getResult content = do
  let entries             = quicksort $ map getEntry $ lines content
      shifts              = splitShifts entries
      guardToMinutes      = foldl insertGuardMinutes guardToMinutesEmpty shifts
      guardToMinutesList  = Map.toList guardToMinutes
      (chosenGuard, _)    = maximumTotal [(guard, mySum $ Map.toList minutesToSleepingMinutes) | (guard, minutesToSleepingMinutes) <- guardToMinutesList]
      (chosenMinute, _)   = maximumTotal $ Map.toList $ guardToMinutes Map.! chosenGuard
  chosenGuard*chosenMinute
    where
      mySum []                  = 0
      mySum ((_, n):xs)         = n + mySum xs

      maximumTotal :: [(Int, Int)] -> (Int, Int)
      maximumTotal [(guard, n)]  = (guard, n)
      maximumTotal ((currGuard, currN):xs)
        = if   currN > nextN
          then (currGuard, currN)
          else (nextGuard, nextN)
        where 
          (nextGuard, nextN) = maximumTotal xs

insertGuardMinutes :: GuardToMinutes -> Shift -> GuardToMinutes
insertGuardMinutes guardToMinutes shift
  = Map.insert guardOnShift newStateUpdated guardToMinutes
  where
    actualState     = Map.lookup guardOnShift guardToMinutes
    guardOnShift    = getGuardOnShift shift
    newState        = if   actualState == Nothing
                      then minutesToSleepingMinutesEmpty
                      else (let Just x = actualState in x)
    newStateUpdated = updateMinutes newState shift

updateMinutes :: MinutesToSleepingMinutes -> Shift -> MinutesToSleepingMinutes
updateMinutes minutesToSleepingMinutes shift
  = foldl (\map (k, v) -> Map.insertWith (+) k v map) minutesToSleepingMinutes periods
  where
    turningShifts = groupsOf 2 (tail shift)
    periods       = zip [0..59] $ foldl insertShift (take 60 [0,0..]) turningShifts
    insertShift :: [Int] -> [Entry] -> [Int]
    insertShift xs []          = xs
    insertShift xs [Entry d _] = (take startShift xs) ++ (take timeDiff [1,1..]) ++ (drop endShift xs)
      where
        startShift = minute d
        endShift   = 60
        timeDiff   = endShift - startShift
    insertShift xs ((Entry d1 _):(Entry d2 _):[]) = (take startShift xs) ++ (take timeDiff [1,1..]) ++ (drop endShift xs)
      where
        startShift = minute d1
        endShift   = minute d2
        timeDiff   = endShift - startShift
 
groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = (take n xs) : (groupsOf n $ drop n xs)

type MinutesToSleepingMinutes = Map.HashMap Int Int
type GuardToMinutes           = Map.HashMap Int MinutesToSleepingMinutes

minutesToSleepingMinutesEmpty :: Map.HashMap Int Int
minutesToSleepingMinutesEmpty = Map.fromList $ zip [0..59] [0,0..]

guardToMinutesEmpty :: GuardToMinutes
guardToMinutesEmpty = Map.empty

getGuardOnShift :: Shift -> Int
getGuardOnShift (e:es) = let Entry _ (BeginsShift n) = e in n 

splitShifts :: [Entry] -> [Shift]
splitShifts []     = []
splitShifts (e:es) = currShift : nextShifts
  where
    currShift  = e : takeWhile (\f -> let i = info f in i == Sleep || i == WakesUp) es
    nextShifts = splitShifts (drop (length currShift) (e:es))

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

-- This method assumes d2 >= d1 and all we will care 
-- in this exercise is the minute difference
timeDifference :: Date -> Date -> Int
timeDifference d1 d2 = (minute d2) - (minute d1)
