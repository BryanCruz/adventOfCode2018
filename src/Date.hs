module Date 
( Date
, parseDate
) where

import Debug.Trace

data Date = Date { year   :: Int
                 , month  :: Int
                 , day    :: Int
                 , hour   :: Int
                 , minute :: Int
                 } deriving (Read)

instance Show Date where
  show (Date year month day hour minute)
    = "[" ++ year' ++ "-" ++ month' ++ "-" ++ day' ++ " " ++ hour' ++ ":" ++ minute' ++ "]" 
    where
      year'      = trace (show (completeWith0s 1000 year)) completeWith0s 1000 year
      month'     = trace (show (completeWith0s 10   month)) completeWith0s 10   month
      day'       = trace (show (completeWith0s 10   day)) completeWith0s 10   day
      hour'      = trace (show (completeWith0s 10   hour)) completeWith0s 10   hour
      minute'    = trace (show (completeWith0s 10   minute)) completeWith0s 10   minute

      completeWith0s :: Int -> Int -> [Char] 
      completeWith0s 0 0 = ""
      completeWith0s n x
        | n < x     = show x
        | otherwise = '0' : completeWith0s (round $ (fromIntegral n)/10) x
 
parseDate :: [Char] -> Date
parseDate ('[':y1:y2:y3:y4:'-':m1:m2:'-':d1:d2:' ':h1:h2:':':min1:min2:']':[])
  = Date year month day hour minute
  where
    year   = read (y1:y2:y3:y4:[]) :: Int
    month  = read (m1:m2:[])       :: Int
    day    = read (d1:d2:[])       :: Int
    hour   = read (h1:h2:[])       :: Int
    minute = read (min1:min2:[])   :: Int
