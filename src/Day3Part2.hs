module Day3Part2
  (sol) where

import Util

-- Day 1 - Part 1
-- No Matter How You Slice It
sol :: IO ()
sol = do
  content <- getContents
  let result = getResult content
  putStrLn $ show result

type Point     = (Integer, Integer)
type Rectangle = (Point, Point)
type Query     = (Integer, Point, Point)

emptySpace = repeat $ [0,0..]

getResult content = do
  let queries       = map getQuery $ lines content
      allOverlaps   = [[query1 | query2 <- queries, overlap query1 query2] | query1 <- queries]
      (result, _,_) = head $ head $ filter (\xs -> length xs <= 1) allOverlaps
  result

overlap :: Query -> Query -> Bool
overlap q1 q2 = overlap' r1 r2
  where 
    r1 = queryToRectangle q1
    r2 = queryToRectangle q2

    overlap' :: Rectangle -> Rectangle -> Bool
    overlap' ((xi1, yi1), (xf1, yf1)) ((xi2, yi2), (xf2, yf2))
      = (overlap'' (xi1, xf1) (xi2, xf2)) && (overlap'' (yi1, yf1) (yi2, yf2))
    

    -- xmin         xmax
    -- |--------------|
    --          |---------|
    --         ymin      ymax
    overlap'' (xmin, xmax) (ymin, ymax) 
      = xmin < ymax && ymin < xmax

-- 1min < 2max && 2min < 1max

queryToRectangle :: Query -> Rectangle
queryToRectangle q = do
  let (_, p0, d) = q
      (x0, y0)   = p0
      (dx, dy)   = d
      pf         = (x0+dx, y0+dy) 
  (p0, pf)

getQuery :: [Char] -> Query
getQuery xs = (id, margin, dimensions)
  where
    aux        = getIntegers xs
    id         =  aux !! 0
    margin     = (aux !! 1, aux !! 2)
    dimensions = (aux !! 3, aux !! 4)

getIntegers xs = map getInt $ sliceIntegers xs

sliceIntegers :: [Char] -> [[Char]]
sliceIntegers []     = []
sliceIntegers (x:xs)
  | isInteger x = currInteger : sliceIntegers remaining
  | otherwise   = sliceIntegers xs 
  where
    (currInteger, remaining) = sliceIntegers' [x] xs
    sliceIntegers' ys [] = (ys, [])
    sliceIntegers' ys (z:zs)
      | isInteger z = sliceIntegers' (ys++[z]) zs
      | otherwise   = (ys, (z:zs))

isInteger :: Char -> Bool
isInteger x = x >= '0' && x <= '9'
