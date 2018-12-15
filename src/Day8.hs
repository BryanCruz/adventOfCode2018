module Day8
  (sol) where

-- Day 8 - Memory Maneuver
sol :: IO ()
sol = do
  line <- getLine
  let numbers    = slice line
      (node, _)  = getNode numbers
      result1    = getResult1 node
      result2    = getResult2 node
  putStrLn result1
  putStrLn result2

slice :: [Char] -> [Int]
slice []       = []
slice (' ':xs) = slice xs
slice cs       = (read x :: Int) : slice cs'
  where
    x   = nextInt cs 
    cs' = drop (length x) cs

    nextInt [] = []
    nextInt (x:xs)
      | x >= '0' && x <= '9' = x : nextInt xs
      | otherwise            = [] 

data Node = Node {children :: [Node], metadata :: [Int]} deriving (Show)

getNode [] = error "Empty list"
getNode (nChildren:nMetadata:ns)
  = (Node ch md, ns'')
  where
    (ch, ns')  = getChildren nChildren ns
    (md, ns'') = getMetadata nMetadata ns'

getChildren 0 xs     = ([], xs)
getChildren n []     = error "Empty list"
getChildren n xs     = ((c:ch), xs'')
  where
    (c,  xs' ) = getNode xs
    (ch, xs'') = getChildren (n-1) xs'

getMetadata n xs = (take n xs, drop n xs)

-- Part 1
sumMetadata :: Node -> Int
sumMetadata (Node ch md) = sum [sumMetadata c | c <- ch] + sum md

getResult1 node = do
  show $ sumMetadata node


-- Part 2
getValue :: Node -> Int
getValue (Node ch md)
  | length ch == 0 = sum md
  | otherwise      = sum [if (i /= 0) && i <= (length ch) then getValue (ch !! (i-1)) else 0 | i <- md]

getResult2 node = do
  show $ getValue node
