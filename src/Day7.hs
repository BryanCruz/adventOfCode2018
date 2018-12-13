module Day7
  (sol) where

import Util
import qualified Data.Set  as Set
import qualified Data.Char as Char
import qualified Data.HashMap.Lazy as Map
import Debug.Trace

-- Day 7 - Part 1 and 2
-- The Sum of Its Parts
sol :: IO ()
sol = do
  content <- getContents
  let result1 = getResult1 content
      result2 = getResult2 content
  putStrLn result1
  putStrLn result2

-- Part 1
-- Inputs are like "Step C must be finished before step A can begin."
parseDependency :: [Char] -> (Char, Char)
parseDependency cs = (a, b)
  where
    a  = fstLetter cs
    b  = sndLetter cs
    
    fstLetter []             = error "Empty list"
    fstLetter (' ':x:' ':xs) = x
    fstLetter (_:xs)         = fstLetter xs

    sndLetter []             = error "Empty list"
    sndLetter (' ':x:' ':xs) = fstLetter xs
    sndLetter (_:xs)         = sndLetter xs

correctOrder :: [(Char, Char)] -> [Char]
correctOrder []    = []
correctOrder edges = result
  where
    haveDependencies = Set.fromList [x | (_, x) <- edges]
    noDependencies   = [x | (x, _) <- edges, not $ x `Set.member` haveDependencies]

    currStep         = minimum noDependencies
    remainingEdges   = [(x, y) | (x, y) <- edges, x /= currStep]

    result           = currStep : correctOrder remainingEdges

getResult1 content = do
  let dependencies     = map parseDependency $ lines content

      wardDependencies = [(y, '0') | (_, y) <- dependencies]

      result1          = show $ correctOrder (dependencies ++ wardDependencies)
  result1


-- Part 2
charToInt :: Char -> Int
charToInt c = Char.ord c - Char.ord 'A' + 1

workers = 5

stepTime = (+60) . charToInt

createGraph :: [(Char, Char)] -> Map.HashMap Char (Set.Set Char)
createGraph = foldr insertVertex Map.empty
  where
    insertVertex (vertex1, vertex2) = Map.insertWith Set.union vertex2 (Set.singleton vertex1)

totalTime 

getResult2 content = do
  let dependencies     = map parseDependency $ lines content

      wardDependencies = Set.toList $ Set.fromList [ ('0', x) | (x, _) <- dependencies]

      dependencyTree   = createGraph $ dependencies ++ wardDependencies

      result2          = totalTime dependencyTree
     
  result2 
