module Day7
  (sol) where

import Util
import qualified Data.Set  as Set
import qualified Data.Char as Char
import qualified Data.HashMap.Lazy as Map

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

-- A Graph that maps each Task to their Dependencies
type Graph = Map.HashMap Char [Char]
createGraph :: [(Char, Char)] -> Graph
createGraph = foldr insertVertex Map.empty
  where
    insertVertex (vertex1, vertex2) = Map.insertWith (++) vertex2 [vertex1]

nWorkers  = 5

taskTime = (+60) . charToInt

data Job   = Job   {task     :: Char, finalTime :: Int  } deriving (Show)
data State = State {currTime :: Int,  jobs      :: [Job]} deriving (Show)

updateState :: State -> Graph -> State
updateState s g = finalState
  where
    finalizedTasks = [tk | (Job tk tm) <- (jobs s), tm == currTime s]
    updatedGraph   = Map.map (remove finalizedTasks) g
      where
        remove ft []     = []
        remove ft (v:vs) = if v `elem` ft then vs' else (v:vs')
          where
            vs' = remove ft vs

    remainingJobs = [j | j <- (jobs s), not $ (task j) `elem` finalizedTasks]

    possibleTasks = quicksort [t | (t, _) <- (Map.toList $ Map.filter (== "0") updatedGraph)]
    possibleJobs  = [j | t <- possibleTasks, let j = Job t (currTime s + taskTime t)]

    updatedJobs  = take nWorkers $ remainingJobs ++ possibleJobs

    updatedState = State (currTime s + 1) updatedJobs
   
    finalState   = if   length updatedJobs == 0 
                   then updatedState
                   else updateState updatedState (Map.filterWithKey (\k _ -> not $ k `elem` updatedTasks) updatedGraph)
      where
        updatedTasks = [tk | (Job tk _) <- updatedJobs]
    

getResult2 content = do
  let dependencies     = map parseDependency $ lines content

      wardDependencies = Set.toList $ Set.fromList $ [('0', x) | (x, _) <- dependencies] ++ [('0', y) | (_, y) <- dependencies]

      dependencyTree   = createGraph $ dependencies ++ wardDependencies

      totalTime        = currTime $ updateState s0 dependencyTree
        where s0 = State (-1) []

      result2          = show $ totalTime
  result2 
