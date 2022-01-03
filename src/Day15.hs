module Day15 (solve) where

import qualified Data.Array as A

import Data.Char (digitToInt)
import Data.Array.ST
import Control.Monad (foldM)
import qualified Data.PQueue.Min as Q

-- https://adventofcode.com/2021/day/15

type Coord = (Int, Int)

parse :: Read e => [Char] -> A.Array (Int, Int) e
parse input = A.array ((0, 0), (rows -1, cols - 1)) [((i, j), read [col])| (i, row) <- [0..] `zip` ls, (j, col) <- [0..] `zip` row]
  where 
    ls = lines input
    rows = length ls
    cols = length $ head ls

deltas:: [(Int, Int)]
deltas = [(-1, 0), (1, 0), (0, -1), (0, 1)]

newtype QueueState = QueueState (Int, Coord)
  deriving Eq

instance Ord QueueState where
  compare (QueueState (a, _)) (QueueState (b, _)) = compare a b
    
dijkstra:: A.Array (Int, Int) Int -> A.Array (Int, Int) Int 
dijkstra weights = runSTArray $ do
  distances <- newArray (A.bounds weights) (-1)
  
  let 
    isValid x y = inRange (A.bounds weights) (x, y)
    
    updateQ x y d pq' (dx, dy) =
      let (nx, ny) = (x + dx, y + dy)
      in 
        if isValid nx ny then do
           let c = weights A.! (nx, ny)
           sd <- readArray distances (nx, ny)
           if sd == -1 || d + c < sd then writeArray distances (nx, ny) (d + c) >> pure (Q.insert (QueueState (d + c, (nx, ny))) pq') else pure pq'  
        else pure pq'
  
    go pq = case Q.minView pq of 
      Just (QueueState (d, (x, y)), pqRest) -> do
        d' <- readArray distances (x, y)
        pq' <- if d' == d then foldM (updateQ x y d) pqRest deltas else pure pqRest
        go pq'
      Nothing -> pure ()
    
  writeArray distances (0, 0) 0
      
  go $ Q.singleton $ QueueState (0, (0, 0))
  
  return distances
  
part1 :: A.Array (Int, Int) Int -> Int
part1 input = last $ A.elems result
  where result = dijkstra input
  
parseLargeCave:: String -> A.Array (Int, Int) Int
parseLargeCave input = A.array ((0, 0), (rows -1, cols - 1)) [((i, j),  col)| (i, row) <- [0..] `zip` grid, (j, col) <- [0..] `zip` row]
  where 
    toRow s = concat . take 5 . iterate increment $ map digitToInt s
    increment = map ((+ 1) . (`mod` 9))
    
    grid = concat . take 5 . iterate (map increment) . map toRow . lines $ input
    
    rows = length grid
    cols = length $ head grid   
  
part2 :: String -> Int
part2 = last . A.elems . dijkstra . parseLargeCave

solve:: String -> IO ()
solve input = putStrLn "--- Day 15 ---" >> print (part1 parsed) >> print (part2 input)
    where parsed = parse input


