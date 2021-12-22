module Day11 (solve) where
           
import Data.Array
import qualified Data.Sequence as Q
import qualified Data.List as L
import Data.Maybe

-- https://adventofcode.com/2021/day/11

parse:: String -> Array (Int, Int) Int
parse input = array ((0, 0), (9, 9)) [((i, j), read [col])| (i, row) <- [0..] `zip` lines input, (j, col) <- [0..] `zip` row]

-- First, the energy level of each octopus increases by 1.

-- Then, any octopus with an energy level greater than 9 flashes. This increases the energy level of all adjacent octopuses by 1, 
-- including octopuses that are diagonally adjacent. If this causes an octopus to have an energy level greater than 9, it also flashes. 
-- This process continues as long as new octopuses keep having their energy level increased beyond 9. 
-- (An octopus can only flash at most once per step.)

-- Finally, any octopus that flashed during this step has its energy level set to 0, as it used all of its energy to flash.

deltas:: [(Int, Int)]
deltas = [(-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (1, -1), (-1, 1), (1, 1)]

neighbours:: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = (filter $ inRange ((0, 0), (9, 9))) . map (\(dx, dy) -> (x + dx, y + dy)) $ deltas

first:: (Array (Int, Int) Int) -> Array (Int, Int) Int
first = fmap (+ 1)

generation:: Array (Int, Int) Int -> Array (Int, Int) Int
generation arr = result
  where 
    afterFirst = first arr
    initialQueue = Q.fromList [pair | (pair, value) <- assocs afterFirst, value > 9]
    expansion = bfs afterFirst initialQueue
    result = fmap (\value -> if value > 9 then 0 else value) expansion
    
pretty:: Array (Int, Int) Int -> String
pretty arr = unlines . map (\r -> concat [show (arr ! (r, c)) | c <- [0..9]]) $ [0..9]

bfs:: Array (Int, Int) Int -> Q.Seq (Int, Int) -> Array (Int, Int) Int
bfs arr queue =
   case (Q.lookup 0 queue) of
     Just (x, y) -> bfs (arr // updates) ((Q.drop 1 queue) Q.>< Q.fromList nextGen)
       where 
         updates = map (\pair -> (pair, arr ! pair + 1)) . filter ((<= 9) . (arr !)) . neighbours $ (x, y)
         nextGen = [pair | (pair, value) <- updates, value > 9]
     Nothing -> arr
     
count :: (a -> Bool) -> [a] -> Int
count p = L.foldl' (\n -> \x -> if p x then n+1 else n) 0     
     
part1:: Array (Int, Int) Int -> Int
part1 arr = sum . take 101 . map (count (== 0) . elems) $ generations
  where generations = iterate generation arr
  
part2:: Array (Int, Int) Int -> Int
part2 arr = fromMaybe 0 . L.findIndex (all (== 0)) $ generations
  where generations = iterate generation arr

solve:: String -> IO ()
solve input = putStrLn "--- Day 11 ---" >> print (part1 parsed) >> print (part2 parsed)
    where parsed = parse input

