module Day07 (solve, part1) where

import Data.List.Split
import Data.List

-- https://adventofcode.com/2021/day/6

parse:: String -> [Int]
parse = (map read . filter (/= "") . splitOn ",")

process:: [Int] -> [Int]
process = 
  let
    f (acc, (prev, cost, n)) current = (newCost: acc, (current, newCost, n + 1)) 
      where newCost = cost + abs (current - prev) * n
  in fst . foldl f ([], (0, 0, 0))

part1:: [Int] -> Int
part1 input = minimum zipped
  where
    x = sort input
    left  = reverse $ process x
    right = process $ reverse x
    zipped = zipWith (+) left right
    f (acc, i) (current, j) = if (current < acc) then (current, j) else (acc, i) 
    

part2:: [Int] -> Int
part2 input = min res1 res2
  where 
    total = (fromIntegral . sum) input
    count = (fromIntegral . length) input

    mean1 = ceiling $ total / count
    mean2 = floor $ total / count

    distance a b = (delta * (delta + 1)) `div` 2
      where delta = abs (a - b)
    
    res1 = sum $ map (distance mean1) input
    res2 = sum $ map (distance mean2) input

solve:: String -> IO ()
solve input = putStrLn "--- Day 07 ---" >> print (part1 parsed) >> print (part2 parsed)
    where parsed = parse input
