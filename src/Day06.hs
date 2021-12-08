module Day06 (solve) where

import Data.List.Split
import Data.Array

-- https://adventofcode.com/2021/day/6

parse:: String -> [Integer]
parse = (map read . filter (/= "") . splitOn ",")

part1:: [Integer] -> Integer
part1 = go 80
  where
    reproduction:: Integer -> [Integer]
    reproduction 0 = [6, 8]
    reproduction n = [n - 1]
    
    go:: Integer -> [Integer] -> Integer
    go 0 current = toInteger (length current)
    go n current = go (n - 1) (concatMap reproduction current)

part2:: [Integer] -> Integer
part2 input = go 256 (accumArray (+) 0 (0, 8) $ zip (map fromInteger input) [1, 1..])
  where
    go:: Int -> Array Int Integer -> Integer
    go 0 m = sum m
    go n m = go (n - 1) (accumArray (+) 0 (bounds m) $ concat [ f i (m!i) | i <- indices m] )
      where
        f 0 c = [(6, c), (8, c)]
        f i c = [(i - 1, c)]

solve:: String -> IO ()
solve input = putStrLn "--- Day 06 ---" >> print (part1 parsed)  >> print (part2 parsed)
    where parsed = parse input

