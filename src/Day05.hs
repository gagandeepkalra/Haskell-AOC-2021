module Day05 (solve) where

import Data.List.Split
import Data.Array
import Data.List(unfoldr)

-- https://adventofcode.com/2021/day/5
  
type Pair = (Int, Int)
  
parse:: String -> [(Pair, Pair)]
parse input = 
  let
    parsePair:: [Int] -> (Pair, Pair)
    parsePair (a:b:c:d:[]) = ((a,b), (c, d))
  in (map (parsePair . map read . filter (/= "") . splitOneOf ",-> ") . lines) input
  
isHorizontalOrVerticalLine :: (Eq a1, Eq a2) => ((a1, a2), (a1, a2)) -> Bool
isHorizontalOrVerticalLine ((a, b), (c, d)) = a == c || b == d
  
inOrder:: (Pair, Pair) -> (Pair, Pair)
inOrder ((a, b), (c, d)) = if a == c then ((a, min b d), (a, max b d)) else ((min a c, b), (max a c, b))

part1::[(Pair, Pair)] -> Int
part1 pairs = 
  let
    onlyStraightLines = (map inOrder . filter isHorizontalOrVerticalLine) pairs
    
    arr:: Array (Int, Int) Int
    arr = accumArray (+) 0 ((0, 0), (999, 999)) [(i, 1) | p <- onlyStraightLines, i <- range p]
  in foldl (\acc -> \i -> if i >= 2 then acc + 1 else acc) 0 arr
  
delta:: (Pair, Pair) -> Pair
delta ((a, b), (c, d)) = (signum (c - a), signum (d - b))

part2::[(Pair, Pair)] -> Int
part2 pairs = 
  let
    points:: (Pair, Pair) -> [Pair]
    points p@(start, end) = 
      let
        f current@(x, y) = if current == end then Nothing else Just (next, next)
          where 
            (dx, dy) = delta p
            next = (x + dx, y + dy)
      in start: unfoldr f start
    
    arr:: Array (Int, Int) Int
    arr = accumArray (+) 0 ((0, 0), (999, 999)) [(i, 1) | p <- pairs, i <- points p]
  in foldl (\acc -> \i -> if i >= 2 then acc + 1 else acc) 0 arr
  
  
solve:: String -> IO ()
solve input = putStrLn "--- Day 04 ---" >> print (part1 parsed) >> print (part2 parsed)
    where parsed = parse input

