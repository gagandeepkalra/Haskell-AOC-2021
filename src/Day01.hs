module Day01 (solve) where
  
-- https://adventofcode.com/2021/day/1  

part1:: [Int] -> Int
part1 = 
  let
    go:: (Int, Maybe Int) -> Int -> (Int, Maybe Int)
    go (acc, Just prev) curr = (if prev < curr then acc + 1 else acc, Just curr)
    go (acc, Nothing) curr = (acc, Just curr)
  in fst . foldl go (0, Nothing) 
  
part2:: [Int] -> Int
part2 =
  let 
    go:: Int -> [Int] -> Int
    go acc (a:b:c:d:xs) = go (if a < d then acc + 1 else acc) (b:c:d:xs) 
    go acc _ = acc
   in go 0
   
solve:: String -> IO ()
solve input = putStrLn "--- Day 01 ---" >> print (part1 $ p input) >> print (part2 $ p input)
    where p = map read . lines
