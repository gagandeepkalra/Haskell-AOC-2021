module Day02 (solve) where
  
import Text.Printf

-- https://adventofcode.com/2021/day/2

data Direction = Forward | Backward | Upward | Downward
 
instance Read Direction where
  readsPrec _ "forward" = [(Forward, "")]
  readsPrec _ "backward" = [(Backward, "")]
  readsPrec _ "up" = [(Upward, "")]
  readsPrec _ "down" = [(Downward, "")]
  readsPrec _ input = error $ printf "invalid input %s" input
  
  
part1:: [(Direction, Integer)] -> Integer
part1 = 
  let
    reduce (x, y) (Forward, i) = (x + i, y)
    reduce (x, y) (Backward, i) = (x - i, y)
    reduce (x, y) (Downward, i) = (x, y + i)
    reduce (x, y) (Upward, i) = (x, y - i)
    
    multiply (x, y) = x * y
  in multiply . foldl reduce (0, 0)
  

part2 :: [(Direction, Integer)] -> Integer
part2 = 
  let
    reduce (x, y, z) (Forward, i) = (x + i, y + z * i, z)
    reduce (x, y, z) (Backward, _) = (x, y, z)
    reduce (x, y, z) (Downward, i) = (x, y, z + i)
    reduce (x, y, z) (Upward, i) = (x, y, z - i)
    
    multiply (x, y, _) = x * y
  in multiply . foldl reduce (0, 0, 0)
  
parse:: String -> (Direction, Integer)
parse input = case (words input) of
  a: b : [] -> (read a, read b)
  _ -> error $ printf "invalid input %s" input
  
  
solve:: String -> IO ()
solve input = putStrLn "--- Day 02 ---" >> print (part1 $ p input) >> print (part2 $ p input)
    where p = map parse . lines
