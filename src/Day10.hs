module Day10 (solve) where
            
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List as L
import Data.Maybe as O
import Data.Sequence as Q
import Debug.Trace

-- https://adventofcode.com/2021/day/10

parse:: String -> [String]
parse = lines

score :: M.Map Char Int
score = M.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

findIllegal:: String -> Maybe Char
findIllegal input = 
  let
    go ('(': xs) stack = go xs ('(': stack)
    go ('[': xs) stack = go xs ('[': stack)
    go ('{': xs) stack = go xs ('{': stack)
    go ('<': xs) stack = go xs ('<': stack)
    go (')': xs) stack = case stack of ('(': ss) -> go xs ss; _ -> Just ')'
    go (']': xs) stack = case stack of ('[': ss) -> go xs ss; _ -> Just ']'
    go ('}': xs) stack = case stack of ('{': ss) -> go xs ss; _ -> Just '}'
    go ('>': xs) stack = case stack of ('<': ss) -> go xs ss; _ -> Just '>'
    go [] _ = Nothing
    result = go input []
  in result
    
part1:: [String] -> Int
part1 = sum . map (score M.!) . catMaybes . map findIllegal

points :: M.Map Char Integer
points = M.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]

findRemainingString:: String -> Maybe String
findRemainingString input = 
  let
    go ('(': xs) stack = go xs ('(': stack)
    go ('[': xs) stack = go xs ('[': stack)
    go ('{': xs) stack = go xs ('{': stack)
    go ('<': xs) stack = go xs ('<': stack)
    go (')': xs) stack = case stack of ('(': ss) -> go xs ss; _ -> Nothing
    go (']': xs) stack = case stack of ('[': ss) -> go xs ss; _ -> Nothing
    go ('}': xs) stack = case stack of ('{': ss) -> go xs ss; _ -> Nothing
    go ('>': xs) stack = case stack of ('<': ss) -> go xs ss; _ -> Nothing
    go [] stack = Just . (map f) $ stack
      where 
        f '(' = ')'  
        f '[' = ']' 
        f '{' = '}' 
        f '<' = '>'
  in go input []
  
part2:: [String] -> Integer
part2 = g . (map $ foldl f 0) . catMaybes . map findRemainingString 
  where
    f acc c = acc * 5 + points M.! c 
    g list = (L.!! (L.length list `div` 2)) . L.sort $ list  


solve:: String -> IO ()
solve input = putStrLn "--- Day 10 ---" >> print (part1 parsed) >> print (part2 parsed)
    where parsed = parse input



