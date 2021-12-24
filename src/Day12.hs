module Day12 (solve) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split
import Data.Maybe
import Data.Char
import Data.Semigroup

-- https://adventofcode.com/2021/day/12

type Graph = M.Map String [String]

parse:: String -> Graph
parse  = M.fromListWith (++) . concatMap f . lines
  where f s = let (k:v:[]) = splitOn "-" s in [(k, [v]), (v, [k])]

start :: [Char]
start = "start"

end :: [Char]
end = "end"

isRepeatable1 :: String -> Bool
isRepeatable1 s = isUpper $ head s

dfs1:: Graph -> S.Set String -> String -> Sum Int
dfs1 graph visited current = if current == end then Sum 1 else foldMap (dfs1 graph newVisited) validNeighbours
  where
    newVisited = if isRepeatable1 current then visited else S.insert current visited
    validNeighbours = filter (`S.notMember` visited) $ fromMaybe [] $ graph M.!? current

part1:: Graph -> Int
part1 graph = getSum $ dfs1 graph S.empty start

dfs2:: Graph -> (S.Set String, Bool) -> String -> Sum Int
dfs2 graph (visited, flag) current = 
  if current == end then Sum 1 
  else if isRepeatable1 current then foldMap (dfs2 graph (visited, flag)) neighbours
  else if S.notMember current visited then foldMap (dfs2 graph (S.insert current visited, flag)) neighbours
  else if flag && current /= start then foldMap (dfs2 graph (visited, False)) neighbours  -- member
  else Sum 0                                                                              -- member
    where 
      neighbours = fromMaybe [] $ graph M.!? current

part2:: Graph -> Int
part2 graph = getSum $ dfs2 graph (S.empty, True) start

solve:: String -> IO ()
solve input = putStrLn "--- Day 12 ---" >> print (part1 parsed) >> print (part2 parsed)
    where parsed = parse input


