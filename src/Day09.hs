module Day09 (solve) where
            
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List as L
import Data.Maybe as O
import Data.Sequence as Q

-- https://adventofcode.com/2021/day/9

type Coord = (Int, Int)

parse:: String -> M.Map (Int, Int) Int
parse = M.fromList . indexed
  where
    indexed :: String -> [((Int, Int), Int)]
    indexed input =
      do
        (row, line) <- [0..] `L.zip` (lines input)
        (col, c) <- [0..] `L.zip` line
        pure ((row, col), read [c] :: Int)
        
deltas :: [(Int, Int)]
deltas = [(-1, 0), (1, 0), (0, -1), (0, 1)]

isMinimum:: (M.Map Coord Int) -> Coord -> Int -> Bool
isMinimum m (x, y) current = all check deltas
  where check (dx, dy) = current < (fromMaybe maxBound $ m M.!? (x + dx, y + dy))

part1:: M.Map Coord Int -> Int
part1 m = M.foldlWithKey' (\acc -> \key -> \value -> if isMinimum m key value then acc + value + 1 else acc) 0 m

part2:: M.Map Coord Int -> Int
part2 m = (product . L.take 3) allBasins
  where
    f (v, acc) c = (newV, size: acc)
      where (size, newV) = basin m v c
    allMinimas = M.foldlWithKey' (\acc -> \key -> \value -> if isMinimum m key value then key: acc else acc) [] m
    allBasins = (L.reverse . L.sort . snd . foldl f (S.empty, [])) allMinimas
      
-- A basin is all locations that eventually flow downward to a single low point. 
basin:: (M.Map Coord Int) -> S.Set Coord -> Coord -> (Int, S.Set Coord)
basin m visited coord = bfs m visited (Q.fromList [coord]) 0

bfs:: (M.Map Coord Int) -> S.Set Coord -> Q.Seq Coord -> Int -> (Int, S.Set Coord)
bfs m visited queue acc =
   case (Q.lookup 0 queue) of
     Just coord@(x, y) ->
       let
         c = m M.! coord
         children = (
           catMaybes 
           . map(\(nx, ny) ->  (m M.!? (nx, ny)) >>= (\h -> if c < h && h /= 9 then Just (nx, ny) else Nothing)) 
           . map (\(dx, dy) -> (x + dx, y + dy))
           ) deltas
         updatedQueue = (Q.drop 1 queue) Q.>< Q.fromList children
         updatedVisited = S.insert coord visited
       in if S.member coord visited then bfs m visited (Q.drop 1 queue) acc else bfs m updatedVisited updatedQueue (acc + 1)
     Nothing -> (acc, visited)

solve:: String -> IO ()
solve input = putStrLn "--- Day 09 ---" >> print (part1 parsed) >> print (part2 parsed)
    where parsed = parse input

