module Day14 (solve) where

import qualified Data.Set as S
import Data.List.Split
import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Map.Strict as MM
import qualified Data.List as L
import Data.Maybe

-- https://adventofcode.com/2021/day/14

parse:: String -> (String, M.Map (Char, Char) Char)
parse input = (first, M.fromList dict)
  where
    (first:_:rest) = lines input
    dict = map (f . (splitOn " -> ")) $ rest
      where
        f:: [String] -> ((Char, Char), Char)
        f [[a, b], [c]] = ((a, b), c)
        f _ = error "invalid"
        
sliding :: [a] -> [(a, a)]
sliding (a:b:rest) = (a,b): sliding (b:rest)
sliding [_] = []
sliding [] = []

times :: Show a => Int -> (a -> a) -> a -> a
times 0 _ a = a
times n f a = times (n - 1) f $ f a

-- Apply 10 steps of pair insertion to the polymer template and find the most and least common elements in the result. 
-- What do you get if you take the quantity of the most common element and subtract the quantity of the least common element?
step:: M.Map (Char, Char) Char -> M.Map (Char, Char) Integer -> M.Map (Char, Char) Integer
step dict input = M.foldlWithKey' (\acc -> \(a, b) -> \count -> maybe acc (\c -> update acc (a, c) (c, b) count) (dict M.!? (a, b))) M.empty input
  where update acc k1 k2 v = M.insertWith (+) k1 v $ M.insertWith (+) k2 v $ acc
  
compute:: Int -> (String, M.Map (Char, Char) Char) -> Integer
compute n (input, m) = maximum frequency - minimum frequency
  where 
    initial = L.foldl' (\acc -> \pair -> MM.insertWith (+) pair (1:: Integer) acc) M.empty $ sliding input
    
    result = times n (step m) initial
    
    frequencyMap = M.foldlWithKey' (\acc -> \(a, b) -> \count -> M.adjust (+ count) a $ M.adjust (+ count) b $ acc) (M.fromList $ ['A'..'Z'] `zip` [0, 0..]) result
    
    frequencyMapAdjusted = fmap (`div` 2) . M.adjust ((+) 1) (head input) $ M.adjust ((+) 1) (last input) $ frequencyMap 
    
    frequency = filter (/= 0) . (map snd) . M.toList $ frequencyMapAdjusted
    
part1 :: (String, MM.Map (Char, Char) Char) -> Integer
part1 = compute 10

part2 :: (String, MM.Map (Char, Char) Char) -> Integer
part2 = compute 40

solve:: String -> IO ()
solve input = putStrLn "--- Day 14 ---" >> print (part1 parsed) >> print (part2 parsed)
    where parsed = parse input


