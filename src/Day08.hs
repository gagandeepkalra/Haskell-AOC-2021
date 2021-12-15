module Day08 (solve) where

import Data.List.Split
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List 
import Data.Semigroup

-- https://adventofcode.com/2021/day/8

parse:: String -> [([String], [String])]
parse = map f . lines
  where
    f:: String -> ([String], [String])
    f line = (g a, g b)
      where 
        (a:b:[]) = (splitOn "|" line)
        g = filter (/= "") . words

part1:: [([String], [String])] -> Int
part1 = foldl (\acc -> \x -> acc + f x) 0
  where
    ofInterest s = l == 2 || l == 3 || l == 4 || l == 7
      where l = length s
    f (_, bs) = foldl (\acc -> \x -> if ofInterest x then acc + 1 else acc) 0 bs

demystify:: [S.Set Char] -> M.Map (S.Set Char) Int
demystify all = M.fromList [(zero, 0), (one, 1), (two, 2), (three, 3), (four, 4), (five, 5), (six, 6), (seven, 7), (eight, 8), (nine, 9)]
  where
    Just one = find ((== 2) . length) all
    Just four = find ((== 4) . length) all
    Just seven = find ((== 3) . length) all
    Just eight = find ((== 7) . length) all
    
    length5s = filter ((== 5) . length) all
    length6s = filter ((== 6) . length) all
    
    Just six = find (not . S.isSubsetOf one) length6s
    bd = S.difference four one
    Just nine = find (\set -> set /= six && bd `S.isSubsetOf` set) length6s
    Just zero = find (\set -> set /= six  && set /= nine) length6s
    
    Just f = find (\ch -> S.member ch six) one 
    Just c = find (\ch -> S.notMember ch six) one 
    
    Just two = find (S.notMember f) length5s
    Just five = find (S.notMember c) length5s
    Just three = find (\set -> set /= two && set /= five) length5s

part2::  [([String], [String])] -> Int
part2 = getSum . foldMap f
  where 
    f:: ([String], [String]) -> Sum Int
    f (code, input) = Sum (read $ concat (map (show . (decoded M.!) . S.fromList) input))
      where 
        decoded = demystify $ map S.fromList code

solve:: String -> IO ()
solve input = putStrLn "--- Day 08 ---" >> print (part1 parsed) >> print (part2 parsed)
    where parsed = parse input

