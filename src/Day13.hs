module Day13 (solve) where

import qualified Data.Set as S
import Data.List.Split
import qualified Data.Array as A
import Debug.Trace

-- https://adventofcode.com/2021/day/13


type Point = (Int, Int)

data Axis = X | Y

newtype Folds = Folds [(Axis, Int)]

parse:: String -> ([Point], Folds)
parse input = (points, folds)
  where
    (pLines, rest) = span (/= "") $ lines input
    points = map (f. splitOn ",") pLines
      where
        f :: [String] -> Point
        f (a:b:[]) = (read a, read b)
    folds = Folds . map (f . (splitOn "=") . drop 11) . tail $ rest
      where
        f:: [String] -> (Axis, Int)
        f (a:b:[]) = (case a of "x" -> X; _ -> Y, read b)

reflect i a = if a <= i then a else i - (a - i)

accum:: S.Set Point -> (Axis, Int) -> S.Set Point
accum points (X, i) = S.map (\(x, y) -> (reflect i x, y)) points
accum points (Y, i) = S.map (\(x, y) -> (x, reflect i y)) points

part1:: ([Point], Folds) -> Int
part1 (points, Folds inputs) = S.size $ accum (S.fromList points) $ head inputs

pretty:: A.Array (Int, Int) Char -> String
pretty arr = unlines . map (\r -> concat [show (arr A.! (r, c)) | c <- [-100..100]]) $ [-100..100]

part2:: ([Point], Folds) -> String
part2 (points, Folds inputs) = pretty . A.accumArray (\_ -> id) '.' ((-100, -100), (100, 100)) . map (\idx -> (idx, '#')) . S.toList . (foldl (accum) (S.fromList points)) $ inputs

solve:: String -> IO ()
solve input = putStrLn "--- Day 13 ---" >> print (part1 parsed) >> print (trace (part2 parsed) ())
    where parsed = parse input
