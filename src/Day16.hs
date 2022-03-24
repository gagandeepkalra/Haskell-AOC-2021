module Day16 (solve) where

import qualified Data.Array as A

import Data.Char (digitToInt)
import Data.Array.ST
import Control.Monad (foldM)
import qualified Data.PQueue.Min as Q
import Numeric
import GHC.Base
import GHC.Read
import GHC.Real
import GHC.Float
import GHC.Num
import GHC.Show
import Text.ParserCombinators.ReadP( ReadP, readP_to_S, pfail )
import qualified Text.Read.Lex as L

toBinary :: Int -> String
toBinary n = showIntAtBase 2 ("01" !!) n ""

binaryToInt :: String -> Int
binaryToInt = fst . head . readInt 2 (\c -> ord c - ord '0' <= 1) ord

-- https://adventofcode.com/2021/day/16

step1:: String -> String
step1 = concatMap $ toBinary . digitToInt

-- BITS transmission contains a single packet at its outermost layer which itself contains many other packets.

parse :: Read e => [Char] -> A.Array (Int, Int) e
parse input = A.array ((0, 0), (rows -1, cols - 1)) [((i, j), read [col])| (i, row) <- [0..] `zip` ls, (j, col) <- [0..] `zip` row]
  where 
    ls = lines input
    rows = length ls
    cols = length $ head ls


solve:: String -> IO ()
solve input = putStrLn "--- Day 16 ---" -- >> print (part1 parsed) >> print (part2 input)
    where parsed = parse input


