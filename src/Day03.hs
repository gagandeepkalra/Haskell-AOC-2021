module Day03 (solve) where

import Data.Bits
import Data.Char(digitToInt)

-- https://adventofcode.com/2021/day/3

updateCount :: [Integer]  -> String -> [Integer]
updateCount = zipWith (\i -> \c -> if c == '0' then i - 1 else if c == '1' then i + 1 else error $ "invalid input" ++ [c])

turn :: Integer -> Integer
turn n
  | n > 0 = 1
  | n < 0 = 0
  | otherwise = error "equal occurrence"

binaryToInteger:: [Integer] -> Integer
binaryToInteger = fst . foldr (\i -> \(acc, n) -> (acc + i `shiftL` n, n + 1)) (0, 0)

part1:: [String] -> Integer
part1 input =
  let
    acc = (map turn . foldl updateCount [0, 0 ..]) input

    gamma = binaryToInteger acc
    epsilon = (binaryToInteger . map (xor 1)) acc
  in gamma * epsilon

oxygenBitCriteria::Integer -> Integer
oxygenBitCriteria i = if i > 0 then 1 else if i < 0 then 0 else 1

carbondioxideBitCriteria::Integer -> Integer
carbondioxideBitCriteria i = if i > 0 then 0 else if i < 0 then 1 else 0

maybeHead :: [a] -> Maybe a
maybeHead (x: _) = Just x
maybeHead [] = Nothing

contains :: Eq a => a -> Maybe a -> Bool
contains i (Just j) = i == j
contains _ _ = False

findRating:: (Integer -> Integer) -> [[Integer]] -> Integer
findRating criteria =
  let
    extractDigit:: [[Integer]] -> Maybe Integer
    extractDigit [] = Nothing
    extractDigit input =
      let
        reduce::Maybe Integer -> Maybe Integer -> Maybe Integer
        reduce Nothing _ = Nothing
        reduce _ Nothing = Nothing
        reduce (Just accI) (Just 0) = Just (accI - 1)
        reduce (Just accI) (Just 1) = Just (accI + 1)
        reduce _ _ = error "impossible"

      in (fmap criteria . foldl reduce (Just 0) . map maybeHead) input

    keep:: Integer -> [[Integer]] -> [[Integer]]
    keep x = map tail . filter ((contains x) . maybeHead)

    go::[[Integer]] -> [Integer]
    go (xs:[]) = xs
    go inp = case (extractDigit inp) of
      Just i -> i: go (keep i inp)
      Nothing -> []

  in binaryToInteger . go

part2:: [String] -> Integer
part2 input =
  let
    p = map (map (toInteger . digitToInt)) input

    o = findRating oxygenBitCriteria p
    co = findRating carbondioxideBitCriteria p
  in o * co

solve:: String -> IO ()
solve input = putStrLn "--- Day 03 ---" >> print (part1 parsed) >> print (part2 parsed)
    where parsed = lines input