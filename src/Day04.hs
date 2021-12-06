module Day04 (solve) where
  
import Data.List.Split
import Data.List

-- https://adventofcode.com/2021/day/4

type Grid = [[Int]]
  
parse:: String -> ([Int], [Grid])
parse input = 
  let
    segments = lines input
    list = parseList $ head segments
    grid = parseGrid $ tail segments
  in (list, grid)
  
parseList:: String -> [Int]
parseList = map read . splitOn [','] 
  
parseGrid:: [String] -> [Grid]
parseGrid [] = []
parseGrid ("": xs) = parseGrid xs
parseGrid (r1: r2: r3: r4: r5: xs) = map (map read . words) [r1, r2, r3, r4, r5]: parseGrid xs

updateList:: Int -> [Int] -> [Int]
updateList v = map (\i -> if i == v then minBound else i)

updateGrid :: Int -> Grid -> Grid
updateGrid v = map (updateList v)

bingo :: Grid -> Bool
bingo grid = find check grid /= Nothing || find check (transpose grid) /= Nothing
  where
    check::[Int] -> Bool
    check = (== Nothing) . find (/= minBound)

sumUnmarked:: Grid -> Int
sumUnmarked = sum . filter (/= minBound) . concat

part1:: ([Int], [Grid]) -> Int
part1 ([], _) = error "impossible"
part1 ((x: xs), grids) = 
  let newGrids = map (updateGrid x) grids                                            
  in case (find bingo newGrids) of 
       Just grid -> x * sumUnmarked grid
       Nothing -> part1 (xs, newGrids)
       
part2:: ([Int], [Grid]) -> Int
part2 ([], _) = error "impossible"
part2 ((x: xs), grids) = 
  let 
    newGrids = map (updateGrid x) grids   
    (bingoGrids, notBingoGrids) = partition bingo newGrids                                         
  in case notBingoGrids of 
       [] -> x * sumUnmarked (last bingoGrids)
       _ -> part2 (xs, notBingoGrids)

solve:: String -> IO ()
solve input = putStrLn "--- Day 04 ---" >> print (part1 parsed) >> print (part2 parsed)
    where parsed = parse input
