module Main where

import Data.List.Split (splitOn)

listToTuple :: [a] -> (a, a)
listToTuple (x : y : []) = (x, y)
listToTuple _            = undefined

fullyContains :: ((Int, Int), (Int, Int)) -> Bool
fullyContains ((a1, a2), (b1, b2)) = a1 <= b1 && a2 >= b2

fullyContainsAny :: ((Int, Int), (Int, Int)) -> Bool
fullyContainsAny (a, b) = fullyContains (a, b) || fullyContains (b, a)

overlaps :: ((Int, Int), (Int, Int)) -> Bool
overlaps ((a1, a2), (b1, b2)) = not (a1 > b2 || a2 < b1)

main :: IO ()
main = do
    content <- readFile "app/input.txt"
    let pairs = map (splitOn ",") . lines $ content
    let splitter = map . map $ splitOn "-"
    let ranges = splitter pairs
    let parsed = (map . map . map) (read :: String -> Int) ranges
    let tupleRanges = (map . map) listToTuple parsed
    let allRanges = map listToTuple tupleRanges
    -- print . length $ filter id $ map fullyContainsAny allRanges
    print . length $ filter id $ map overlaps allRanges
