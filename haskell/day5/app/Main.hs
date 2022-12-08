module Main where

import Data.Char (isDigit, isLetter)
import Data.List (transpose)

parseStacks :: [String] -> [[Char]]
parseStacks input = map reverse . map (filter isLetter) . map tail $ cols
    where cols = filter (isDigit . head) . transpose . reverse $ input

listToTuple :: [a] -> (a, a, a)
listToTuple (x : y : z : []) = (x, y, z)
listToTuple _                = undefined

parseMove :: String -> (Int, Int, Int)
parseMove s = listToTuple $ map read [l !! 1, l !! 3, l !! 5]
    where l = words s

executeMove :: [[Char]] -> (Int, Int, Int) -> [[Char]]
executeMove l (n, from, to) = left ++ [firstToAdd] ++ middle ++ [secondToAdd] ++ end
    where firstSplit = min from to
          secondSplit = max from to
          (left, right) = splitAt (firstSplit - 1) l
          (middle, _end) = splitAt (secondSplit - firstSplit - 1) (tail right)
          end = tail _end
          fromStr = l !! (from - 1)
          toStr = l !! (to - 1)
          (toMove, rest) = splitAt n fromStr
          -- newTo = reverse toMove ++ toStr
          newTo = toMove ++ toStr
          firstToAdd = if from < to then rest else newTo
          secondToAdd = if from < to then newTo else rest

executeMoves :: [String] -> [(Int, Int, Int)] -> [String]
executeMoves = foldl executeMove

main :: IO ()
main = do
    content <- readFile "app/input.txt"
    let startingPosition = takeWhile (not . null) $ lines content
    let stacks = parseStacks startingPosition
    let movesList = tail $ dropWhile (not . null) $ lines content
    let moves = map parseMove movesList
    print $ executeMoves stacks moves
