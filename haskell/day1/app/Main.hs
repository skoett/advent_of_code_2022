module Main where

import Data.List.Extra

elfPackages :: [String] -> [[Int]]
elfPackages = (map $ map read) . wordsBy null

elfCalories :: [[Int]] -> [Int]
elfCalories = map sum

topN :: Int -> [Int] -> [Int]
topN n = takeEnd n . sort

top3 :: [Int] -> [Int]
top3 = topN 3

--getLines :: FilePath -> IO [[Int]]
--getCalories = fmap lines . readFile

main :: IO ()
main = do
    content <- readFile "app/input.txt"
    print . sum . top3 . elfCalories . elfPackages . lines $ content
