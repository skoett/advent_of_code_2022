module Main where

import Data.Char (isUpper)
import Data.List.Split (chunksOf)
import Data.Set (Set, fromList, toList, intersection)

sack :: [Char] -> Set Char
sack = fromList

misplacedItem :: ([Char], [Char]) -> Char
misplacedItem (l, r) = head $ toList $ intersection ls rs
    where ls = sack l
          rs = sack r

prio :: Char -> Int
prio c | isUpper c = fromEnum c - 38
       | otherwise = fromEnum c - 96

sackContents :: String -> ([Char], [Char])
sackContents l = splitAt ((length l + 1) `div` 2) l

badge :: [[Char]] -> Char
badge (x : y : z : []) = head $ toList $ xs `intersection` ys `intersection` zs
    where xs = fromList x
          ys = fromList y
          zs = fromList z
badge _                = 'a'

main :: IO ()
main = do
    content <- readFile "app/input.txt"
    -- print . sum . map prio . map misplacedItem . map sackContents . lines $ content
    print . sum . map prio . map badge . chunksOf 3 . lines $ content
