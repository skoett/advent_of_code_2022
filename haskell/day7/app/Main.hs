module Main where

import Control.Monad.Trans.State
import Data.Either (fromRight)
import Data.Foldable (find, minimumBy)
import Data.Maybe (fromJust)
import Data.Tree
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), many, runParser)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- for the Bool part True is dir and False is file
type Directory = Tree (String, Int, Bool)

data Input = CD String | LS | Dir String | File String Int deriving (Show)

type Parser = Parsec Void String

parserDirName :: Parser String
parserDirName = string ".." <|> string "/" <|> many letterChar

parserFileName :: Parser String
parserFileName = many (letterChar <|> char '.')

parserCD :: Parser Input
parserCD = do
    _ <- string "$ cd "
    dirName <- L.lexeme space1 parserDirName
    return (CD dirName)

parserLS :: Parser Input
parserLS = do
    _ <- string "$ ls\n"
    return LS

parserDir :: Parser Input
parserDir = do
    _ <- string "dir "
    dirName <- L.lexeme space1 parserDirName
    return (Dir dirName)

parserFile :: Parser Input
parserFile = do
    num <- L.lexeme space1 L.decimal
    fileName <- L.lexeme space1 parserFileName
    return $ File fileName num

parser :: Parser [Input]
parser = many $ parserLS <|> parserCD <|> parserDir <|> parserFile

nameAndSize :: Input -> (String, Int, Bool)
nameAndSize (Dir name)       = (name, 0, True)
nameAndSize (File name size) = (name, size, False)
nameAndSize _                = undefined

addNode :: Directory -> [String] -> Input -> Directory
addNode (Node v ns) [] input = Node v $ Node (nameAndSize input) [] : ns
addNode (Node v ns) (x : xs) input = Node v $ modified : others
    where others   = filter (\(Node (n, _, _) _) -> n /= x) ns
          current  = fromJust $ find (\(Node (n, _, _) _) -> n == x) ns
          modified = addNode current xs input

type ProcessingState = ([String], Directory)

processInput :: [Input] -> State ProcessingState Directory
processInput [] = do
    (_, dirs) <- get
    return dirs
processInput (LS : xs) = processInput xs
processInput (CD dirname : xs) = do
    (path, dirs) <- get
    case dirname of
        ".." -> put (tail path, dirs)
        _    -> put (dirname : path, dirs)
    processInput xs
processInput (x : xs) = do
    (path, dirs) <- get
    let newDirs = addNode dirs (tail $ reverse path) x
    put (path, newDirs)
    processInput xs

onlyDirs :: Directory -> Directory
onlyDirs f@(Node (_, _, False) _) = f
onlyDirs (Node x xs)              = Node x $ fmap onlyDirs (filter (\(Node (_, _, d) _) -> d) xs)

totalSize :: Directory -> Directory
totalSize d@(Node _ [])       = d
totalSize (Node (name, _, d) xs) = Node (name, sum ints, d) newNodes
    where newNodes = fmap totalSize xs
          ints = fmap (\(Node (_, s, _) _) -> s) newNodes

main :: IO ()
main = do
    content <- readFile "app/input.txt"
    let inputs = runParser parser "input.txt" content
    let dirs = evalState (processInput $ fromRight [] inputs) ([], Node ("/", 0, True) [])
    -- let intTree = fmap show $ totalSize dirs
    -- putStrLn $ drawTree intTree
    let dirsWithSize = onlyDirs $ totalSize dirs
    print . sum . filter (<= 100000) . flatten $ (fmap (\(_, s, _) -> s)) dirsWithSize
    let (Node (_, currentSize, _) _) = dirsWithSize
    let spaceNeede = 30000000 - (70000000 - currentSize)
    let dirNamesWithSize = filter (\(_, s, _) -> s > spaceNeede) $ flatten dirsWithSize
    print $ minimumBy (\(_, s1, _) (_, s2, _) -> compare s1 s2) dirNamesWithSize
