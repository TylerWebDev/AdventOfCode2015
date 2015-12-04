module Advent where

import Data.List

upOrDown :: Int -> Char -> Int
upOrDown carry paren
    | paren == '(' = carry + 1
    | paren == ')' = carry - 1
    | otherwise    = carry

whatFloor :: String -> Int
whatFloor =
    foldl upOrDown 0

floorTraversalPath :: String -> [Int]
floorTraversalPath =
    map whatFloor . inits

firstNegativeIndex :: [Int] -> Maybe Int
firstNegativeIndex list =
    snd <$> find (\f -> fst f < 0) (zip list [0..])

findBasementEntryPoint :: String -> Maybe Int
findBasementEntryPoint =
    firstNegativeIndex . floorTraversalPath

-- Implementation from Files

partOne :: IO ()
partOne = do
    file <- readFile "input.txt"
    print $ whatFloor file

partTwo :: IO ()
partTwo = do
    file <- readFile "input.txt"
    print $ findBasementEntryPoint file
