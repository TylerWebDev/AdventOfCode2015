{-# LANGUAGE OverloadedStrings #-}

module Advent where

import Data.List
import Data.Text (splitOn, unpack, pack, Text)
import qualified Data.ByteString.Char8 as B
import Text.Regex.PCRE.Light

containsAtLeastTwo :: [[a]] -> Bool
containsAtLeastTwo list =
    elemExists $ find (>= 2) $ map length list
        where
            elemExists (Just _) =
                True
            elemExists Nothing =
                False

-- Utils for part 1

isVowel :: Char -> Bool
isVowel char =
    char `elem` ("aeiou" :: String)

hasThreeVowels :: Text -> Bool
hasThreeVowels =
    (>= 3) . length . filter isVowel . unpack

hasDuplicatedLetter :: Text -> Bool
hasDuplicatedLetter str =
    containsAtLeastTwo $ group (unpack str)

testString :: Text
testString =
    "abcdefg"

naughtyCombinations :: [Text]
naughtyCombinations =
    [ "ab"
    , "cd"
    , "pq"
    , "xy"
    ]

containsNaughtCombination :: Text -> Bool
containsNaughtCombination s =
    containsAtLeastTwo $ splitOn <$> naughtyCombinations <*> pure s

isNice :: Text -> Bool
isNice str =
    hasThreeVowels str && hasDuplicatedLetter str && not (containsNaughtCombination str)

-- Utils for part 2

containsDoublet :: String -> Bool
containsDoublet str =
    case match (compile "^.*(.{2}).*(\\1).*$" []) (B.pack str) [] of
        Just _  -> True
        Nothing -> False

matchesABAPattern :: String -> Bool
matchesABAPattern (a:x:b:xs) =
    (a == b) || matchesABAPattern (x : b : xs)
matchesABAPattern [_, _] =
    False
matchesABAPattern [_] =
    False
matchesABAPattern [] =
    False

isNiceRedux :: String -> Bool
isNiceRedux str =
    containsDoublet str && matchesABAPattern str

-- Implementations

partOne :: IO ()
partOne = do
    input <- readFile "input.txt"
    print $ length $ filter (== True) $ naughtOrNice input
        where
            naughtOrNice wordsList =
                map (isNice . pack) (lines wordsList)

partTwo :: IO ()
partTwo = do
    input <- lines <$> readFile "input.txt"
    print . length . filter (== True) $ map isNiceRedux input
