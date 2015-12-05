{-# LANGUAGE OverloadedStrings #-}

module Advent where
    
import Data.List
import Data.Text (splitOn, unpack, pack, Text)

containsAtLeastTwo :: [[a]] -> Bool
containsAtLeastTwo list =
    elemExists $ find (>= 2) $ map length list
        where
            elemExists (Just _) =
                True
            elemExists Nothing =
                False
                
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

-- Implementations

partOne :: IO ()
partOne = do
    input <- readFile "input.txt"
    print $ length $ filter (== True) $ naughtOrNice input
        where
            naughtOrNice wordsList =
                map (isNice . pack) (lines wordsList)
    