module Advent where

import Data.List
import Data.Char
import Text.Regex.PCRE.Light
import qualified Data.ByteString.Char8 as B

-- Generation for the next password
nextPassword :: String -> String
nextPassword str =
    let incrementChar = chr . (+1) . ord in
        case last str of
            'z' -> nextPassword (init str) ++ "a"
            _   -> init str ++ [incrementChar $ last str]

-- Components for requirement 1
sublistThree :: [a] -> [[a]]
sublistThree l
    | length l >= 3 =
        take 3 l : sublistThree (tail l)
    | otherwise =
        []

isCharStraight :: String -> Bool
isCharStraight [a, b, c] =
    ord a == ord b - 1 && ord b == ord c - 1
isCharStraight _ =
    False

containsStraight :: String -> Bool
containsStraight str =
    any isCharStraight (sublistThree str)

-- Components for requirement 2
doesNotContainIOL :: String -> Bool
doesNotContainIOL =
    not . any (`elem` "iol")

-- Components for requirement 3
containsDoublet :: String -> Bool
containsDoublet str =
    case match (compile (B.pack "(.)(\\1).*(.)(\\3)") []) (B.pack str) [] of
        Just _  -> True
        Nothing -> False

-- Implementation

meetsRequirements :: String -> Bool
meetsRequirements str =
    containsDoublet str && doesNotContainIOL str && containsStraight str

findNextPassword :: String -> Maybe String
findNextPassword pass =
    find meetsRequirements (tail $ iterate nextPassword pass)

partOne :: IO ()
partOne =
    print $ findNextPassword "vzbxkghb"

partTwo :: IO ()
partTwo =
    print $ findNextPassword "vzbxkghb" >>= findNextPassword