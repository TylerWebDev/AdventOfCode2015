{-# LANGUAGE OverloadedStrings #-}

module Advent where

import Data.List
import Data.Hash.MD5

keyList :: String -> [(Int, String)]
keyList secretKey =
    [(x, secretKey ++ show x) | x <- [1..] :: [Int]]

startsWithNZeroes :: Int -> String -> Bool
startsWithNZeroes n =
    (== replicate n '0') . take n

findKeyWithSecretness :: (String -> Bool) -> String -> Maybe Int
findKeyWithSecretness secretnessLevel secretKey =
    fmap fst $ find (secretnessLevel . snd) $ map (fmap $ md5s . Str) (keyList secretKey)

findSecretKey :: String -> Maybe Int
findSecretKey =
    findKeyWithSecretness $ startsWithNZeroes 5

findSecreterKey :: String -> Maybe Int
findSecreterKey =
    findKeyWithSecretness $ startsWithNZeroes 6

-- Integrations

myHash :: String
myHash =
    "bgvyzdsv"

partOne :: IO ()
partOne =
    print $ findSecretKey myHash
    
partTwo :: IO ()
partTwo =
    print $ findSecreterKey myHash
