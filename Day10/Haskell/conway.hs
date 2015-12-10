module Advent where

import Data.List

say :: String -> String
say str =
    let appendHead s = (++ [head s]) in
        concatMap (\i -> appendHead i . show . length $ i) $ group str

sayForeverFrom :: String -> [String]
sayForeverFrom =
    iterate say

getNthSayIteration :: Int -> String
getNthSayIteration n =
    -- We have to give n + 1 as the argument because the first result of 'iterate'
    -- is the identity case, e.g. the 41st element is the 40th application
    last . take (n + 1) $ sayForeverFrom "1321131112"

-- Implementations

partOne :: IO ()
partOne =
    print (length $ getNthSayIteration 40)

partTwo :: IO ()
partTwo =
    print (length $ getNthSayIteration 50)