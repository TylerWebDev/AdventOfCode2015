module Advent where

import Control.Monad
import Data.List

say :: String -> String
say str =
    let encode s = [show $ length s, take 1 s] in
        concat $ (group >=> encode) str

getNthSayIteration :: Int -> String
getNthSayIteration n =
    iterate say "1321131112" !! n

-- Implementations

partOne :: IO ()
partOne =
    print (length $ getNthSayIteration 40)

partTwo :: IO ()
partTwo =
    print (length $ getNthSayIteration 50)