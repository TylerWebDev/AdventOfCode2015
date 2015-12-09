module Advent where

import Parse
import Data.List
import Data.Function.Memoize
import Control.Applicative
import qualified Data.Map as Map

createDistanceMap :: [String] -> Maybe (Map.Map (String, String) Int)
createDistanceMap strs =
    Map.fromList <$> liftA2 (++) (mapM (fmap fst . parseCityDistance) strs) (mapM (fmap snd . parseCityDistance) strs)

travel :: Map.Map (String, String) Int -> [String] -> Int
travel distanceMap =
    evaluateDistanceMemoized
        where
            evaluateDistanceMemoized =
                memoize evaluateDistance
            evaluateDistance :: [String] -> Int
            evaluateDistance (a:b:xs) =
                case Map.lookup (a, b) distanceMap of
                    Just distance ->
                        distance + evaluateDistanceMemoized (b:xs)
                    Nothing ->
                        -1
            evaluateDistance [_] =
                0
            evaluateDistance [] =
                0

asList :: (a, a) -> [a]
asList (a, b) =
    [a, b]

partN :: ([Int] -> Int) -> IO ()
partN transformer = do
    maybeDistanceMap <- createDistanceMap . lines <$> readFile "input.txt"
    case maybeDistanceMap of
        Just distanceMap ->
            let cities = nub . (>>= asList) . Map.keys $ distanceMap in
                print . transformer $ map (travel distanceMap) (permutations cities)
        Nothing ->
            print "Problem parsing distance map"

partOne :: IO ()
partOne =
    partN minimum

partTwo :: IO ()
partTwo =
    partN maximum
