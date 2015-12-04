module Advent where
    
import Data.List
import Control.Applicative

-- --------------
-- Business Logic
-- --------------

data Direction
    = North
    | East
    | South
    | West
    deriving (Show)
    
data Location = Location
    { x :: Int
    , y :: Int
    } deriving (Eq, Show)

-- Turns a singleton into a tuple by repeating it twice
link :: a -> (a, a)
link a = 
    (a, a)

evenElements :: [a] -> [a]
evenElements list =
    map snd $ filter ((== 0) . (`mod` 2) . fst) $ zip ([0..] :: [Int]) list
    
oddElements :: [a] -> [a]
oddElements list =
    map snd $ filter ((== 1) . (`mod` 2) . fst) $ zip ([0..] :: [Int]) list

-- Tears an list into two lists, alternating every other element
tear :: [a] -> ([a], [a])
tear list =
    (evenElements list, oddElements list)

charToDirection :: Char -> Maybe Direction
charToDirection c
    | c == '^'  = Just North
    | c == '>'  = Just East
    | c == 'v'  = Just South
    | c == '<'  = Just West
    | otherwise = Nothing

strToDirectionList :: String -> Maybe [Direction]
strToDirectionList =
    mapM charToDirection

moveInDirection :: Location -> Direction -> Location
moveInDirection start dir =
    case dir of
        North -> start { y = y start + 1 }
        East  -> start { x = x start + 1 }
        South -> start { y = y start - 1 }
        West  -> start { x = x start - 1 }

travel :: [Direction] -> [Location]
travel directions =
    initialLocation : snd visitedLocations
        where
            initialLocation =
                Location 0 0
            visitedLocations =
                -- mapAccumL expects the accumulator to return a tuple, even if both
                -- elements in the tuple are identical
                mapAccumL (\loc dir -> link $ moveInDirection loc dir) initialLocation directions

-- ---------------
-- Implementations
-- ---------------

partOne :: IO ()
partOne = do
    input <- strToDirectionList <$> readFile "input.txt"
    print $ (length . nub . travel) <$> input

partTwo :: IO ()
partTwo = do
    input <- strToDirectionList <$> readFile "input.txt"
    print $ (length . nub) <$> combinedVisists input
        where
            combinedVisists directions =
                liftA2 (++) (santaVisits <$> directions) (roboSantaVisists <$> directions)
            santaVisits =
                travel . fst . tear
            roboSantaVisists =
                travel . snd . tear
