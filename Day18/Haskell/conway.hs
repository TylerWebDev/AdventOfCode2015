module Advent where

import Data.Maybe
import Data.List.Split
import qualified Data.Map as Map

data Light
    = On
    | Off
    deriving (Show, Eq)

lightToChar :: Light -> Char
lightToChar On =
    '#'
lightToChar Off =
    '.'

charToLight :: Char -> Light
charToLight '#' =
    On
charToLight _ =
    Off

data Point =
    Point Int Int
    deriving (Show, Eq, Ord)

-- Because of my naive and quick implementation, you have to set the size of the
-- grid you're expecting to load explicitly
gridLength :: Int
gridLength =
    100
gridHeight :: Int
gridHeight =
    100

-- Creates a list of all the points on the grid
gridPoints :: [Point]
gridPoints =
    [Point x y | x <- [0..gridLength - 1], y <- [0..gridHeight - 1]]

-- Gets the coordinates of all neighbors of a point
neighborLocations :: Point -> [Point]
neighborLocations (Point locX locY) =
    [Point x y | x <- [locX - 1 .. locX + 1], y <- [locY - 1 .. locY + 1], x /= locX || y /= locY]

-- Gets the status, On or Off, or a point on a grid
-- If the light is outside the bounds of the grid, then the light is Off as
-- defined by the specification
lightStatus :: Map.Map Point Light -> Point -> Light
lightStatus environment loc =
    fromMaybe Off (Map.lookup loc environment)

-- Gets the neighbors of a light as an array of lights
getNeighbors :: Map.Map Point Light -> Point -> [Light]
getNeighbors environment loc =
    map (lightStatus environment) $ neighborLocations loc

-- Given a point on the grid, determine its new state after a step is taken
updateLight :: Map.Map Point Light -> Point -> Light
updateLight environment loc =
    case Map.lookup loc environment of
        Just On ->
            if numNeighborsOn == 2 || numNeighborsOn == 3 then On else Off
        Just Off ->
            if numNeighborsOn == 3 then On else Off
        Nothing ->
            Off
        where
            numNeighborsOn =
                length $ filter (== On) $ getNeighbors environment loc

-- From a starting grid, take a step forward
step :: Map.Map Point Light -> Map.Map Point Light
step startingGrid =
    Map.fromList $ zip gridPoints $ map (updateLight startingGrid) gridPoints

-- Given a grid, draw it to the console
-- This is outside the scope of the advent problem, but it is cool nonetheless
drawGrid :: Map.Map Point Light -> IO ()
drawGrid environment =
    mapM_ print $ chunksOf gridLength (map gridPointToChar $ Map.toList environment)
    where
        gridPointToChar :: (Point, Light) -> Char
        gridPointToChar (_, light) =
            lightToChar light

-- Read a grid from input.txt
readGrid :: IO (Map.Map Point Light)
readGrid = do
    raw <- readFile "input.txt"
    return (Map.fromList $ zip gridPoints $ map charToLight . concat . lines $ raw)

run :: Int -> IO [Map.Map Point Light]
run numSteps = do
    grid <- readGrid
    return (take (numSteps + 1) $ iterate step grid)

-- --------------
-- Implementation
-- --------------

partOne :: IO ()
partOne = do
    answer <- (length . filter ((== On) . snd) . Map.toList . last) <$> run 100
    print answer
