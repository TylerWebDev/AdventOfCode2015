module Advent where

import Parse
import Data.Vector (Vector, fromList, (//), (!))
import qualified Data.Vector as Vector

-- Coordinates are in the format (Row, Column)

gridDimensions :: (Int, Int)
gridDimensions =
    (1000, 1000)

data Light
    = On
    | Off
    deriving (Eq, Show)

invert :: Light -> Light
invert On = 
    Off
invert Off = 
    On
    
decrement :: Int -> Int -> Int
decrement a b 
    | b - a >= 0 = b - a
    | otherwise  = 0

inverseOfRange :: [Int] -> Vector Light -> [(Int, Light)]
inverseOfRange range lights =
    zip range $ map (invert . (lights !)) range

alterRange :: (Int -> Int) -> [Int] -> Vector Int -> [(Int, Int)]
alterRange alteration range lights =
    zip range $ map (alteration . (lights !)) range

rowColToIndex :: (Int, Int) -> Int
rowColToIndex (row,col) =
    (row * fst gridDimensions) + col

grid :: Vector Light
grid =
    fromList $ map (const Off) ([0..(uncurry (*) gridDimensions - 1)] :: [Int])
    
fancyGrid :: Vector Int
fancyGrid =
    Vector.map (const 0) grid

-- Create a row/col range from two points that we can then convert to indeces
-- (499, 499) (500, 500)
-- [(499, 499), (499, 500), (500, 499), (500, 500)]

createRowColRange :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
createRowColRange (x1, y1) (x2, y2) =
    [(x, y) | x <- [x1..x2], y <- [y1..y2]]
    
changeLightGrid :: Vector Light -> Instruction -> Vector Light
changeLightGrid originalGrid instruction =
    let range = map rowColToIndex $ createRowColRange (from instruction) (to instruction) in
        case change instruction of
            TurnOn  -> originalGrid // zip range (repeat On)
            TurnOff -> originalGrid // zip range (repeat Off)
            Toggle  -> originalGrid // inverseOfRange range originalGrid
            
changeLightGridFancy :: Vector Int -> Instruction -> Vector Int
changeLightGridFancy originalGrid instruction =
    let range = map rowColToIndex $ createRowColRange (from instruction) (to instruction) in
        case change instruction of
            TurnOn  -> originalGrid // alterRange (+1) range originalGrid
            TurnOff -> originalGrid // alterRange (decrement 1) range originalGrid
            Toggle  -> originalGrid // alterRange (+2) range originalGrid
            
-- Implementations

getInput :: IO (Maybe [Instruction])
getInput =
    mapM parseInstruction . lines <$> readFile "input.txt"

partOne :: IO ()
partOne = do
    input <- getInput
    case input of
        Just instructionList -> print (length $ Vector.filter (== On) $ foldl changeLightGrid grid instructionList)
        Nothing -> print "Error"
    
partTwo :: IO ()
partTwo = do
    input <- getInput
    case input of
        Just instructionList -> print $ Vector.foldl (+) 0 (foldl changeLightGridFancy fancyGrid instructionList)
        Nothing -> print "Error"