module Advent where

import Wires
import Parse
import Text.Parsec
import Data.Map as Map

readInput :: IO (Either ParseError [(String, Wire String)])
readInput =
    mapM (parse wireParser "Invalid Wire") <$> lines <$> readFile "input.txt"

partOne :: IO ()
partOne = do
    input <- readInput
    case input of
        Right wires -> print $ lookupEval (Map.fromList wires) "a"
        Left _      -> print "Failed to parse wire diagram"

partTwo :: IO ()
partTwo = do
    input <- readInput
    case input of
        Right wires
            -> case lookupEval (Map.fromList wires) "a" of
                Just a  -> print $ lookupEval (Map.insert "b" (Const a) (Map.fromList wires)) "a"
                Nothing -> print "Failed to evaluate 'a'"
        Left _
            -> print "Failed to parse wire diagram"
