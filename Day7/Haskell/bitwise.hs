module Main where

import Wires
import Parse
import Text.Parsec
import Data.Map as Map

main :: IO ()
main = do
    input <- mapM (parse wireParser "Invalid Wire") <$> lines <$> readFile "input.txt"
    case input of
        Right wires -> print $ lookupEval (Map.fromList wires) "a"
        Left _ -> print "Failed to parse wire diagram"
