module Parse where

import Pair
import Text.Parsec

type DistanceTuple =
    ((String, String), Int)

flipped :: DistanceTuple -> DistanceTuple
flipped ((a, b), i) =
    ((b, a), i)

parsePair :: Parsec String () DistanceTuple
parsePair = do
    cityA <- many1 letter <* string " to "
    cityB <- many1 letter <* string " = "
    dist  <- many1 digit
    return ((,) cityA cityB, read dist)

parseCityDistance :: String -> Maybe (DistanceTuple, DistanceTuple)
parseCityDistance str =
    case parse parsePair "" str of
        Right pair -> Just (pair, flipped pair)
        Left _     -> Nothing
