module Parse where

import Control.Applicative (liftA2)
import Data.Functor.Identity
import Text.Parsec
import Wires

type ParsecPartial =
    ParsecT String () Identity

assignmentArrow :: ParsecPartial String
assignmentArrow =
    string " -> "

tupleify :: b -> a -> (a, b)
tupleify =
    flip (,)

readKey :: ParsecPartial String
readKey =
    many1 letter

readDualVal :: String -> (String -> String -> Wire String) -> ParsecPartial (String, Wire String)
readDualVal separator constructor = do
    valA <- manyTill anyChar $ string separator
    valB <- manyTill anyChar assignmentArrow
    key  <- readKey
    return (key, constructor valA valB)

constParser :: ParsecPartial (String, Wire String)
constParser =
    liftA2 tupleify (Const . read <$> manyTill digit assignmentArrow) readKey

linkParser :: ParsecPartial (String, Wire String)
linkParser =
    liftA2 tupleify (Link <$> manyTill letter assignmentArrow) readKey

notParser :: ParsecPartial (String, Wire String)
notParser =
    liftA2 tupleify (string "NOT " *> (Not <$> manyTill letter assignmentArrow)) readKey

andParser :: ParsecPartial (String, Wire String)
andParser =
    readDualVal " AND " And

orParser :: ParsecPartial (String, Wire String)
orParser =
    readDualVal " OR " Or

lShiftParser :: ParsecPartial (String, Wire String)
lShiftParser =
    readDualVal " LSHIFT " (\wireLink shiftAmt -> LShift wireLink $ read shiftAmt)

rShiftParser :: ParsecPartial (String, Wire String)
rShiftParser =
    readDualVal " RSHIFT " (\wireLink shiftAmt -> RShift wireLink $ read shiftAmt)

wireParser :: ParsecPartial (String, Wire String)
wireParser =
    choice
        [ try constParser
        , try linkParser
        , try notParser
        , try andParser
        , try orParser
        , try lShiftParser
        , rShiftParser
        ]
