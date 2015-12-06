{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Applicative
import Text.Parsec
    
data Change
    = TurnOn
    | TurnOff
    | Toggle
    deriving (Show)

data Instruction = Instruction
    { change :: Change
    , from :: (Int, Int)
    , to :: (Int, Int)
    } deriving (Show)

{-
Parse Rules:
Expecting ->
    'toggle'
    'turn off'
    'turn on'
Followed By ->
    ' '
Followed By ->
    '###,###'
Followed By ->
    ' through '
Followed By ->
    '###,###'
-}

strToInstruction :: String -> Change
strToInstruction "toggle" =
    Toggle
strToInstruction "turn on" =
    TurnOn
strToInstruction _ =
    TurnOff

parseTuple :: Parsec String () (Int, Int)
parseTuple =
    liftA2 (,) (read <$> many1 digit) (char ',' >> (read <$> many1 digit))
    
instructionParser :: Parsec String () Instruction
instructionParser = do
    instructionString <- choice
        [ try $ string "toggle"
        , try $ string "turn on"
        , string "turn off"
        ]
    spaces
    firstPair  <- parseTuple
    _          <- string " through "
    secondPair <- parseTuple
    return $ Instruction (strToInstruction instructionString) firstPair secondPair

parseInstruction :: String -> Maybe Instruction
parseInstruction str =
    case parse instructionParser "Failed to Parse Instruction" str of
        Right instruction -> Just instruction
        Left _            -> Nothing
