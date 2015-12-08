module Advent where

encodedLength :: String -> Int
encodedLength ('\\':'\"':chars) =
    2 + encodedLength chars
encodedLength ('\\':'\\':chars) =
    2 + encodedLength chars
encodedLength ('\\':'x':_:_:chars) =
    4 + encodedLength chars
encodedLength (_:chars) =
    1 + encodedLength chars
encodedLength [] =
    0

decodedLength :: String -> Int
decodedLength ('\"':chars) =
    0 + decodedLength chars
decodedLength ('\\':'\\':chars) =
    1 + decodedLength chars
decodedLength ('\\':'x':_:_:chars) =
    1 + decodedLength chars
decodedLength (_:chars) =
    1 + decodedLength chars
decodedLength [] =
    0

reEncodedLength :: String -> Int
reEncodedLength ('\"':chars) =
    3 + reEncodedLength chars
reEncodedLength ('\\':'\\':chars) =
    4 + reEncodedLength chars
reEncodedLength ('\\':'x':_:_:chars) =
    5 + reEncodedLength chars
reEncodedLength (_:chars) =
    1 + reEncodedLength chars
reEncodedLength [] =
    0

diffInLengths :: (String -> Int) -> (String -> Int) -> [String] -> Int
diffInLengths fa fb strs =
    (sum . map fa $ strs) - (sum . map fb $ strs)

partOne :: IO ()
partOne = do
    input <- lines <$> readFile "input.txt"
    print $ diffInLengths encodedLength decodedLength input

partTwo :: IO ()
partTwo = do
    input <- lines <$> readFile "input.txt"
    print $ diffInLengths reEncodedLength encodedLength input
