module Advent where
    
import Data.List.Split
    
data Dimension = Dimension
    { l :: Int
    , w :: Int
    , h :: Int
    } deriving (Show)

testCase :: String
testCase =
    "2x3x4"

strToDimension :: String -> Dimension
strToDimension dimensionString =
    Dimension dimL dimW dimH
        where
            dimL           = read . head $ dimensionArray
            dimW           = read $ dimensionArray !! 1
            dimH           = read $ dimensionArray !! 2
            dimensionArray = splitOn "x" dimensionString

volume :: Dimension -> Int
volume d = 
    l d * w d * h d

perimeters :: Dimension -> [Int]
perimeters d =
    [ 2 * l d + 2 * w d
    , 2 * w d + 2 * h d
    , 2 * h d + 2 * l d
    ]

faceAreas :: Dimension -> [Int]
faceAreas d =
    [ l d * w d
    , w d * h d
    , h d * l d
    ]

surfaceArea :: Dimension -> Int
surfaceArea =
    sum . map (*2) . faceAreas
    
slack :: Dimension -> Int
slack =
    minimum . faceAreas

paperRequired :: Dimension -> Int
paperRequired dim =
    surfaceArea dim + slack dim
    
ribbon :: Dimension -> Int
ribbon =
    minimum . perimeters
    
ribbonBow :: Dimension -> Int
ribbonBow =
    volume
    
ribbonRequired :: Dimension -> Int
ribbonRequired dim =
    ribbon dim + ribbonBow dim

-- Implementation from Files

partOne :: IO ()
partOne = do
    input <- readFile "input.txt"
    print $ sum (map (paperRequired . strToDimension) $ lines input)

partTwo :: IO ()
partTwo = do
    input <- readFile "input.txt"
    print $ sum (map (ribbonRequired . strToDimension) $ lines input)
