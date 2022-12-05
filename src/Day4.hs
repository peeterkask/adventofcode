module Day4 (part1, part2) where
import Data.List.Split ( splitOn )

parseInput :: String -> [String]
parseInput raw = lines raw

-- split input lines into pair of values
splitIntoPairs :: String -> [[String]]
splitIntoPairs inputLine = fmap (\x -> splitOn "-" x)
    $ splitOn "," inputLine

-- parse integer values from string inputs
parseRangeValues :: [String] -> [Int]
parseRangeValues pair = fmap read pair

-- check if any of the pairs is fully contained by other pair
isFullyContained :: [Int] -> [Int] -> Bool
isFullyContained [a1, a2] [b1, b2] =
    (a1 >= b1 && a1 <= b2 && a2 >= b1 && a2 <= b2)
    || (b1 >= a1 && b1 <= a2 && b2 >= a1 && b2 <= a2)

-- check if any of the pairs is overlapping at all
isOverlapping :: [Int] -> [Int] -> Bool
isOverlapping [a1, a2] [b1, b2] =
    (a1 >= b1 && a1 <= b2 || a2 >= b1 && a2 <= b2)
    || (b1 >= a1 && b1 <= a2 || b2 >= a1 && b2 <= a2)

part1 :: IO ()
part1 = do
    rawData <- readFile "input/day4.txt"
    let parsedInput = fmap splitIntoPairs $ parseInput rawData
    let parsedPairs = fmap (\x -> fmap parseRangeValues x) parsedInput
    let containedPairs = filter (\[p1, p2] -> (isFullyContained p1 p2)) parsedPairs

    putStrLn "# Day 4 / Part 1"
    print $ length containedPairs

part2 :: IO ()
part2 = do
    rawData <- readFile "input/day4.txt"
    let parsedInput = fmap splitIntoPairs $ parseInput rawData
    let parsedPairs = fmap (\x -> fmap parseRangeValues x) parsedInput
    let overlappingPairs = filter (\[p1, p2] -> (isOverlapping p1 p2)) parsedPairs

    putStrLn "# Day 4 / Part 2"
    print $ length overlappingPairs
