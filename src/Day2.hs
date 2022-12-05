module Day2 (part1, part2) where
import Data.List ( sort )
import Data.List.Split ( splitOn )

parseInput :: String -> [[String]]
parseInput raw = fmap (splitOn " ") $ lines raw

-- replace letters with integer values of corresponding shapes
replaceWithValue :: String -> Integer
replaceWithValue a
    | a == "A" || a == "X" = 0
    | a == "B" || a == "Y" = 1
    | a == "C" || a == "Z" = 2

replaceValues :: [[String]] -> [[Integer]]
replaceValues = fmap (fmap replaceWithValue)

-- calculate score for a single round
calculateRound :: [Integer] -> Integer
calculateRound [a, b]
    | a == b = 3 + b + 1 -- draw
    | mod (a + 1) 3 == mod b 3 = 6 + b + 1 -- win
    | otherwise = b + 1 -- lose

-- calculate single round for part 2
calculateRound2 :: [Integer] -> Integer
calculateRound2 [a, b]
    | b == 1 = 3 + a + 1 -- draw
    | b == 2 = 6 + (mod (a + 1) 3) + 1 -- win
    | otherwise = (mod (a - 1 + 3) 3) + 1 -- lose

part1 :: IO ()
part1 = do
    rawData <- readFile "input/day2.txt"
    let parsedInput = replaceValues $ parseInput rawData

    putStrLn "# Day 2 / Part 1"
    print $ sum $ fmap calculateRound parsedInput

part2 :: IO ()
part2 = do
    rawData <- readFile "input/day2.txt"
    let parsedInput = replaceValues $ parseInput rawData

    putStrLn "# Day 2 / Part 2"
    print $ sum $ fmap calculateRound2 parsedInput
