module Day1 (part1, part2) where
import Data.List ( sort )
import Data.List.Split ( splitWhen )

parseInput :: String -> [[Integer]]
parseInput raw = fmap (fmap (read)) $ splitWhen null $ lines raw

part1 :: IO ()
part1 = do
    rawData <- readFile "input/day1.txt"
    let parsedInput = parseInput rawData
    let listOfSums = fmap sum parsedInput

    putStrLn "# Day 1 / Part 1"
    print $ maximum listOfSums

part2 :: IO ()
part2 = do
    rawData <- readFile "input/day1.txt"
    let parsedInput = parseInput rawData
    let listOfSums = fmap sum parsedInput

    putStrLn "# Day 1 / Part 2"
    print $ sum $ take 3 $ reverse $ sort listOfSums
