module Day3 (part1, part2) where
import Data.List (find)
import Data.List.Split (chunksOf)
import Data.Char (ord, isUpper)

parseInput :: String -> [String]
parseInput raw = lines raw

-- split str into two even parts
splitStr :: String -> ([Char], [Char])
splitStr inputStr = splitAt ((length inputStr) `div` 2) inputStr

splitIntoGroups :: [String] -> [[String]]
splitIntoGroups = chunksOf 3

-- find the first duplicate item
findDuplicateItem :: ([Char], [Char]) -> Maybe Char
findDuplicateItem (a, b) = find (\x -> elem x b) a

-- badge is an item which is present in every group
findBadgeItem :: [[Char]] -> Maybe Char
findBadgeItem [a, b, c] = find (\x -> elem x b && elem x c) a

-- convert character to priority
getItemValue :: Char -> Int
getItemValue item
    | isUpper item = (ord item) - 38 -- uppercase
    | otherwise = (ord item) - 96 -- lowercase

getPriority :: Maybe Char -> Int
getPriority item = case item of
    Just value -> getItemValue value
    Nothing -> 0

part1 :: IO ()
part1 = do
    rawData <- readFile "input/day3.txt"
    let parsedInput = fmap splitStr $ parseInput rawData

    putStrLn "# Day 3 / Part 1"
    print $ sum
        $ fmap getPriority
        $ fmap findDuplicateItem parsedInput

part2 :: IO ()
part2 = do
    rawData <- readFile "input/day3.txt"
    let parsedInput = parseInput rawData

    putStrLn "# Day 3 / Part 2"
    print $ sum
        $ fmap getPriority
        $ fmap findBadgeItem
        $ splitIntoGroups parsedInput
