module Day6 (part1, part2) where
import Data.List (nub, find)

isUnique :: String -> Bool
isUnique marker = length marker == length (nub marker)

checkForStartMarker :: String -> Bool
checkForStartMarker datastream = isUnique $ take 4 datastream

checkForMessageStartMarker :: String -> Bool
checkForMessageStartMarker datastream = isUnique $ take 14 datastream

getMarkerEnd :: Maybe Int -> Int
getMarkerEnd targetIndex = case targetIndex of
    Just value -> value + 4
    Nothing -> 0

getMessageMarkerEnd :: Maybe Int -> Int
getMessageMarkerEnd targetIndex = case targetIndex of
    Just value -> value + 14
    Nothing -> 0

part1 :: IO ()
part1 = do
    rawData <- readFile "input/day6.txt"
    let indices = [0..(length rawData)]
    let targetIndex = find (\i -> checkForStartMarker (drop i rawData)) indices

    putStrLn "# Day 6 / Part 1"
    print $ getMarkerEnd targetIndex

part2 :: IO ()
part2 = do
    rawData <- readFile "input/day6.txt"
    let indices = [0..(length rawData)]
    let targetIndex = find (\i -> checkForMessageStartMarker (drop i rawData)) indices

    putStrLn "# Day 6 / Part 2"
    print $ getMessageMarkerEnd targetIndex
