module Day5 (part1, part2) where
import Data.List (find, transpose)
import Data.List.Split (splitWhen, chunksOf, splitOn)

parseInput :: String -> [[String]]
parseInput raw = splitWhen null $ lines raw

parseRow :: [[Char]] -> [Char]
parseRow = fmap (\x -> x!!1)

removeWhitespace :: String -> String
removeWhitespace = filter (\x -> x /= ' ')

parseCommand :: String -> (String, String, String)
parseCommand = fmap (\x -> (x!!1, x!!3, x!!5)) $ splitOn " "

convertCommandToInt :: (String, String, String) -> (Int, Int, Int)
convertCommandToInt (a, b, c) = (read a, read b, read c)

-- move N blocks at once from one stack to another
moveBlocks :: [String] -> (Int, Int, Int) -> [String]
moveBlocks state (count, from, to) = do
    let fromIndex = from - 1
    let toIndex = to - 1
    let movableBlocks = take count (state!!fromIndex)
    let updatedFrom = drop count (state!!fromIndex)
    let updatedTo = movableBlocks ++ (state!!toIndex)
    let updatedState = (take fromIndex state) ++ updatedFrom : drop (fromIndex + 1) state
    (take toIndex updatedState) ++ updatedTo : drop (toIndex + 1) updatedState

executeCommand :: [String] -> (Int, Int, Int) -> [String]
executeCommand state (count, from, to)
    | count == 1 = moveBlocks state (count, from, to)
    | otherwise = do
        let newState = moveBlocks state (1, from, to)
        executeCommand newState (count - 1, from, to)

part1 :: IO ()
part1 = do
    rawData <- readFile "input/day5.txt"
    let [state, commands] = parseInput rawData
    let stateWithColumns = map (\c -> chunksOf 4 c) state
    
    let cleanState = map removeWhitespace $ transpose $ init $ map parseRow stateWithColumns
    let cleanCommands = map convertCommandToInt $ map parseCommand commands
    let finalState = foldl executeCommand cleanState cleanCommands

    putStrLn "# Day 5 / Part 1"
    print $ map head finalState

part2 :: IO ()
part2 = do
    rawData <- readFile "input/day5.txt"
    let [state, commands] = parseInput rawData
    let stateWithColumns = map (\c -> chunksOf 4 c) state
    
    let cleanState = map removeWhitespace $ transpose $ init $ map parseRow stateWithColumns
    let cleanCommands = map convertCommandToInt $ map parseCommand commands
    let finalState = foldl moveBlocks cleanState cleanCommands

    putStrLn "# Day 5 / Part 2"
    print $ map head finalState
