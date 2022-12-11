module Day7 (part1, part2) where
import Data.List (isPrefixOf, isSuffixOf, intercalate, find)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map

type File = (String, Int)
type Dirs = Map.Map String Int

parseInput :: String -> [String]
parseInput raw = lines raw

changeDir :: String -> String -> String
changeDir currentDir currentLine
    | isSuffixOf " /" currentLine = "/"
    | isSuffixOf " .." currentLine = (intercalate "/" $ init $ init $ splitOn "/" currentDir) ++ "/"
    | otherwise = currentDir ++ (last (splitOn " " currentLine)) ++ "/"

handleLsCmd :: String -> String -> [File] -> [File]
handleLsCmd currentLine currentDir files
    | isPrefixOf "dir " currentLine = files
    | otherwise = files ++ [(
            currentDir ++ (last (splitOn " " currentLine)),
            read $ head (splitOn " " currentLine)
        )]

parseCmdOutput :: String -> String -> String -> [File] -> [File]
parseCmdOutput cmd currentLine currentDir files
    | cmd == "ls" = handleLsCmd currentLine currentDir files

handleLine :: (String, [File], String) -> String -> (String, [File], String)
handleLine (currentDir, files, prevCmd) currentLine
    | isPrefixOf "$ cd" currentLine = (changeDir currentDir currentLine, files, "cd")
    | isPrefixOf "$ ls" currentLine = (currentDir, files, "ls")
    | otherwise = (currentDir, (parseCmdOutput prevCmd currentLine currentDir files), prevCmd)

addDir :: Dirs -> String -> Int -> Dirs
addDir dirs dirname filesize = case Map.lookup dirname dirs of
    Just oldSize -> Map.adjust (filesize +) dirname dirs
    Nothing -> Map.insert dirname filesize dirs

generateDirList :: Dirs -> File -> Dirs
generateDirList dirs (filename, filesize) = do
    let pathParts = init $ splitOn "/" filename
    let (newDirs, _) = foldl (\(cDirs, cPart) nextPart -> ((addDir cDirs (cPart ++ nextPart ++ "/") filesize), (cPart ++ nextPart ++ "/"))) (dirs, "") pathParts
    newDirs

spaceRequired :: Dirs -> Int
spaceRequired dirs = case Map.lookup "/" dirs of
    Just totalSpace -> 30000000 - (70000000 - totalSpace)
    Nothing -> 30000000

checkDir :: [Int] -> Int -> [Int]
checkDir currentSizes size = currentSizes

part1 :: IO ()
part1 = do
    rawData <- readFile "input/day7.txt"
    let parsedInput = parseInput rawData
    let (currentDir, files, prevCmd) = foldl handleLine ("", [], "") parsedInput
    let directories = foldl generateDirList Map.empty files
    let smallDirectories = Map.filter (<= 100000) directories

    putStrLn "# Day 7 / Part 1"
    print $ sum $ Map.elems smallDirectories

part2 :: IO ()
part2 = do
    rawData <- readFile "input/day7.txt"
    let parsedInput = parseInput rawData
    let (currentDir, files, prevCmd) = foldl handleLine ("", [], "") parsedInput
    let directories = foldl generateDirList Map.empty files

    let requiredSize = spaceRequired directories
    let availableSizes = filter (\(dir, size) -> size > requiredSize) $ Map.toList directories
    let minPossible = minimum $ map (\(dir, size) -> size) availableSizes

    putStrLn "# Day 7 / Part 2"
    print minPossible
