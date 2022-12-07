data FileSystem = Dir String (Maybe Int) [FileSystem] | File String Int deriving (Eq, Show)

inputs = fmap lines (readFile "input.txt")

filesystem = Dir "/" Nothing []

insertToFileSystem :: [String] -> FileSystem -> FileSystem -> FileSystem
insertToFileSystem [] _ mainDir = mainDir
insertToFileSystem _ _ (File a b) = File a b
insertToFileSystem [targetDirName] fs mainDir@(Dir currentDirName dirSize dirContents) = if targetDirName == currentDirName 
                                                                                         then Dir currentDirName dirSize (fs:dirContents) 
                                                                                         else mainDir
insertToFileSystem (targetDirName:ts) fs mainDir@(Dir currentDirName dirSize dirContents) = if targetDirName == currentDirName 
                                                                                            then Dir currentDirName dirSize (fmap (insertToFileSystem ts fs) dirContents) 
                                                                                            else mainDir

changeDirectory :: [String] -> String -> [String]
changeDirectory _ "/" = ["/"]
changeDirectory currentDirectory ".." = init currentDirectory
changeDirectory currentDirectory targetDirectory = currentDirectory ++ [targetDirectory]

blankDir :: String -> FileSystem
blankDir name = Dir name Nothing []

parseLine :: (FileSystem, [String]) -> String -> (FileSystem, [String]) 
parseLine (fileSystem, currentDirectory) command 
    | take 3 command == "dir" = (insertToFileSystem currentDirectory (blankDir (drop 4 command)) fileSystem, currentDirectory)
    | take 4 command == "$ ls" = (fileSystem, currentDirectory)
    | take 4 command == "$ cd" = (fileSystem, changeDirectory currentDirectory (drop 5 command))
    | otherwise = (insertToFileSystem currentDirectory (File ((!!1) . words $ command) (read . (!!0) . words $ command)) fileSystem, currentDirectory)
         
type State = (FileSystem, [String])

initialState :: State
initialState = (filesystem, [])

parsedFS :: IO FileSystem
parsedFS = fst . (foldl parseLine initialState)  <$> inputs

calcSizeFS :: FileSystem -> Int
calcSizeFS (File _ size) = size
calcSizeFS (Dir _ _ []) = 0
calcSizeFS (Dir _ Nothing content) = sum $ fmap calcSizeFS content
calcSizeFS (Dir _ (Just size) _) = size

writeSizeOfDirs :: FileSystem -> FileSystem
writeSizeOfDirs (File a b) = File a b
writeSizeOfDirs dir@(Dir a _ contents) = Dir a (Just $ calcSizeFS dir) (fmap writeSizeOfDirs contents) 

sumOfDirsUnderThreshold :: Int -> FileSystem -> Int
sumOfDirsUnderThreshold threshold (File _ _) = 0
sumOfDirsUnderThreshold threshold (Dir _ Nothing _) = error "This function must be called after writing sizes of directories" 
sumOfDirsUnderThreshold threshold (Dir _ (Just size) contents) = if size <= threshold 
                                                                 then size + (sum $ fmap (sumOfDirsUnderThreshold threshold) contents) 
                                                                 else   (sum $ fmap (sumOfDirsUnderThreshold threshold) contents)
finalFS :: IO FileSystem
finalFS = writeSizeOfDirs <$> parsedFS

findSmallestLarger :: Int -> Int -> FileSystem -> Int
findSmallestLarger _ currentMin (File _ _) = currentMin
findSmallestLarger _ _ (Dir _ Nothing _) =  error "This function must be called after writing sizes of directories" 
findSmallestLarger threshold currentMin (Dir _ (Just size) []) =  if size > threshold && size < currentMin then size else currentMin
findSmallestLarger threshold currentMin (Dir _ (Just size) contents) = if size > threshold && size < currentMin
                                                                  then minimum (fmap (findSmallestLarger threshold size) contents) 
                                                                  else minimum (fmap (findSmallestLarger threshold currentMin) contents) 
getSize :: FileSystem -> Int
getSize = calcSizeFS

updateSize :: Int
updateSize = 30000000

totalSize :: Int
totalSize = 70000000

q1 :: IO Int
q1 = (sumOfDirsUnderThreshold 100000) <$> finalFS
q2 :: IO Int
q2 = (\fs -> findSmallestLarger (updateSize - totalSize + (getSize fs)) (getSize fs) fs) <$> finalFS
     