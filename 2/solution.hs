import System.IO

q1 :: IO Int
q1 = sum . fmap pointFromRound <$> readInputFile

q2 :: IO Int
q2 = sum . fmap (pointFromRound.convertToRound) <$> readInputFile

readInputFile :: IO [[String]]
readInputFile = do
        handle <- openFile "input.txt" ReadMode
        contents <-  (fmap lines (hGetContents handle))
        return (fmap words contents)

pointFromPick :: [String] -> Int
pointFromPick [_,"X"] = 1
pointFromPick [_,"Y"] = 2
pointFromPick [_,"Z"] = 3

pointFromResult :: [String] -> Int
pointFromResult ["A", "X"] = 3
pointFromResult ["B", "Y"] = 3
pointFromResult ["C", "Z"] = 3

pointFromResult ["A", "Z"] = 0
pointFromResult ["B", "X"] = 0
pointFromResult ["C", "Y"] = 0

pointFromResult ["A", "Y"] = 6
pointFromResult ["B", "Z"] = 6
pointFromResult ["C", "X"] = 6

convertToRound :: [String] -> [String]
convertToRound ["A", "X"] = ["A", "Z"]
convertToRound ["A", "Y"] = ["A", "X"]
convertToRound ["A", "Z"] = ["A", "Y"]

convertToRound ["B", "X"] = ["B", "X"]
convertToRound ["B", "Y"] = ["B", "Y"]
convertToRound ["B", "Z"] = ["B", "Z"]

convertToRound ["C", "X"] = ["C", "Y"]
convertToRound ["C", "Y"] = ["C", "Z"]
convertToRound ["C", "Z"] = ["C", "X"]

pointFromRound :: [String] -> Int
pointFromRound round = pointFromResult round + pointFromPick round

