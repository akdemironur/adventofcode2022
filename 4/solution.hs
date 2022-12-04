import System.IO

q1 :: IO Int
q1 = length . (filter isFullyOverlapping) <$> readInputFile

q2 :: IO Int
q2 = length . (filter isOverlapping) <$> readInputFile

readInputFile :: IO [((Int, Int), (Int, Int))]
readInputFile = do
        handle <- openFile "input.txt" ReadMode
        contents <- (fmap parsePairs) . lines <$> (hGetContents handle)
        return contents

parsePairs :: String -> ((Int, Int), (Int, Int))
parsePairs str = ((read fe1, read fe2), (read se1, read se2))
    where
        (firstElf, secondElf) = splitStrAtChar str ','
        (fe1, fe2) = splitStrAtChar firstElf '-'
        (se1, se2) = splitStrAtChar secondElf '-'

splitStrAtChar :: String -> Char -> (String, String)
splitStrAtChar str c = (firstPart, secondPart)
    where
        firstPart = takeWhile ((/=) c) str
        secondPart = tail $ dropWhile ((/=) c) str

isFullyOverlapping :: ((Int, Int), (Int, Int)) -> Bool
isFullyOverlapping ((a, b), (c, d)) = ((a <= c) && (b >= d)) || ((c <= a) && (d >= b))

isOverlapping :: ((Int, Int), (Int, Int)) -> Bool
isOverlapping ((a, b), (c, d)) = ((a <= c) && (b >= c)) || ((c <= a) && (d >= a))
