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

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, b) = (f a, f b)

parsePairs :: String -> ((Int, Int), (Int, Int))
parsePairs = mapTuple ((mapTuple read) . (splitStrAtChar '-')) . (splitStrAtChar ',')

splitStrAtChar :: Char -> String -> (String, String)
splitStrAtChar c str = (firstPart, secondPart)
    where
        firstPart = takeWhile ((/=) c) str
        secondPart = tail $ dropWhile ((/=) c) str

isFullyOverlapping :: ((Int, Int), (Int, Int)) -> Bool
isFullyOverlapping ((a, b), (c, d)) = ((a <= c) && (b >= d)) || ((c <= a) && (d >= b))

isOverlapping :: ((Int, Int), (Int, Int)) -> Bool
isOverlapping ((a, b), (c, d)) = ((a <= c) && (b >= c)) || ((c <= a) && (d >= a))
