import System.IO  
import Data.Char

q1 :: IO Int
q1 = (sum . (fmap totalPriority))  <$> readInputFile

q2 :: IO Int
q2 = sum . (fmap (priority.commonItemOf3)) . (chunksOf 3) <$> readInputFile

readInputFile :: IO [String]
readInputFile = do
        handle <- openFile "input.txt" ReadMode
        contents <- lines <$> (hGetContents handle)
        return contents

priority :: Char -> Int
priority c 
    | isUpper c = ord c - 38
    | isLower c = ord c - 96
    | otherwise = error "Not a letter"

splitAtHalf :: String -> (String, String)
splitAtHalf str = splitAt (length str `div` 2) str

commonItems :: (String, String) -> Char
commonItems (a, b) = head $ filter (\c -> elem c b) a

totalPriority :: String -> Int
totalPriority = priority.commonItems.splitAtHalf

chunksOf :: Int -> [a] -> [[a]]
chunksOf = chunksOfHelper []
    where
        chunksOfHelper acc _ [] = acc
        chunksOfHelper acc x n = chunksOfHelper (acc++[take x n]) x (drop x n)

commonItemOf3 :: [String] -> Char
commonItemOf3 [s1, s2, s3] = head $ filter (\c -> elem c s2 && elem c s3) s1
commonItemOf3 _ = error "Invalid input"
