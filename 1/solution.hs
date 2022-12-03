import System.IO
import Data.List (sort)

q1 :: IO Int
q1 = (solve 1 0 0 0) <$> readInputFile 

q2 :: IO Int
q2 =  sum.(take 3).reverse.sort.(solve2 [0]) <$> readInputFile 

readInputFile :: IO [String]
readInputFile = do
        handle <- openFile "input.txt" ReadMode
        contents <- fmap lines (hGetContents handle)
        return contents


solve :: Int -> Int -> Int -> Int -> [String] -> Int
solve _ _ _ maxCal [] = maxCal
solve currentElf currentCal maxElf maxCal (s:str)
        | s == "" = if currentCal > maxCal 
                    then solve (currentElf+1) 0 currentElf currentCal str 
                    else solve (currentElf+1) 0 maxElf maxCal str
        | otherwise = solve currentElf (currentCal + (read s)) maxElf maxCal str

solve2 :: [Int] -> [String] -> [Int]
solve2 a [] = a
solve2 (c:cs) (s:str)
        | s == "" = solve2 (0:c:cs) str
        | otherwise = solve2 ((c+read s):cs) str

