q1 = (\a -> sum $ zipWith (*) (fmap (a!!) [19,59..219]) [20,60..220]) <$> xAfterInstr

q2 = (\a -> (zipWith cycleToChar a (concat . (take 6) $ repeat [0..39]))) <$> xAfterInstr

main = q1 >>= print >> q2 >>= putStrLn

puzzleInput :: IO [String]
puzzleInput = lines <$> readFile "input.txt"

inputToInstr :: [Int] -> [String] -> [Int]
inputToInstr xs [] = xs
inputToInstr xs (str:ss)
    | head str == 'n' = inputToInstr (xs ++ [0]) ss
    | otherwise = inputToInstr (xs ++ [0,read $ drop 5 str]) ss

allInstr = inputToInstr [] <$> puzzleInput

xAfterInstr :: IO [Int]
xAfterInstr = scanl (+) 1 <$> allInstr

cycleToChar spritePos cycle
    | abs(cycle-spritePos) <= 1 = '▉'
    | otherwise = ' '
