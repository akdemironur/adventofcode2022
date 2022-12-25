q1 :: IO Int
q1 = fmap (check 4 4) (readFile "input.txt")

q2 :: IO Int
q2 = fmap (check 14 14) (readFile "input.txt")

check :: Int -> Int -> String -> Int
check n c (s:str) = if isUnique (take n str) then c else check n (c+1) (tail str)

isUnique :: Eq a => [a] -> Bool
isUnique [] = True
isUnique (x:xs) = if elem x xs then False else isUnique xs

main :: IO ()
main = q1 >>= print >> q2 >>= print