import Data.List (sort)

data IntOrList = I Int | L [IntOrList] deriving (Show, Eq, Read)

instance Ord IntOrList where
    compare (I i1) (I i2) = compare i1 i2
    compare (L xs) (L ys) = compare xs ys
    compare (I x) (L ys) = compare (L [I x]) (L ys)
    compare (L xs) (I y) = compare (L xs) (L [I y])

readIOL :: String -> IntOrList
readIOL "" = L []
readIOL pstr = L [read $ stringPreprocessor pstr]
    where
        stringPreprocessor "" = ""
        stringPreprocessor str@(c:cs)
            | c == '[' = "L [" ++ stringPreprocessor cs
            | c == ' ' = ' ' : stringPreprocessor cs
            | c == ',' = ',' : stringPreprocessor cs
            | c == ']' = ']' : stringPreprocessor cs
            | otherwise = "I " ++ (takeWhile isNumeric str) ++ (stringPreprocessor (dropWhile isNumeric str))
        isNumeric = (flip elem) "-0123456789"

q1 :: IO Int
q1 = countRightOrders 1 0 <$> puzzleInput

q2 :: IO Int
q2 = (dividerIndicesProduct (dividers []) 1).sort.dividers <$> puzzleInput

main :: IO ()
main = q1 >>= print >> q2 >>= print

puzzleInput :: IO [IntOrList]
puzzleInput = (filter (/= (L []))).(fmap readIOL).lines <$> readFile "input.txt"

dividers :: [IntOrList] -> [IntOrList]
dividers = ((readIOL "[[2]]"):).((readIOL "[[6]]"):)

dividerIndicesProduct :: [IntOrList] -> Int -> [IntOrList] -> Int
dividerIndicesProduct [] _ _ = 1
dividerIndicesProduct _ _ [] = error "Not all dividers found"
dividerIndicesProduct (d:ds) n (p:ps)
    | p == d = n * (dividerIndicesProduct ds (n+1) ps)
    | otherwise = (dividerIndicesProduct (d:ds) (n+1) ps)

countRightOrders :: Int -> Int -> [IntOrList] -> Int
countRightOrders n acc [] = acc
countRightOrders n acc (x:y:zs)
    | compare x y == GT = countRightOrders (n+1) acc zs
    | otherwise = countRightOrders (n+1) (acc+n) zs
