import qualified Data.Vector as V

puzzleInput :: IO (V.Vector (Int,Int))
puzzleInput = (V.indexed . V.fromList).(fmap read).lines <$> readFile "input.txt"

insertToIndex :: Int -> (Int, Int) -> V.Vector (Int, Int) -> V.Vector (Int, Int)
insertToIndex i x xs = before V.++ (V.cons x after)
    where
        (before, after) = V.splitAt i xs 
mix :: Int -> V.Vector (Int, Int) -> V.Vector (Int, Int) -> V.Vector (Int, Int)
mix lp1 pps newList
    | V.null pps = newList
    | otherwise = mix lp1 ps mixedVec 
    where
        Just currentIndex = V.findIndex (\(b,_) -> b==y) newList
        newIndex = mod (x + currentIndex) lp1
        mixedVec = insertToIndex newIndex p (b V.++ (V.tail a))
        (b,a) = V.splitAt currentIndex newList
        p@(y,x) = V.head pps
        ps = V.tail pps

mixNTimes :: Int -> V.Vector (Int, Int) -> V.Vector (Int, Int) -> V.Vector (Int, Int)
mixNTimes 0 _ mixedVec = mixedVec
mixNTimes n originalVec mixedVec = mixNTimes (n-1) originalVec (mix (V.length originalVec -1) originalVec mixedVec)

checkSum :: V.Vector (Int, Int) -> Int
checkSum xs = sum (fmap (snd.(xs V.!)) checkSumIndex)
    where
        Just indexZero = V.findIndex (\(_,a) -> a == 0) xs
        checkSumIndex = fmap (\x -> mod (x+indexZero) (V.length xs)) (V.fromList [1000,2000,3000])

q1 ::  V.Vector (Int, Int) -> Int        
q1 ps = checkSum (mixNTimes 1 ps ps)

q2 :: V.Vector (Int, Int) -> Int
q2 ps = checkSum (mixNTimes 10 decryptApplied decryptApplied)
    where
        decryptApplied  = fmap (\(x,y) -> (x,y*811589153)) ps

main = do
    ps <- puzzleInput
    print (q1 ps)
    print (q2 ps)