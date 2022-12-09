q1 :: IO Int
q1 = numberOfTailVisitedLocations 2

q2 :: IO Int
q2 = numberOfTailVisitedLocations 10

main :: IO ()
main = q1 >>= print >> q2 >>= print

puzzleInput = fmap ((fmap ((\[a,b]->(a,read b)::(String,Int)) . words)).lines) (readFile "input.txt")

calcTailPos :: (Int,Int) -> (Int, Int) -> (Int, Int)
calcTailPos (headX, headY) (tailX, tailY) 
    | abs(tailX-headX) <= 1 && abs(tailY-headY) <= 1 = (tailX, tailY)
    | otherwise = (newDP headX tailX, newDP headY tailY)
        where
            newDP a b
                | a > b = b+1
                | a < b = b-1
                | otherwise = b

calcHeadPos :: String -> (Int, Int) -> (Int, Int)
calcHeadPos "L" (headX, headY) = (headX-1, headY) 
calcHeadPos "R" (headX, headY) = (headX+1, headY) 
calcHeadPos "U" (headX, headY) = (headX, headY+1) 
calcHeadPos "D" (headX, headY) = (headX, headY-1) 
calcHeadPos _ _ = error "undefined direction" 

initial :: Int -> ([(Int, Int)], [(Int, Int)])
initial n = ([(0,0)], take n $ repeat (0,0))

moveIt :: ([(Int, Int)], [(Int,Int)]) -> (String, Int) -> ([(Int, Int)], [(Int,Int)])
moveIt a (moveDir, 0) = a 
moveIt (tailPosHistory,positions) (moveDir, moveMag) = moveIt (newTailPosHistory, newPoss) (moveDir, moveMag-1)
    where
        newHeadPos = calcHeadPos moveDir (positions!!0)
        newPoss = scanl calcTailPos newHeadPos (tail positions) 
        tailPos = last newPoss
        newTailPosHistory = if elem tailPos tailPosHistory then tailPosHistory else tailPos:tailPosHistory

numberOfTailVisitedLocations :: Int -> IO Int
numberOfTailVisitedLocations n = length.fst.(foldl moveIt (initial n)) <$> puzzleInput
