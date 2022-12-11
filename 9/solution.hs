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
    | otherwise = (tailX + signum (headX - tailX), tailY + signum (headY - tailY))

calcHeadPos :: String -> (Int, Int) -> (Int, Int)
calcHeadPos "L" (headX, headY) = (headX-1, headY) 
calcHeadPos "R" (headX, headY) = (headX+1, headY) 
calcHeadPos "U" (headX, headY) = (headX, headY+1) 
calcHeadPos "D" (headX, headY) = (headX, headY-1) 
calcHeadPos _ _ = error "undefined direction" 

initialPos :: Int -> ([(Int, Int)], [(Int, Int)])
initialPos n = ([(0,0)], take n $ repeat (0,0))

moveIt :: ([(Int, Int)], [(Int,Int)]) -> (String, Int) -> ([(Int, Int)], [(Int,Int)])
moveIt a (moveDir, 0) = a 
moveIt (tailPosHistory, headPos:tailsPos) (moveDir, moveMag) = moveIt (newTailPosHistory, newPos) (moveDir, moveMag-1)
    where
        newHeadPos = calcHeadPos moveDir headPos
        newPos = scanl calcTailPos newHeadPos tailsPos
        newTailPosHistory = if elem (last newPos) tailPosHistory then tailPosHistory else (last newPos):tailPosHistory

numberOfTailVisitedLocations :: Int -> IO Int
numberOfTailVisitedLocations n = length.fst.(foldl moveIt (initialPos n)) <$> puzzleInput
