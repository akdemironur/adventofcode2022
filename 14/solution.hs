import qualified Data.Set as S
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Data.List (nub)

q1 = counterToAbyss Nothing 0 <$> rockSet 
q2 = bfs Nothing S.empty [sandSource] <$> rockSet
main = q1 >>= print >> q2 >>= print

rockSet :: IO (S.Set (Int, Int))
rockSet = foldl addLineToSet S.empty <$> puzzleInput

puzzleInput :: IO [String]
puzzleInput = lines <$> readFile "input.txt"

takeOnePairFromString :: String -> ((Int,Int), String)
takeOnePairFromString str = ((secondNum, firstNum), remainingString)
    where
        firstNum = (read . (takeWhile isDigit)) str
        strAfterFirstNum = tail $ dropWhile isDigit str
        secondNum = (read . (takeWhile isDigit)) strAfterFirstNum
        remainingString = dropWhile (not.isDigit) $ dropWhile isDigit strAfterFirstNum

addToSet :: (Int, Int) -> (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)
addToSet a@(x1,y1) b@(x2,y2) s 
    | a == b = S.insert a s
    | x1 == x2 = addToSet (x1, if y1 > y2 then y1 - 1 else y1 + 1) b (S.insert a s)
    | y1 == y2 = addToSet (if x1 > x2 then x1 - 1 else x1 + 1, y1) b (S.insert a s)
    | otherwise = error "Wrong input"

addLineToSet :: S.Set (Int, Int) -> String -> S.Set (Int, Int)
addLineToSet s "" = s
addLineToSet s str = if remaining == "" then newSet else addLineToSet newSet remainingString
    where
        (firstPair, remainingString) = takeOnePairFromString str
        (secondPair, remaining) = takeOnePairFromString remainingString
        newSet = addToSet firstPair secondPair s

nextLoc :: ((Int, Int) -> S.Set (Int, Int) -> Bool) -> S.Set (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
nextLoc notMember s (x,y)
    | notMember d s = Just d
    | notMember l s = Just l
    | notMember r s = Just r
    | otherwise = Nothing
    where 
        d = (x+1, y)
        l = (x+1, y-1)
        r = (x+1, y+1)

sandSource :: (Int, Int)
sandSource = (0, 500)

findFinalPoint :: ((Int, Int) -> S.Set (Int, Int) -> Bool) -> Int -> S.Set (Int, Int) -> (Int, Int) -> (Int, Int)
findFinalPoint notMember l s p@(x,y)
    | p2 == Nothing = p
    | otherwise = if fst (fromJust p2) > l then (fromJust p2) else findFinalPoint notMember l s (fromJust p2)
    where
        p2 = nextLoc notMember s p

counterToAbyss l c s = if fst finalPoint > l2 then c else counterToAbyss (Just l2) (c+1) (S.insert finalPoint s)
    where
        finalPoint = findFinalPoint S.notMember l2 s sandSource
        l2 = if l == Nothing then (fst $ S.findMax s) else fromJust l

floorNotMember :: Int -> (Int, Int) -> S.Set (Int, Int) -> Bool
floorNotMember f (x,y) s = if f+2 == x then False else S.notMember (x,y) s

validNeighbors :: Int -> S.Set (Int, Int) -> (Int, Int) -> [(Int,Int)]
validNeighbors l s (x,y) = filter (\p -> floorNotMember l p s) neighbors
    where
        neighbors = [down,left,right]
        down = (x+1, y)
        left = (x+1, y-1)
        right = (x+1, y+1)

bfs :: Maybe Int -> S.Set (Int, Int) -> [(Int, Int)] -> S.Set (Int, Int) -> Int
bfs l visitedPoints currentPoints rocks = if newVisitedPoints == visitedPoints then S.size visitedPoints else bfs (Just l2) newVisitedPoints newcp rocks
    where
        newcp = nub (filter (`S.notMember` newVisitedPoints) (concat $ fmap (validNeighbors l2 rocks) currentPoints))
        newVisitedPoints = foldl (flip S.insert) visitedPoints currentPoints 
        l2 = if l == Nothing then (fst $ S.findMax rocks) else fromJust l