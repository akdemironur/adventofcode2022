import Data.Char (ord)
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Vector ((!))

type Grid a = V.Vector (V.Vector a) 
type Coord = (Int, Int)

q1 :: IO Int
q1 = (bfs 1 S.empty) <$> startPointsForPart1 <*> endPoint <*> replacedGrid

q2 :: IO Int
q2 = (bfs 1 S.empty) <$> startPointsForPart2 <*> endPoint <*> replacedGrid

main :: IO ()
main = q1 >>= print >> q2 >>= print

puzzleInput :: IO (Grid Char)
puzzleInput = (V.fromList.(fmap V.fromList).lines) <$> readFile "input.txt"


findListOfIndices :: Eq a => a -> Grid a -> [Coord]
findListOfIndices c grid = filter (\(x,y) -> grid!x!y == c) allIndices
    where
        n = V.length grid
        m = V.length (grid!0)
        allIndices = [(x,y) | x <- [0..n-1], y <- [0..m-1]]

replacedGrid :: IO (Grid Int)
replacedGrid = fmap (fmap func) <$> puzzleInput
    where
        func x
            | x == 'S' = ord 'a'
            | x == 'E' = ord 'z'
            | otherwise = ord x

validNeighbors :: Grid Int -> Coord -> [Coord]
validNeighbors grid (x,y) = filter (\(a,b) -> isValidIndex (a,b) && isValidHeight(a,b)) allNeighborIndices
    where
        n = V.length grid
        m = V.length (grid!0)
        allNeighborIndices = fmap (\(a,b) -> (a+x,b+y)) [(0,1), (0,-1), (1,0), (-1,0)]
        isValidIndex (a,b) = a >= 0 && b >= 0 && a < n && b < m
        isValidHeight (a,b) = (grid!a!b) - (grid!x!y) <= 1

startPointsForPart1 :: IO [Coord]
startPointsForPart1 = fmap (findListOfIndices 'S') puzzleInput

startPointsForPart2 :: IO [Coord]
startPointsForPart2 = fmap (findListOfIndices (ord 'a')) replacedGrid

endPoint :: IO Coord
endPoint =  fmap (head . findListOfIndices 'E') puzzleInput

bfs :: Int -> S.Set Coord -> [Coord] -> Coord -> Grid Int -> Int
bfs distance visitedPoints currentPoints end grid = if elem end newcp then distance else bfs (distance+1) newVisitedPoints newcp end grid
    where
        newcp = addListifNotExist (filter (`S.notMember` newVisitedPoints) (concat $ fmap (validNeighbors grid) currentPoints)) []
        newVisitedPoints = foldl (flip S.insert) visitedPoints currentPoints 
        addListifNotExist [] acc = acc
        addListifNotExist (x:xs) acc
            | elem x acc = addListifNotExist xs acc
            | otherwise = addListifNotExist xs (x:acc)
