import Data.List
import qualified Data.Vector as V
import Data.Vector ((!))

q1 :: IO Int
q1 = fmap countVisibleTrees puzzleInputVector

q2 :: IO Int
q2 = fmap maxScenicScore puzzleInputVector

main :: IO ()
main = q1 >>= print >> q2 >>= print

puzzleInput :: IO [[Int]]
puzzleInput = fmap (fmap (read.(:[]))).lines <$> readFile "input.txt"

type Trees = (V.Vector (V.Vector Int))

puzzleInputVector :: IO Trees
puzzleInputVector = V.fromList . (fmap V.fromList) <$> puzzleInput

isVisible :: Trees -> (Int, Int) -> (Int, Int) -> Bool
isVisible trees (lx, ly) (x,y) = top || bot || left || right
    where
        treeHeight = trees!x!y
        top = max2 (fmap (trees!x!) (V.enumFromStepN (y-1) (-1) y)) < treeHeight
        bot = max2 (fmap (trees!x!) (V.enumFromN (y+1) (ly-y-1))) < treeHeight
        left = max2 (fmap (\k -> trees!k!y) (V.enumFromStepN (x-1) (-1) x)) < treeHeight
        right = max2 (fmap (\k -> trees!k!y) (V.enumFromN (x+1) (lx-x-1))) < treeHeight
        max2 a 
            | V.null a = 0
            | otherwise = V.maximum a

countVisibleTrees :: Trees -> Int
countVisibleTrees trees = fromEdges + fromInterior
    where
        lx = V.length trees
        ly = V.length (trees!0)
        indices = V.fromList [(x,y) | x<-[1..lx-2], y<-[1..ly-2]]
        fromEdges = (lx*ly-(lx-2)*(ly-2))
        fromInterior = (V.length $ V.filter id (fmap (isVisible trees (lx, ly)) indices))

scenicScore :: Trees -> (Int, Int) -> (Int, Int) -> Int
scenicScore trees (lx, ly) (x,y) = top * bot * left * right
    where
        treeHeight = trees!x!y
        top = scenicScoreHelper treeHeight (fmap (trees!x!) (V.enumFromStepN (y-1) (-1) y))
        bot = scenicScoreHelper treeHeight (fmap (trees!x!) (V.enumFromN (y+1) (ly-y-1)))
        left = scenicScoreHelper treeHeight (fmap (\k -> trees!k!y) (V.enumFromStepN (x-1) (-1) x))
        right = scenicScoreHelper treeHeight (fmap (\k -> trees!k!y) (V.enumFromN (x+1) (lx-x-1)))
        scenicScoreHelper :: Int -> V.Vector Int -> Int
        scenicScoreHelper x ts 
            | V.null ts = 0
            | otherwise = if x > V.head ts 
                          then 1 + (scenicScoreHelper x (V.tail ts))
                          else 1

maxScenicScore :: Trees -> Int
maxScenicScore trees = maximum ((fmap (scenicScore trees (lx, ly)) indices))
    where
        lx = V.length trees
        ly = V.length (trees!0)
        indices = V.fromList [(x,y) | x<-[1..lx-2], y<-[1..ly-2]]
