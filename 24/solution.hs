import qualified Data.Set as S

type Blizz4 = (S.Set (Int, Int),S.Set (Int, Int),S.Set (Int, Int),S.Set (Int, Int))

blizzardsInitial :: [String] -> Blizz4
blizzardsInitial = (addAllLinesToSet 0 (S.empty,S.empty,S.empty,S.empty))

puzzleInput :: IO [String]
puzzleInput = lines <$> readFile "input.txt"

lims :: [String] -> (Int, Int)
lims strs = (xLim, yLim)
    where
        yLim = (length strs) - 1
        xLim = (length $ strs!!0) - 1

startPoint :: (Int, Int)
startPoint = (1,0)

endPoint :: [String] -> (Int, Int)
endPoint = (\(x,y)-> (x-1,y)).lims

addLineToSet :: Int -> Int -> Blizz4 -> String -> Blizz4
addLineToSet _ _ acc "" = acc 
addLineToSet lineNumber counter acc@(accLeft, accRight, accUp, accDown) (c:cs) = addLineToSet lineNumber (counter+1) newAcc cs
    where 
        newAcc
            | c == '^' = (accLeft, accRight, (S.insert (counter,lineNumber) accUp), accDown)
            | c == 'v' = (accLeft, accRight, accUp, (S.insert (counter,lineNumber) accDown))
            | c == '>' = (accLeft, (S.insert (counter,lineNumber) accRight), accUp, accDown)
            | c == '<' = ((S.insert (counter,lineNumber) accLeft), accRight, accUp, accDown)
            | otherwise = acc

addAllLinesToSet :: Int -> Blizz4 -> [String] -> Blizz4
addAllLinesToSet lineNumber acc [] = acc
addAllLinesToSet lineNumber acc (l:ls) = addAllLinesToSet (lineNumber+1) (addLineToSet lineNumber 0 acc l) ls

leftBlizzardStep :: (Int, Int) -> (Int, Int) -> (Int, Int)
leftBlizzardStep (limitx, limity) (x,y) = (if x-1 == 0 then limitx-1 else x-1, y)

rightBlizzardStep :: (Int, Int) -> (Int, Int) -> (Int, Int)
rightBlizzardStep (limitx, limity) (x,y) = (if x+1 == limitx then 1 else x+1, y)

upBlizzardStep :: (Int, Int) -> (Int, Int) -> (Int, Int)
upBlizzardStep (limitx, limity) (x,y) = (x, if y-1 == 0 then limity-1 else y-1)

downBlizzardStep :: (Int, Int) -> (Int, Int) -> (Int, Int)
downBlizzardStep (limitx, limity) (x,y) = (x, if y+1 == limity then 1 else y+1)

possibleSteps :: S.Set (Int, Int) -> (Int, Int) -> S.Set (Int, Int)
possibleSteps acc (x,y) = foldl (flip S.insert) acc [(x,y), (x+1,y), (x-1,y), (x,y-1), (x,y+1)]


allBlizzStep :: (Int, Int) -> Blizz4 -> Blizz4
allBlizzStep limits (left, right, up, down) = ( S.map (leftBlizzardStep limits) left
                                              , S.map (rightBlizzardStep limits) right
                                              , S.map (upBlizzardStep limits) up
                                              , S.map (downBlizzardStep limits) down 
                                              )

checkPointValidity :: (Int, Int) -> Blizz4 -> (Int, Int) -> Bool
checkPointValidity (limitx, limity) (left, right, up, down) p@(x,y)
    | p == (1,0) = True
    | p == (limitx-1,limity) = True
    | x <= 0 = False
    | y <= 0 = False
    | x >= limitx = False
    | y >= limity = False
    | S.member p left = False
    | S.member p right = False
    | S.member p up = False
    | S.member p down = False
    | otherwise = True

bfs :: Int -> (Int, Int) -> Blizz4 -> (Int, Int) -> S.Set (Int, Int) -> (Int, Blizz4)
bfs counter limits blizzards ep currentPoints = if elem ep currentPoints then (counter, blizzards) else bfs (counter+1) limits newBlizzards ep newCurrentPoints
    where
        newBlizzards = allBlizzStep limits blizzards
        newCurrentPoints = S.filter (checkPointValidity limits newBlizzards) (foldl possibleSteps S.empty currentPoints)

main :: IO ()
main = do
    pi <- puzzleInput
    let ep = endPoint pi
        bi = blizzardsInitial pi
        lim = lims pi
        (firstRun, firstBI) = bfs 0 lim bi ep (S.singleton startPoint)
        (secondRun, secondBI) = bfs firstRun lim firstBI startPoint (S.singleton ep)
        (thirdRun, _) = bfs secondRun lim secondBI ep (S.singleton startPoint)
    print firstRun
    print secondRun
    print thirdRun
