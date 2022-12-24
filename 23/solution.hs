import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S

q1 :: IO Int
q1 = rectangle.(nRounds 0 10) <$>  puzzleInput

q2 :: IO Int
q2 = countRoundUntilConvergence 0 <$> puzzleInput

main :: IO ()
main = q1 >>= print >> q2 >>= print

puzzleInput :: IO (S.Set (Int,Int))
puzzleInput = (addAllLinesToSet 0 S.empty) . lines <$> readFile "input.txt"

addLineToSet :: Int -> Int -> S.Set (Int,Int) -> String -> S.Set (Int,Int)
addLineToSet _ _ acc "" = acc 
addLineToSet lineNumber counter acc (c:cs) = addLineToSet lineNumber (counter+1) newAcc cs
    where 
        newAcc
            | c == '#' = (S.insert (lineNumber, counter) acc)
            | otherwise = acc

addAllLinesToSet :: Int -> S.Set (Int,Int) -> [String] -> S.Set (Int,Int)
addAllLinesToSet lineNumber acc [] = acc
addAllLinesToSet lineNumber acc (l:ls) = addAllLinesToSet (lineNumber+1) (addLineToSet lineNumber 0 acc l) ls

proposeMove :: Int -> S.Set (Int,Int) -> (M.Map (Int,Int) (Int,Int), M.Map (Int,Int) Int) -> (Int,Int) -> (M.Map (Int,Int) (Int,Int), M.Map (Int,Int) Int)
proposeMove ci positions acc@(accProposes, accProposeCount) (r,c)
    | all check [n,s,w,e,ne,nw,se,sw] = (M.insert (r,c) (r,c) accProposes, M.insertWith (+) (r,c) 1 accProposeCount)
    | fsc!!i1 = (M.insert (r,c) (fsp!!i1) accProposes, M.insertWith (+) (fsp!!i1) 1 accProposeCount)
    | fsc!!i2 = (M.insert (r,c) (fsp!!i2) accProposes, M.insertWith (+) (fsp!!i2) 1 accProposeCount)
    | fsc!!i3 = (M.insert (r,c) (fsp!!i3) accProposes, M.insertWith (+) (fsp!!i3) 1 accProposeCount)
    | fsc!!i4 = (M.insert (r,c) (fsp!!i4) accProposes, M.insertWith (+) (fsp!!i4) 1 accProposeCount)
    | otherwise = (M.insert (r,c) (r,c) accProposes, M.insertWith (+) (r,c) 1 accProposeCount) 
    where
        [n,s,w,e,ne,nw,se,sw] = [(r-1,c),(r+1,c),(r,c-1),(r,c+1),(r-1,c+1),(r-1,c-1),(r+1,c+1),(r+1,c-1)]
        check = flip S.notMember positions
        fsc = [cn,cs,cw,ce]
        fsp = [n,s,w,e]
        [i1,i2,i3,i4] = fmap (\x -> mod (x+ci) 4) [0..3]
        cn = all check [n,ne,nw]
        cs = all check [s,se,sw]
        cw = all check [w,nw,sw]
        ce = all check [e,ne,se]

allElvesPropose :: Int -> S.Set (Int,Int) -> (M.Map (Int,Int) (Int,Int), M.Map (Int,Int) Int)
allElvesPropose ci positions = foldl (proposeMove ci positions) (M.empty, M.empty) positions
applyPropositions (propositions, propositionCount) = M.foldrWithKey addIfCount1 S.empty propositions
    where
        addIfCount1 k a acc = if propositionCount!a == 1 then S.insert a acc else S.insert k acc

nRounds :: Int -> Int -> S.Set (Int,Int) -> S.Set (Int,Int)
nRounds _ 0 positions = positions
nRounds ci n positions = nRounds (ci+1) (n-1) propApplied
    where
        propositionswithCount = allElvesPropose ci positions
        propApplied = applyPropositions propositionswithCount

rectangle :: S.Set (Int,Int) -> Int
rectangle positions = rectangleArea - (S.size positions)
    where
        rs = S.map fst positions
        cs = S.map snd positions
        rmin = S.findMin rs
        rmax = S.findMax rs
        cmin = S.findMin cs
        cmax = S.findMax cs
        rectangleArea = (rmax-rmin+1) * (cmax-cmin+1)

countRoundUntilConvergence :: Int -> S.Set (Int,Int) -> Int
countRoundUntilConvergence acc positions
    | propApplied == positions = 1+acc
    | otherwise = countRoundUntilConvergence (acc+1) propApplied
    where
        propositionswithCount = allElvesPropose acc positions
        propApplied = applyPropositions propositionswithCount

