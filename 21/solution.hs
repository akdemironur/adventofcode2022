import qualified Data.Map as M

data Monkey = Operation String String String String | Number String Int deriving (Eq, Show)

line2Monkey :: String -> (String, Monkey)
line2Monkey str
    | length seperated == 2 = (take 4 $ seperated!!0, Number (take 4 $ seperated!!0) (read $ seperated!!1))
    | otherwise = (take 4 $ seperated!!0, Operation (take 4 $ seperated!!0) (seperated!!1) (seperated!!2) (seperated!!3))
        where
            seperated = words str

str2func :: String -> (Int -> Int -> Int)
str2func "*" = (*)
str2func "-" = (-)
str2func "+" = (+)
str2func "/" = div

puzzleInput :: IO (M.Map String Monkey)
puzzleInput = M.fromList . (fmap line2Monkey) . lines <$> readFile "input.txt"

evalWithHumnCheck :: M.Map String Monkey -> Monkey -> (Int, Bool)
evalWithHumnCheck _ (Number n a) = (a, n=="humn")
evalWithHumnCheck monkeys (Operation monkeyc monkey1 op monkey2) = (fun m1Val m2Val, hmn1 || hmn2)
    where
        (m1Val, hmn1) = evalWithHumnCheck monkeys (monkeys M.! monkey1)
        (m2Val, hmn2) = evalWithHumnCheck monkeys (monkeys M.! monkey2)
        fun = str2func op

reverseOp :: Bool -> String -> Int -> Int -> Int -> Int
reverseOp True "*" a b assertedValue = div assertedValue b
reverseOp False "*" a b assertedValue = div assertedValue a
reverseOp True "+" a b assertedValue = assertedValue - b
reverseOp False "+" a b assertedValue = assertedValue - a
reverseOp True "-" a b assertedValue = assertedValue + b
reverseOp False "-" a b assertedValue = a - assertedValue
reverseOp True "/" a b assertedValue = assertedValue * b 
reverseOp False "/" a b assertedValue = div a assertedValue 

backTrack :: M.Map String Monkey -> Int -> Monkey -> Int
backTrack _ assertedValue (Number "humn" _) = assertedValue
backTrack monkeys assertedValue (Operation mn monkey1 op monkey2)
    | humnPath1 = backTrack monkeys (reverseOp True op m1Val m2Val assertedValue) m1
    | humnPath2 = backTrack monkeys (reverseOp False op m1Val m2Val assertedValue) m2
    where
        m1 = monkeys M.! monkey1
        m2 = monkeys M.! monkey2
        (m1Val, humnPath1) = evalWithHumnCheck monkeys m1
        (m2Val, humnPath2) = evalWithHumnCheck monkeys m2

backTrackRoot :: M.Map String Monkey -> Int
backTrackRoot monkeys
    | humnPath1 = backTrack monkeys m2Val m1
    | humnPath2 = backTrack monkeys m1Val m2
    where
        (Operation _ monkey1 _ monkey2) = monkeys M.! "root"
        m1 = monkeys M.! monkey1
        m2 = monkeys M.! monkey2
        (m1Val, humnPath1) = evalWithHumnCheck monkeys m1
        (m2Val, humnPath2) = evalWithHumnCheck monkeys m2

evalRoot:: M.Map String Monkey -> Int
evalRoot monkeys = fst (evalWithHumnCheck monkeys (monkeys M.! "root"))

q1 :: IO Int
q1 = evalRoot <$> puzzleInput

q2 :: IO Int
q2 = backTrackRoot <$> puzzleInput

main :: IO ()
main = q1 >>= print >> q2 >>= print