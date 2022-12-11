import Data.List (sort)

data Monkey = Monkey { items :: [Int], func :: (Int->Int), funcDivTest :: Int -> Int, divNum :: Int, counter :: Int }

q1 :: IO Int
q1 = (product.(take 2).reverse.sort.(fmap counter).(processNRound 20 0 (`div` 3))) <$> allMonkeys

q2 :: IO Int
q2 = (\x -> (product.(take 2).reverse.sort.(fmap counter).(processNRound 10000 0 (`mod` (foldl lcm 1 (fmap divNum x))))) x) <$> allMonkeys

main :: IO ()
main = q1 >>= print >> q2 >>= print

puzzleInput :: IO [String]
puzzleInput = lines <$> readFile "input.txt"

readMonkey :: [String] -> Monkey
readMonkey [_,l2,l3,l4,l5,l6,_] = Monkey itemList f fDivTest divisibleTestNum 0
    where
        itemList = read ("[" ++ (drop 18 l2) ++ "]")
        facc
            | head (drop 23 l3) == '*' = (*)
            | head (drop 23 l3) == '+' = (+)
            | head (drop 23 l3) == '-' = (-)
        f
            | head (drop 25 l3) == 'o' = \x -> facc x x
            | otherwise = \x -> facc x (read (drop 25 l3))
        divisibleTestNum = read (drop 21 l4)
        fDivTest = \x -> if mod x divisibleTestNum == 0 then (read (drop 29 l5)) else (read (drop 30 l6))

readAllMonkeys :: [String] -> [Monkey]
readAllMonkeys [] = []
readAllMonkeys allInputs = (readMonkey (take 7 allInputs)) : (readAllMonkeys (drop 7 allInputs))

allMonkeys :: IO [Monkey]
allMonkeys = readAllMonkeys <$> puzzleInput

addItemToMonkey :: Int -> Monkey -> Monkey
addItemToMonkey item (Monkey itemList f fDivTest divTest c) = Monkey (item:itemList) f fDivTest divTest c

addItem :: Int -> Int -> Int -> [Monkey] -> [Monkey]
addItem _ _ _ [] = []
addItem item targetM currentM (m:ms)
    | targetM == currentM = (addItemToMonkey item m):ms
    | otherwise = m:(addItem item targetM (currentM+1) ms)

getProcessingMonkey :: Int -> [Monkey] -> (Monkey, [Monkey])
getProcessingMonkey n monkeys = (oldMonkey, newMonkeys)
    where
        oldMonkey = monkeys!!n
        newMonkey = Monkey [] (func oldMonkey) (funcDivTest oldMonkey) (divNum oldMonkey) ((length $ items oldMonkey) + (counter oldMonkey))
        newMonkeys = (take n monkeys) ++ newMonkey:(drop (n+1) monkeys)

processOneMonkey :: (Monkey, [Monkey]) -> (Int -> Int) -> [Monkey]
processOneMonkey (currentMonkey, monkeys) wr
    | items currentMonkey == [] = monkeys
    | otherwise = processOneMonkey (newCurrentMonkey, newMonkeys) wr
        where
            i = head (items currentMonkey)
            newi = wr ((func currentMonkey) i)
            target = (funcDivTest currentMonkey) newi
            newMonkeys = addItem newi target 0 monkeys
            newCurrentMonkey = Monkey (tail $ items currentMonkey) (func currentMonkey) (funcDivTest currentMonkey) (divNum currentMonkey) (counter currentMonkey)

processOneRound :: Int -> [Monkey] -> (Int -> Int) -> [Monkey]
processOneRound n monkeys wr
    | n == length monkeys = monkeys
    | otherwise = processOneRound (n+1) (processOneMonkey (getProcessingMonkey n monkeys) wr) wr

processNRound :: Int -> Int -> (Int -> Int) -> [Monkey] -> [Monkey]
processNRound n c wr monkeys
    | n == c = monkeys
    | otherwise = processNRound n (c+1) wr (processOneRound 0 monkeys wr)
