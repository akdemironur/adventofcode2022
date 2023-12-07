import qualified Data.Map as M
import Data.Map ((!))
import Data.List (nub, sort, sortBy, maximumBy)
import qualified Data.Vector as V
import qualified Data.Set as S

data Valve = Valve { flowRate :: Int, connections :: [Int] } deriving (Show, Eq)

data State = State { loc :: Int, pressureLeak :: Int, timeRemaining :: Int, openedValves :: S.Set Int } deriving (Show, Ord, Eq)

initialState :: Int -> State
initialState time = State 0 0 time S.empty

lineParser :: String -> (String, Int, [String])
lineParser str = (valvename, flowrate, connections)
    where
        isNumeric = (flip elem) "-0123456789"
        takeNum = takeWhile isNumeric
        dropNum = dropWhile isNumeric
        dropNotNum = dropWhile (not.isNumeric)
        (flowrate, remaining1) = ((read.takeNum.dropNotNum) str, (words.dropNum.dropNotNum) str)
        valvename = [str!!6,str!!7]
        connections = fmap (take 2) (drop 5 remaining1)

input :: String -> IO [(String, Int, [String])]
input filename = (fmap lineParser).lines <$> readFile filename
inputStrToIntIndexing :: [(String, Int, [String])] -> V.Vector Valve
inputStrToIntIndexing inputs = V.fromList $ fmap (\(_,a) -> a) $ sortBy (\(a,_) (b,_) -> compare a b) $ fmap helper2 inputs
    where
        helper (a,b,c) = a
        conversion = M.fromList (zipWith (,) (sort (fmap helper inputs)) [0..])
        helper2 (a,b,c) = (conversion!a, Valve b (fmap (conversion!) c))

connectivity :: IO (V.Vector [Int])
connectivity = (fmap connections) <$> puzzleInput

initialFlowRates :: IO (V.Vector Int)
initialFlowRates = (fmap flowRate) <$> puzzleInput

puzzleInput :: IO (V.Vector Valve)
puzzleInput =  inputStrToIntIndexing <$> (input "input.txt")

shortestPath :: V.Vector [Int] -> Int -> [Int] -> Int -> Int
shortestPath con acc from to
    | to `elem` from = acc
    | otherwise = shortestPath con (acc+1) (nub $ concat $ fmap (con V.!) from) to

shortestPathMatrix :: V.Vector [Int] -> V.Vector (V.Vector Int)
shortestPathMatrix con = V.fromList $ fmap V.fromList [[(shortestPath con 0 [from] to)|to<-[0..(length con -1)]] | from <- [0..(length con -1)]]

removeFlowRate :: V.Vector Int -> Int -> V.Vector Int
removeFlowRate flows l = flows V.// [(l, 0)]

nextState :: V.Vector (V.Vector Int) -> V.Vector Int -> State -> Int -> State
nextState spm flows (State loc p time open) to = State to newP newTime newOpen 
    where
        cost = (((spm `V.unsafeIndex` loc) `V.unsafeIndex` to) + 1) 
        newOpen = S.insert to open
        newTime = time - cost
        newP = p + (flows `V.unsafeIndex` to) * newTime

processQ :: V.Vector (V.Vector Int) -> V.Vector Int -> [State] -> M.Map (S.Set Int) Int -> M.Map (S.Set Int) Int
processQ _ _ [] acc = acc
processQ spm flows (s@(State loc p time open):rest) acc = processQ spm flows (nextStates ++ rest) newAcc 
    where
        nonZeroNextLocs = filter (\x -> flows `V.unsafeIndex` x /= 0 && x `S.notMember` open && (((spm `V.unsafeIndex` loc) `V.unsafeIndex` x) + 1) < time) [0..(V.length flows - 1)]
        nextStates =  (fmap (nextState spm flows s) nonZeroNextLocs)
        newAcc = M.insertWith max open p acc 

findMaxPressure :: M.Map (S.Set Int) Int -> Int
findMaxPressure = snd . maximumBy (\(a,b) (c,d) -> compare b d) . M.toList

q1 :: V.Vector [Int] -> V.Vector Int -> State -> Int
q1 cm fr is = findMaxPressure (processQ (shortestPathMatrix cm) fr [is] M.empty)

q2 :: V.Vector [Int] -> V.Vector Int -> State -> Int
q2 cm fr is = maximum [p1+p2 | (set1, p1) <- lastMap, (set2, p2) <- lastMap, S.disjoint set1 set2] 
    where 
        lastMap = M.toList $ processQ (shortestPathMatrix cm) fr [is] M.empty

main :: IO ()
main = do
 cm <- connectivity
 fr <- initialFlowRates
 print (q1 cm fr (initialState 30))
 print (q2 cm fr (initialState 26))
