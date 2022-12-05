import System.IO

q1 :: IO String
q1 = ((fmap ((<$>) head) . applyAll reverse . tail . dropWhile (/= "")) <$> readInputFile) <*> initialState

q2 :: IO String
q2 = ((fmap ((<$>) head) . applyAll id . tail . dropWhile (/= "")) <$> readInputFile) <*> initialState

readInputFile :: IO [String]
readInputFile = do
        handle <- openFile "input.txt" ReadMode
        contents <- lines <$> (hGetContents handle)
        return contents

initialStateLineParser :: String -> [String] -> [String]
initialStateLineParser str state = zipWith (\s c -> if c == ' ' then s else s ++ [c]) state (fmap (str!!) [1,5..33])

initialStateParser :: [String] -> [String] -> [String]
initialStateParser acc (l:ls)
    | l !! 1 == '1' = acc
    | otherwise = initialStateParser (initialStateLineParser l acc) ls

initialState :: IO [String]
initialState = fmap (initialStateParser . take 9 $ repeat []) readInputFile

actionParserApplier :: (String -> String) -> [String] -> String -> [String]
actionParserApplier f state str = mover to 1 (getterDropper f from 1 amount state []) []
    where
        [amount,from,to] = (fmap (read.((words str)!!)) [1,3,5])
        getterDropper f n c m (s:ss) acc
            | n == c = (f $ take m s, acc ++ [drop m s] ++ ss)
            | otherwise = getterDropper f n (c+1) m ss (acc ++ [s])
        mover n c (m, (s:ss)) acc
            | n == c = acc ++ [m ++ s] ++ ss
            | otherwise = mover n (c+1) (m, ss) (acc ++ [s])

applyAll :: (String -> String) -> [String] -> [String] -> [String]
applyAll _ [] state = state
applyAll f (a:as) state = applyAll f as (actionParserApplier f state a)
