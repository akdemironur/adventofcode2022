import Data.Vector ((!), fromList, Vector)
import Data.Char (isDigit)

data State = State Int Int (Int, Int) deriving (Show, Eq)
data Instr = Move Int | L | R deriving (Show, Eq)

puzzleMap = fromList.(fmap fromList).helper.lines <$> readFile "map.txt"
    where
        helper s = fmap (take (length (s!!0)) . (++ repeat ' ')) s

turn (State x y (1,0)) L = State x y (0,-1) 
turn (State x y (0,-1)) L = State x y (-1,0) 
turn (State x y (-1,0)) L = State x y (0,1) 
turn (State x y (0,1)) L = State x y (1,0) 
turn (State x y (1,0)) R = State x y (0,1) 
turn (State x y (0,-1)) R = State x y (1,0) 
turn (State x y (-1,0)) R = State x y (0,-1) 
turn (State x y (0,1)) R = State x y (-1,0)

initialState = State 50 0 (1,0)

move wrapper m state 0 = state
move wrapper m os k = case m!ny!nx of
    '#' -> os
    '.' -> move wrapper m ns (k-1)
    where
        ns@(State nx ny (na, nb)) = wrapper m os

instructions = strToInstrList [] <$> readFile "instr.txt"

strToInstrList acc "" = acc
strToInstrList acc str
    | isMove = strToInstrList (acc ++ [Move takeNum]) dropNum
    | (head str) == 'L' = strToInstrList (acc ++ [L]) (tail str)
    | (head str) == 'R' = strToInstrList (acc ++ [R]) (tail str)
    where
        isMove = isDigit (head str)
        takeNum = read $ takeWhile isDigit str
        dropNum = dropWhile isDigit str

applyInstructions wrapper state m [] = state
applyInstructions wrapper state m (L:is) = applyInstructions wrapper (turn state L) m is
applyInstructions wrapper state m (R:is) = applyInstructions wrapper (turn state R) m is
applyInstructions wrapper state m ((Move k):is) = applyInstructions wrapper (move wrapper m state k) m is

bet a x b = a <= x && x < b

wrapper2 m os@(State ox oy (a,b))
    | (bet 0 x 150) && (bet 0 y 200) && (m!y!x /= ' ') = State x y (a,b)
    | a == 1 && (bet 0 y 50) =      State       99              (49-y+100)      (-1,0)
    | a == 1 && (bet 50 y 100) =    State       (y-50+100)      49              (0,-1)
    | a == 1 && (bet 100 y 150) =   State       149             (149-y)         (-1,0)
    | a == 1 && (bet 150 y 200) =   State       (y-150+50)      149             (0,-1)
    | a == -1 && (bet 0 y 50) =     State       0               (49-y+100)      (1,0)
    | a == -1 && (bet 50 y 100) =   State       (y-50)          100             (0,1)
    | a == -1 && (bet 100 y 150) =  State       50              (149-y)         (1,0)
    | a == -1 && (bet 150 y 200) =  State       (y-150+50)      0               (0,1)
    | b == 1 && (bet 0 x 50) =      State       (x+100)         0               (0,1)
    | b == 1 && (bet 50 x 100) =    State       49              (150+x-50)      (-1,0)
    | b == 1 && (bet 100 x 150) =   State       99              (50+x-100)      (-1,0)
    | b == -1 && (bet 0 x 50) =     State       50              (x+50)          (1,0)
    | b == -1 && (bet 50 x 100) =   State       0               (150+x-50)      (1,0)
    | b == -1 && (bet 100 x 150) =  State       (x-100)         199             (0,-1)
        where
            x = ox+a
            y = oy+b

wrapper1 m os@(State ox oy (a,b))
    | (bet 0 x 150) && (bet 0 y 200) && (m!y!x /= ' ') = State x y (a,b)
    | a == 1 && (bet 0 y 100) =     State 50   y     (a,b)
    | a == 1 && (bet 100 y 200) =   State 0    y     (a,b)
    | a == -1 && (bet 0 y 50) =     State 149  y     (a,b)
    | a == -1 && (bet 50 y 150) =   State 99   y     (a,b)
    | a == -1 && (bet 150 y 200) =  State 49   y     (a,b)
    | b == 1 && (bet 0 x 50) =      State x    100   (a,b)
    | b == 1 && (bet 50 x 150) =    State x    0     (a,b)
    | b == -1 && (bet 0 x 50) =     State x    199   (a,b)
    | b == -1 && (bet 50 x 100) =   State x    149   (a,b)
    | b == -1 && (bet 100 x 150) =  State x    49    (a,b)
        where
            x = ox+a
            y = oy+b

wrapper1 m os@(State ox oy (a,b))
    | (bet 0 x 150) && (bet 0 y 200) && (m!y!x /= ' ') = State x y (a,b)
    | a == 1 = State (finder (+1) fun 0 y) y (a,b)
    | a == -1 = State (finder (+(-1)) fun 149 y) y (a,b)
    | b == 1 = State x (finder (+1) (flip fun) 0 x) (a,b)
    | b == -1 = State x (finder (+(-1)) (flip fun) 199 x) (a,b)
        where
            x = ox+a
            y = oy+b
            finder inc f acc k 
                | f acc k /= ' ' = acc
                | otherwise = finder inc f (inc acc) k
            fun p q = m!q!p

pointCalc (State x y d) = (y+1)*1000 + (x+1)*4 + pd
    where
        pd = case d of
            (1,0) -> 0
            (0,1) -> 1
            (-1,0) -> 2
            (0,-1) -> 3

q1 = pointCalc <$> ((applyInstructions wrapper1 initialState) <$> puzzleMap <*> instructions)
q2 = pointCalc <$> ((applyInstructions wrapper2 initialState) <$> puzzleMap <*> instructions)
main = q1 >>= print >> q2 >>= print