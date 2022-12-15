q1 = sum.(fmap (\(a,b) -> b-a)).(mergeAll []).concat.(fmap (sensorBoundaryAtY 2000000))  <$> sensorsRangeM

q2 = do
    sensorsRange <- sensorsRangeM
    return (tuningFreq.head $ (filter (\p -> and (fmap (pointCheck p) sensorsRange)) (sensorsBoundaries sensorsRange)))

main = q1 >>= print >> q2 >>= print

isOverlapping (a,b) (c,d)
    | b >= c && c > a && d > b = True
    | d >= a && a > c && b > d = True
    | otherwise = False

isNeighbor (a,b) (c,d)
    | a < c && b+1 == c = True
    | c < a && d+1 == a = True
    | otherwise = False

isContaining (a,b) (c,d)
    | a <= c && b >= d = True
    | c <= a && d >= b = True
    | otherwise = False

isMergeable p1 p2 = isContaining p1 p2 || isNeighbor p1 p2 || isOverlapping p1 p2 

mergeOrSecond p1@(a,b) p2@(c,d)
    | isMergeable p1 p2 = (min a c, max b d)
    | otherwise = p2

mergeAll acc [] = acc
mergeAll acc (x:xs) = if mergeables == [] then mergeAll (x:acc) xs else mergeAll acc (fmap (mergeOrSecond x) xs)
    where
        mergeables = filter (isMergeable x) xs

inputReader str = ((sx, sy), (bx, by))
    where
        sx = read $ (takeNum.dropNotNum) str
        sy = read $ (takeNum.dropNotNum.dropNum.dropNotNum) str
        bx = read $ (takeNum.dropNotNum.dropNum.dropNotNum.dropNum.dropNotNum) str
        by = read $ (takeNum.dropNotNum.dropNum.dropNotNum.dropNum.dropNotNum.dropNum.dropNotNum) str
        isNumeric = (flip elem) "-0123456789"  
        dropNotNum = dropWhile (not.isNumeric)
        takeNum = takeWhile isNumeric
        dropNum = dropWhile isNumeric

puzzleInput = (fmap inputReader).lines <$> readFile "input.txt"

manDist (a,b) (c,d) = abs(a-c) + abs(b-d)

sensorsRangeM = fmap (\(p1,p2) -> (p1, manDist p1 p2)) <$> puzzleInput

sensorBoundaryAtY y ((sx, sy), range)
    | ydist > range = []
    | otherwise = [(sx-remRange,sx+remRange)]
    where
        ydist = abs(sy-y)
        remRange = range-ydist

sensorOneAwayFromRangeAtY y ((sx, sy), range)
    | ydist == range+1 = [(sx,y)]
    | ydist > range+1 = []
    | otherwise = [(sx-remRange-1,y), (sx+remRange+1,y)]
    where
        ydist = abs(sy-y)
        remRange = range-ydist

sensorsBoundaries = concat . (fmap sensorBoundaryPoints)

sensorBoundaryPoints sensor@((sx,sy),range) = go startingY
    where
        startingY = max (sy-range-1) 0
        endingY = min (sy+range+1) 4000000
        go y
            | y == endingY = (sensorOneAwayFromRangeAtY y sensor)
            | otherwise = (sensorOneAwayFromRangeAtY y sensor) ++ go (y+1)


pointCheck point (sensor,range) = (manDist point sensor) > range
tuningFreq (a,b) = a*4000000+b

