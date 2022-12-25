puzzleInput = lines <$> readFile "input.txt"

chrConverter '2' = 2
chrConverter '1' = 1
chrConverter '0' = 0
chrConverter '-' = -1
chrConverter '=' = -2

snafuToBase10 str = sum $ zipWith (\a b -> b*(5^a)) [0..] num
    where
        num = reverse $ fmap chrConverter str

base10ToSnafu = reverse.helper
    where
        helper 0 = []
        helper num
            | mod num 5 == 0 = '0' : (helper (div num 5))
            | mod num 5 == 1 = '1' : (helper (div num 5))
            | mod num 5 == 2 = '2' : (helper (div num 5))
            | mod num 5 == 3 = '-' : (helper ((div num 5)+2))
            | mod num 5 == 4 = '=' : (helper ((div num 5)+1))

q1 = base10ToSnafu.sum.(fmap snafuToBase10) <$> puzzleInput
main = q1 >>= print