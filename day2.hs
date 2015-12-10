import Data.List.Split

paperRequired :: (Int, Int, Int) -> Int
paperRequired (w, h, l) = 2 * w * h + 2 * w * l + 2 * h * l + (min (min (l * w)  (h * w)) (l * h))
paperRequired _ = error "Incorrect tuple format"

test1 = paperRequired (2, 3, 4) == 58
test2 = paperRequired (1, 1, 10) == 43

tuplify :: [a] -> (a, a, a)
tuplify [x, y, z] = (x, y, z)

rowToResultInternal :: String -> String -> Int
rowToResultInternal str delim = paperRequired . tuplify . map read $ (splitOn delim str)

rowToResult :: String -> Int
rowToResult str = rowToResultInternal str "x"

test3 = rowToResult "2x3x4" == 58
test4 = rowToResult "1x1x10" == 43
testall = test1 && test2 && test3 && test4

main = do
	input <- readFile "day2.txt"
 	print $ sum (map rowToResult $ lines input)
