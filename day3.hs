import Data.List (nub)

moveSanta :: String -> [(Int, Int)]
moveSanta input = move (0, 0) input where
					move (x,y) [] = [(x, y)]
	 				move (x, y) ('^' : is) = (x, y) : move (x, y+1) is
	  				move (x, y) ('v' : is) = (x, y) : move (x, y-1) is
	   				move (x, y) ('>' : is) = (x, y) : move (x+1, y) is
					move (x, y) ('<' : is) = (x, y) : move (x-1, y) is
	 				move _ _ = []

housesVisited :: [(Int, Int)] -> Int
housesVisited = length . nub

test1 = (housesVisited $ moveSanta ">") == 2
test2 = (housesVisited $ moveSanta "^>v<") == 4
test3 = (housesVisited $ moveSanta "^v^v^v^v^v") == 2
testall = test1 && test2 && test3

main = do
	file <- readFile "day3.txt"
 	print . housesVisited $ moveSanta file

inc :: Int -> Int
inc = (+1)

moveSantaRobo :: String -> [(Int, Int, Bool)]
moveSantaRobo input = move (0, 0) 0 input where
					move (x,y) index [] = [(x, y, odd index)]
	 				move (x, y) index ('^' : is) = (x, y, odd index) : move (x, y+1) (inc index) is
	  				move (x, y) index ('v' : is) = (x, y, odd index) : move (x, y-1) (inc index) is
	   				move (x, y) index ('>' : is) = (x, y, odd index) : move (x+1, y) (inc index) is
					move (x, y) index ('<' : is) = (x, y, odd index) : move (x-1, y) (inc index) is
	 				move _ _ _ = []

