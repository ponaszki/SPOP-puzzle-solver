
data Cell = Empty | Cell Int 

--c :: Cell
--c = 

--solveRow :: [Int] -> [Cell] -> [Cell]
--solveRow [] x = x
--solveRow (i:is) (x:xs) | i>0 = 
datarow = [[3], [2, 2], [2, 2], [3], [1], [1], [1], [1], [2], [3]]
elem = [3]
paintFromRight :: [Int] -> [Cell] -> [Cell]
paintFromRight [] x = x
paintFromRight (i:is) (c:cs) 	|i>0 = (Cell 1):paintFromRight ((i-1):is) cs 
								|i==0 = (Cell 0): paintFromRight (is) cs
