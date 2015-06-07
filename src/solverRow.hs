import Data.Matrix

data Cell = Cell Int --deriving Show 

showCell :: Cell -> String
showCell (Cell a) = show a
instance Show Cell where
	show(Cell c) = show c 
--c :: Cell
--c = 

--solveRow :: [Int] -> [Cell] -> [Cell]
--solveRow [] x = x
--solveRow (i:is) (x:xs) | i>0 = 
datarow = [[3], [2, 2], [2, 2], [3], [1], [1], [1], [1], [2], [3]]
elem2 = 1:1:[]
row = (Cell 0):(Cell 0):(Cell 0):(Cell 0):(Cell 0):[]
paintFromRight :: [Integer] -> [Cell] -> [Cell]
paintFromRight [] x = x
paintFromRight [0] x = x
paintFromRight (i:is) (c:cs) 	|i>0 = (Cell 1):paintFromRight ((i-1):is) cs 
								|i==0 = (Cell 0): paintFromRight (is) cs


paintfromLeft  ::[Integer] -> [Cell] -> [Cell]
--paintfromLeft []
paintFromLeft (i:is) (c:cs)	|i>0 = 
