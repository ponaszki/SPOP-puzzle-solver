import System.Environment
import System.IO
import Data.Matrix
import Data.List
import Debug.Trace

main = do
	putStrLn "Hello, Type in a path to file"  
	let filename = "/home/ponaszki/workspace/haskell/SPOP-puzzle-solver/src/p1.txt"
	--filename <- getLine  
	putStrLn ("Opening: " ++ filename )

	--[s] <- getArgs --to do wczytywania pliku jako argument wywołania aplikacji
	--putStrLn (s)
	--handle <- openFile s ReadMode 
	
	handle <- openFile filename ReadMode
	firstLine <- hGetLine handle
	secondLine <- hGetLine handle 
	let rows = read firstLine::[[Int]]
	let columns = read secondLine::[[Int]] 
	let width = length columns
	let height = length rows
	--solve rows columns
	let vals = getElementsToPaintFromLeft (head rows) 1
	putStrLn ("Rows: " ++ firstLine)
	putStrLn ("Columns: " ++ secondLine)
	putStrLn ("Width: " ++ (show width))
	putStrLn ("Height: " ++ (show height))
	print ( solvePuzzle rows columns )


solvePuzzle rss css = solvePuzzle' rss css (matrix (length rss) (length css) $ \(_,_)-> 0)


solvePuzzle' :: [[Int]] ->[[Int]] -> Matrix Int-> Matrix Int 
solvePuzzle' rss css m 	|isAllMatrixFilled m  = m
--								|otherwise = checkAllRows rss 1 m
								|otherwise = checkAllColumns css 1 (checkAllRows rss 1 m)

-- funkcja sprawdzajaca czy cala matryca zostala wypelniona
-- zwraca falsz jezeli nie i prawde jezeli tak	
isAllMatrixFilled :: Matrix Int -> Bool
isAllMatrixFilled m = all (>0) (toList m)


-- ROWS
-- sprawdza wszystkie wiersze macierzy metoda zakreslania czesci wspolnej (od lewej i prawej)
checkAllRows :: [[Int]]-> Int -> Matrix Int-> Matrix Int
checkAllRows [] _ m = m
checkAllRows (rs:rss) startNr m  = Debug.Trace.trace ("Calling car with: rss sn m"++(show rss)++" "++(show startNr)++" \n" ++ (show m) ) 
	(checkAllRows rss (startNr+1) (paintCellsInRow startNr (getElementsToPaintInRow rs (ncols m)) m) )

-- sprawdza czy zachodzi szczegolny przypadek w ktorym jest tylko jeden sposob rozmieszczenia w danym wierszu
checkIfOnlyOneOptionRow :: Int-> [Int] -> Matrix Int -> Bool
checkIfOnlyOneOptionRow rowNr (id:ids) m 	|(sumList (id:ids))+((length (id:ids))-1)==ncols m = True
											| otherwise = False

-- funkcja zwraca liste indeksow elementow w danym wierszu ktore nalezy pomalowac idac od lewej
getElementsToPaintFromLeft :: [Int]-> Int -> Int -> [Int]
getElementsToPaintFromLeft [] _  _= []
getElementsToPaintFromLeft [_] _ 0 = []
getElementsToPaintFromLeft (id:ids) startpos len |id == 0 = [] ++ (getElementsToPaintFromLeft ids (startpos+1) (len-1))	 
												|id >0 = [startpos] ++ (getElementsToPaintFromLeft ((id-1):ids) (startpos+1) (len-1))  

-- funkcja zwraca liste indeksow elementow w danym wierszu ktore nalezy pomalowac idac od prawej
getElementsToPaintFromRight :: [Int] -> Int -> [Int]
getElementsToPaintFromRight [] _ = []
getElementsToPaintFromRight (id:ids)  len 	|id == 0 = [] ++ (getElementsToPaintFromRight ids len)
										|id>0 = [stp] ++ (getElementsToPaintFromRight ((id-1):ids) len)
	where
		stp = len - ((sumList (id:ids))+((length (id:ids))-1)-1)

-- funkcja sumujaca wszystkie elementy listy
sumList :: [Int] -> Int
sumList xs = foldr (+) 0 xs 

-- funkcja zwracajaca czesc wspolna indeksow do pomalowania w danym wierszu (czesc wspolna list "od lewej" i "od prawej")
getElementsToPaintInRow :: [Int]->Int->[Int] 
getElementsToPaintInRow x len= intersect (getElementsToPaintFromLeft x 1 len)  (getElementsToPaintFromRight x len) 

-- funkcja malująca elementy o indeksach podanych w liscie n:ns i numerze wiersza podanym w pierwszym argumencie
paintCellsInRow :: Int->[Int]->Matrix Int->Matrix Int
paintCellsInRow _ [] m = m
paintCellsInRow rowNr (n:ns) m = paintCellsInRow rowNr ns (setElem 1 (rowNr, n) m) 



--COLUMNS 
-- funkcja zwraca liste indeksow elementow w danej kolumnie ktore nalezy pomalowac idac od gory
getElementsToPaintFromUp :: [Int]-> Int -> Int -> [Int]
getElementsToPaintFromUp ids startpos len = getElementsToPaintFromLeft ids startpos len 

-- funkcja zwraca liste indeksow elementow w danej kolumnie ktore nalezy pomalowac idac od dolu
getElementsToPaintFromDown :: [Int] -> Int -> [Int]
getElementsToPaintFromDown ids len = getElementsToPaintFromRight ids len  

-- funkcja zwracajaca czesc wspolna indeksow do pomalowania w danej kolumnie (czesc wspolna list "od gory" i "od dolu")
getElementsToPaintInCol x len = getElementsToPaintInRow x len 

-- funkcja malująca elementy o indeksach podanych w liscie n:ns i numerze kolumny podanym w pierwszym argumencie
paintCellsInColumn :: Int -> [Int] -> Matrix Int-> Matrix Int
paintCellsInColumn _ [] m = m
paintCellsInColumn colNr (n:ns) m = paintCellsInColumn colNr ns (setElem 1 (n, colNr) m) 

-- sprawdza wszystkie kolumny macierzy metoda zakreslania czesci wspolnej (od gory i od dolu)
checkAllColumns :: [[Int]]-> Int -> Matrix Int-> Matrix Int
checkAllColumns [] _ m = m
checkAllColumns (cs:css) colNr m = Debug.Trace.trace ("Calling cac with: css sn m"++(show css)++" "++(show colNr)++" \n" ++ (show m) ) 
	(checkAllColumns css (colNr+1) (paintCellsInColumn colNr (getElementsToPaintInCol cs (nrows m)) m) )

-- sprawdza czy zachodzi szczegolny przypadek w ktorym jest tylko jeden sposob rozmieszczenia w danej kolumnie
checkIfOnlyOneOptionCol colNr (id:ids) m 	|(sumList (id:ids))+((length (id:ids))-1)==nrows m= True
											| otherwise = False

--WYKRESLANIE 
--areGroupsFull Int -> [Int] ->Matrix Int -> Matrix Int
--areGroupsFull =  

--getLengthOfBlock :: Int -> Int ->Matrix Int-> Int
--getLengthOfBlock blockNr  rowNr m = 	if (isNotEmpty (getrow rowNr)) then

