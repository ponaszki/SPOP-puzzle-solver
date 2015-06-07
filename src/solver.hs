import System.Environment
import System.IO

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
	putStrLn ("Rows: " ++ firstLine)
	putStrLn ("Columns: " ++ secondLine)
	putStrLn ("Width: " ++ (show width))
	putStrLn ("Height: " ++ (show height))
	
