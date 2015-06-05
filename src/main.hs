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
	putStrLn (firstLine)
	putStrLn (secondLine)
	
