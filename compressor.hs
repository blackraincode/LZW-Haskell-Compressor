import System.IO
import Text.Show
import Data.List

main =  do  
		let textList = []
		let singlewords = []
		let diccList = []
		let textCompr = []
		
		--Open a text file to compress, change 'Text.txt' for the name(or path) of you text file
		handle <- openFile "Text.txt" ReadMode
		contents <- hGetContents handle
		let singlewords = words contents
		
		print "Text readed :"
		print singlewords

		let textList = removeDuplicates singlewords
		hClose handle  

		let diccList = addDic textList 1
		outh <- openFile "dicc.txt" WriteMode
		hPrint  outh  diccList
		hClose outh	
		
		print "Dictionary created :"
		print diccList

		let textCompr = compressor singlewords diccList 

		print "Compressed file created:"
		print textCompr
			
		outer <- openFile "textCom.txt" WriteMode
		hPrint  outer  textCompr
		
		print "Compressed text: "
        
		let textDesc = decompressor textCompr diccList
	    
		print textDesc  		
		hClose outer
		                
		oute <- openFile "textDecomp.txt" WriteMode
		
		hPrint  oute   (unwords textDesc )
 
--Remove the duplicate elements and return a list without them
--Example; input ["Hello", "Hello","World"] output -> ["Hello", "World"]
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs
-
-
--Example; input [ "Hello", "World"] output --> [("Hello ", 1), ("World", 2)]	
addDic:: [String] -> Int -> [(String , Int)]
addDic [] n = []
addDic (x:xs) n =  [(x, n)] ++ addDic xs (n+1) 	

compressor :: [String ] -> [(String, Int)] ->  [String]
compressor [] _ = []
compressor (x:xs)[] = []
compressor (x:xs) y = (gimmeTheKey x y) : compressor xs y

gimmeTheKey :: String -> [(String,Int)] -> String
gimmeTheKey key [] = ""
gimmeTheKey key ((k,v):xs) = if key == k  
                            then show(v) 
                            else gimmeTheKey key xs 

decompressor :: [String] -> [(String, Int)] -> [String]
decompressor [] _ = []
decompressor (x:xs)[] = [] 
decompressor (x:xs) y = (gimmeTheValue x y) : decompressor xs y

gimmeTheValue :: String -> [(String,Int)] -> String
gimmeTheValue key [] = ""
gimmeTheValue key ((k,v):xs) = if key == show(v)  
                            then k
                            else gimmeTheValue key xs
