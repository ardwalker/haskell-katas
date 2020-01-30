
module BankOCR (readNumber) where
  
import Debug.Trace 

{-
Each entry is 4 lines long, and each line has 27 characters. The first 3 lines 
of each entry contain an account number written using pipes and underscores, 
and the fourth line is blank. 

Each account number should have 9 digits, all of which should be in the 
range 0-9. A normal file contains around 500 entries.

Your first task is to write a program that can take this file and parse it into
actual account numbers
-}


{-
Each character is split along 3 lines and 3 columns, forming a 3x3 box.
Here, each one is represented by a 9 char string of symbols, top, middle, bottom chars
    _  _     _  _  _  _  _ 
  | _| _||_||_ |_   ||_||_|
  ||_  _|  | _||_|  ||_| _|

["   ,  |,  |"] = "1"
[" _ , _|,|_ "] = "2"
[" _ , _|, _|"] = "3"
["   ,|_|,  |"] = "4"
[" _ ,|_ , _|"] = "5"
[" _ ,|_ ,|_|"] = "6"
[" _ ,  |,  |"] = "7"
[" _ ,|_|,|_|"] = "8"
[" _ ,|_|, _|"] = "9"

-}

readNumber :: [String] -> String
readNumber [] = ""
readNumber (line1:[]) = ""
readNumber (line1:line2:[]) = ""
readNumber (line1:line2:line3:rest) = (linesToCharacters line1 line2 line3) ++ readNumber(rest)


linesToCharacters :: String -> String -> String -> String 
linesToCharacters l1@(_:_:_:[]) l2@(_:_:_:[]) l3@(_:_:_:[]) = (segment(segmentLines l1 l2 l3))
linesToCharacters l1@(_:_:_:xs) l2@(_:_:_:ys) l3@(_:_:_:zs) = (segment(segmentLines l1 l2 l3)) ++ (linesToCharacters xs ys zs)


segmentLines :: String -> String -> String -> String
segmentLines s1 s2 s3 = (take 3 s1) ++ (take 3 s2) ++ (take 3 s3)

-- Retrieves the character by analyzing the 3x3 character block
segment :: String -> String 
segment "     |  |"  = "1"
segment " _  _||_ "  = "2"
segment " _  _| _|"  = "3"
segment "   |_|  |"  = "4"
segment " _ |_  _|"  = "5"
segment " _ |_ |_|"  = "6"
segment " _   |  |"  = "7"
segment " _ |_||_|"  = "8"
segment " _ |_| _|"  = "9"

segment _ = ""






