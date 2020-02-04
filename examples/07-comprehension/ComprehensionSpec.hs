module ComprehensionSpec where

import Test.Hspec
import Data.Char

-- List comprehensions are syntactic sugar

main :: IO ()
main = hspec $ do
  
  describe "Comprehensions" $ do

      context "basic case" $ do
    
        it "[x*2 | x <- [1..10]] == [2,4,6,8,10,12,14,16,18,20]" $
          [x*2 | x <- [1..10]] == [2,4,6,8,10,12,14,16,18,20]

      context "using guards" $ do

        it "[x*2 | x <- [1..10], x*2 >= 12] == [12,14,16,18,20]" $
          [x*2 | x <- [1..10], x*2 >= 12] == [12,14,16,18,20]

        it "[x | x <- [10..20], x/=13, x/=15, x/=19] == [10,11,12,14,16,17,18,20]" $
          [x | x <- [10..20], x/=13, x/=15, x/=19] == [10,11,12,14,16,17,18,20]

      context "multiple generators and a guard" $ do 
        
        it "[x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50] == [55,80,100,110]" $
          [x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50] == [55,80,100,110]


      context "tuple returned" $ do 

        -- Tuple list returned 
        it "[(i,j) | i <- [1,2], j <- [1..3]] == [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3)]" $
          [(i,j) | i <- [1,2], j <- [1..3]] == [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3)]


        it "[(x,y) | x <- [1,3,5], y <- [2,4,6], x<y] == [(1,2),(1,4),(1,6),(3,4),(3,6),(5,6)]" $
          [(x,y) | x <- [1,3,5], y <- [2,4,6], x<y] == [(1,2),(1,4),(1,6),(3,4),(3,6),(5,6)]

      context "with functions" $ do 
        
        it "[toLower x | x <- ['A','1','c', '?'], isLetter x] == ['a','c']" $
          [toLower x | x <- ['A','1','c', '?'], isLetter x]  == ['a','c']

        it "[c | c <- \"Hahaha! Ahahaha!\", c `elem` ['A'..'Z']] == HA" $ 
          [c | c <- "Hahaha! Ahahaha!", c `elem` ['A'..'Z']] == "HA"


      context "combination condition and guard" $ do 

        it "[if x < 10 then \"BOOM!\" else \"BANG!\" | x <- [7..13], odd x] == [\"BOOM!\",\"BOOM!\",\"BANG!\",\"BANG!\"]" $ do 
        [if x < 10 then "BOOM!" else "BANG!" | x <- [7..13], odd x] == ["BOOM!","BOOM!","BANG!","BANG!"]
-- 
-- -- This would be infinite, use take 10
-- take 10 [ (i,j) | i <- [1,2], j <- [1..] ]
-- 
-- take 10 [ (i,j) | i <- [1..], 
--                   let k = i*i, 
--                   j <- [1..k] ]
-- 





