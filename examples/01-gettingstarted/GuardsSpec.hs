module CaseExpressionsSpec where

import Test.Hspec
import Data.Char



-- Guard 
hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2  -- only if n mod 2 is zero
  | otherwise      = 3 * n + 1
  

-- Foo function
foo :: Integer -> Integer
foo 0 = 16
foo 1
    | "Haskell" > "C++" = 3
    | otherwise         = 4
foo n
    | n < 0           = 0
    | n `mod` 17 == 2 = -43
    | otherwise       = n + 3
    

isEven :: Integer -> Bool
isEven n
    | n `mod` 2 == 0 = True
    | otherwise      = False

    
absolute x
  | x < 0     = -x
  | otherwise = x
  

-- Golf scoring 
holeScore :: Int -> Int -> String
holeScore strokes par
  | score < 0 = show (abs score) ++ " under par"
  | score == 0 = "level par"
  | otherwise = show(score) ++ " over par"
 where score = strokes-par  


--------------------------------------------------------------------------------
-- tests
--------------------------------------------------------------------------------
main :: IO ()
main = hspec $ do
  
  describe "Guards" $ do

    it "ex1 \"help\" == \"Help\" " $
      ex1 "help" `shouldBe` "Help"




