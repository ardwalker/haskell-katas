module CaseExpressionsSpec where

import Test.Hspec
import Data.Char


ex1 :: String -> String
ex1 args = case args of
  "help"  -> "Help"
  "start" -> "startProgram"
  _       -> "bad args"


ex2 :: String -> Int
ex2 s = case s of
  []      -> 3
  ('H':s) -> length s
  _       -> 7


-- Case expression takes a polymorphic list of any type and returns a string
ex3 :: [a] -> String  
ex3 xs = "The list is " ++ case xs of 
  [] -> "empty."  
  [x] -> "a singleton list."   
  xs -> "a longer list." 


data Colour = Black | White | RGB Int Int Int

ex4 :: Colour -> String
ex4 c =
  "This colour is"
  ++ case c of
       Black           -> " black"
       White           -> " white"
       RGB 0 0 0       -> " black"
       RGB 255 255 255 -> " white"
       _               -> "... uh... something else"
  ++ ", yeah?"


--------------------------------------------------------------------------------
-- tests
--------------------------------------------------------------------------------
main :: IO ()
main = hspec $ do
  
  describe "Case Expressions" $ do

    it "ex1 \"help\" == \"Help\" " $
      ex1 "help" `shouldBe` "Help"

    it "ex1 \"whatever\" == \"bad args\"" $
      ex1 "whatever" `shouldBe` "bad args"

    it "ex2 \"\" == 3" $
      ex2 "" `shouldBe` 3

    it "ex2 \"Hello\" == 4" $
      ex2 "Hello" `shouldBe` 4

    it "ex2 \"Anything\" == 7" $
      ex2 "Anything" `shouldBe` 7

    it "ex3 [] == \"The list is empty.\"" $
      ex3 [] `shouldBe` "The list is empty."

    it "ex3 [1] == \"The list is a singleton list.\"" $
      ex3 [1] `shouldBe` "The list is a singleton list."

    it "ex3 [1,2] == \"The list is a longer list.\"" $
      ex3 [1,2] `shouldBe` "The list is a longer list."

    it "ex4 (RGB 255 255 255) shouldBe \"This colour is white, yeah?.\"" $
      ex4 (RGB 255 255 255) `shouldBe` "This colour is white, yeah?"

