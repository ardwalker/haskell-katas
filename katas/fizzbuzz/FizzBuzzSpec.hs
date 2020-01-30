

import Test.Hspec
import Test.QuickCheck
import FizzBuzz 


main :: IO ()
main = hspec $ do

  describe "FizzBuzz " $ do
    it "prints 1 and 2" $ do
      fizzbuzz [1,2] `shouldBe` ["1","2"]

    it "prints 1, 2 and Fizz" $ do
      fizzbuzz [1,2,3] `shouldBe` ["1","2","Fizz"]

    it "prints 1, 2, Fizz, 4, Buzz" $ do
      fizzbuzz [1..5] `shouldBe` ["1","2","Fizz","4","Buzz"]

    it "list 1..15, 15 should print FizzBuzz" $ do
      fizzbuzz [1..15] `shouldBe` ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz"]
