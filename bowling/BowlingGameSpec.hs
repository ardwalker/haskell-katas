
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Applicative
import BowlingGame 


main :: IO ()
main = hspec $ do

  describe "BowlingGame " $ do
    it "roll no balls" $ do
      roll [] `shouldBe` 0
        
    it "roll all zeros" $ do 
      roll [ 0,0 , 0,0 , 0,0 , 0,0 , 0,0 , 0,0 , 0,0 , 0,0 , 0,0 , 0,0 ] `shouldBe` 0 
    
    it "first ball 1, rest all zeros" $ do 
      roll [ 1,0 , 0,0 , 0,0 , 0,0 , 0,0 , 0,0 , 0,0 , 0,0 , 0,0 , 0,0 ] `shouldBe` 1 
    
    it "roll all ones should score 20" $ do 
      roll [ 1,1 , 1,1 , 1,1 , 1,1 , 1,1 , 1,1 , 1,1 , 1,1 , 1,1 , 1,1 ] `shouldBe` 20
    
    it "test 1 spare" $ do 
      roll [ 5,5 , 3,0 , 0,0 , 0,0 , 0,0 , 0,0 , 0,0 , 0,0 , 0,0 , 0,0 ] `shouldBe` 16 
    
    it "test 1 strike" $ do 
      roll [ 10  , 3,4 , 0,0 , 0,0 , 0,0 , 0,0 , 0,0 , 0,0 , 0,0 , 0,0 ] `shouldBe` 24 
    
    it "test perfect game" $ do 
      roll [ 10 , 10 , 10 , 10 , 10 , 10 , 10 , 10 , 10 , 10 , 10, 0  ] `shouldBe` 300 
