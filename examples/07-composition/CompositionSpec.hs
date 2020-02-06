module CompositionSpec where

import Test.Hspec


main :: IO ()
main = hspec $ do
  
  describe "traditional composition with brackets" $ do
    
      it "sum  (takeWhile (<10000)  (filter odd  (map (^2) [1..])))" $
        sum (takeWhile (<10000) (filter odd (map (^2) [1..]))) `shouldBe` 166650

      it "negate (abs x))" $
        map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24] `shouldBe` [-5,-3,-6,-7,-3,-2,-19,-24]
  
      it "negate (sum (tail xs)))" $
        map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]] `shouldBe` [-14,-15,-27]
  
  describe "composition with . operator" $ do
  
      it "sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]" $
        ( sum . takeWhile (<10000) . filter odd . map (^2) $ [1..] ) `shouldBe` 166650
  
      it "(negate . abs)" $
        map (negate . abs) [5,-3,-6,7,-3,2,-19,24] `shouldBe` [-5,-3,-6,-7,-3,-2,-19,-24]
  
      it "(negate . sum . tail)" $
        map (negate . sum . tail) [[1..5],[3..6],[1..7]] `shouldBe` [-14,-15,-27]
  



