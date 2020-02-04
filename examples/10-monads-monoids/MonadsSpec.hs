
module MonadsSpec where

import Test.Hspec


inc :: Int -> Maybe Int
inc n = Just (n + 1)

add1 :: Int -> [Int]
add1 n = [n + 1]


main :: IO ()
main = hspec $ do
  describe "bind without value" $ do
    
    context "maybe monad" $ do

      it "Nothing >> (Just 0) should be Nothing" $
        (Nothing >> (Just 0)) `shouldBe` Nothing

      it "(Just 0) >> Nothing >> (Just 1) should be Nothing" $
        ((Just 0) >> Nothing >> (Just 1)) `shouldBe` Nothing

      it "(Just 0) >> (Just 5) >> (Just 4) should be Just 4" $
        ( (Just 0) >> (Just 5) >> (Just 4) ) `shouldBe` Just 4

    context "using valueless bind synonym *>" $ do
      it "(Just 0) *> (Just 5) *> (Just 4) should be Just 4" $
        ( (Just 0) *> (Just 5) *> (Just 7) ) `shouldBe` Just 7


    context "list monad" $ do

      it "[] >> [1, 2] should be []" $
        ([] >> [1, 2]) `shouldBe` []

      it "[1, 2] >> ([] :: [Int]) should be []" $
        ([1, 2] >> ([] :: [Int])) `shouldBe` []
      
      it "[1] >> [3, 4, 5] should be [3,4,5]" $
        ([1] >> [3, 4, 5]) `shouldBe` [3,4,5]

      it "[1, 2] >> [3, 4, 5] should be [3,4,5,3,4,5]" $
        ([1, 2] >> [3, 4, 5]) `shouldBe` [3,4,5,3,4,5]

      it "[1, 2, 3] >> [3, 4, 5] should be [3,4,5,3,4,5,3,4,5]" $
        ([1, 2, 3] >> [3, 4, 5]) `shouldBe` [3,4,5,3,4,5,3,4,5]


  describe "bind with value" $ do

    context "maybe monad" $ do

      it "Nothing >>= inc >>= inc >>= inc should be Nothing" $
        (Nothing >>= inc >>= inc >>= inc) `shouldBe` Nothing

      it "(Just 0) >>= inc >>= inc >>= inc should be Just 3" $
        ((Just 0) >>= inc >>= inc >>= inc) `shouldBe` Just 3


    context "list monad" $ do

      it "[] >>= add1 >>= add1 >>= add1 should be []" $
        ([] >>= add1 >>= add1 >>= add1) `shouldBe` []

      it "[1, 2, 3] >>= add1 should be [2,3,4]" $
        ([1, 2, 3] >>= add1) `shouldBe` [2,3,4]

      it "[1, 2, 3] >>= add1 >>= add1 should be [3,4,5]" $
        ([1, 2, 3] >>= add1 >>= add1) `shouldBe` [3,4,5]
      
      it "[1, 2, 3] >>= add1 >>= add1 >>= add1 should be [4,5,6]" $
        ([1, 2, 3] >>= add1 >>= add1 >>= add1) `shouldBe` [4,5,6]


  describe "functor map" $ do

    context "fmap and <$> are synonyms" $ do 

      it "fmap (*2) (Just 200) should be Just 400" $
        (fmap (*2) (Just 200)) `shouldBe` Just 400
      
      it "(*2) <$> (Just 200) should be Just 400" $
        ((*2) <$> (Just 200)) `shouldBe` Just 400
          

    -- print (Just 2) <$ (Just 3)
    -- print 1 $> 3

