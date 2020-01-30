
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Applicative


main :: IO ()
main = hspec $ do
    describe "Functor Maybe testing" $ do

        it "Should apply (+1) function to Nothing" $ do
            (+1) <$> Nothing `shouldBe` Nothing

        it "Should apply (+1) function to Just 2" $ do
            (+1) <$> Just 2 `shouldBe` Just 3

        -- it "Should left apply 1 to Nothing" $ do
        --     1 :: Int <$ Nothing `shouldBe` 1

            

            -- print $ "--- functor left val ---"
            -- print $ 1 <$ Nothing
            -- print $ 1 <$ Just 2
        
            -- print $ Nothing <**> Just (+ 2)
            -- print $ Just 1 <**> Just (+ 2)
        
            -- print $ liftA (+ 1) Nothing
            -- print $ liftA (+ 1) $ Just 2
        
            -- print $ liftA2 (+) Nothing Nothing
            -- print $ liftA2 (+) (Just 1) (Just 2)
        
            -- print $ (+) <$> Just 1 <*> Nothing
            -- print $ (+) <$> Just 1 <*> Just 2


    describe "Applicatives testing" $ do
        it "Maybe: should apply (+1) function to nothing" $ do
            Just (+ 1) <*> Nothing `shouldBe` Nothing

        it "Maybe: should apply (+1) function to Just 2" $ do
            Just (+ 1) <*> Just 2 `shouldBe` Just 3

        it "List: should apply list of functions to empty list" $ do
            [(+ 1), (* 2)] <*> [] `shouldBe` []
            
        it "List: should apply list of functions to list of int" $ do
            [(+ 1), (* 2)] <*> [1, 2, 3] `shouldBe` [2,3,4,2,4,6] 

        it "apply preserves left argument" $ do
            Just 1 <* Just 2 `shouldBe` Just 1


        it "apply preserves right argument v1" $ do
            Just 1 *> Just 2 `shouldBe` Just 2

        it "apply preserves right argument v2" $ do
            Just 2 *> Just 3 `shouldBe` Just 3

        it "apply preserves right argument v3" $ do
            Nothing *> Just 3 `shouldBe` Nothing

