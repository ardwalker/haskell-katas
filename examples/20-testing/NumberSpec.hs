import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Numeric operations" $ do

        it "convert Int to have Num typeclass" $ do
            6 / fromIntegral (length [1, 2, 3]) `shouldBe` 2.0
