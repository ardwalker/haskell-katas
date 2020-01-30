import Test.Hspec
import Test.QuickCheck
import BankOCR

main :: IO ()
main = hspec $ do

  describe "BankOCR " $ do
    it "reads the number 1" $ do 
      readNumber ["   ", "  |","  |"] `shouldBe` "1"

    it "reads the number 2" $ do 
      readNumber [" _ ", " _|","|_ "] `shouldBe` "2"

    it "reads the number 12" $ do 
      readNumber ["    _ ", "  | _|","  ||_ "] `shouldBe` "12"

    it "parses account number" $ do
      readNumber ["    _  _     _  _  _  _  _ ", "  | _| _||_||_ |_   ||_||_|", "  ||_  _|  | _||_|  ||_| _|"] `shouldBe` "123456789"



