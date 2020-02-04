import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Applicative

{-
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

Anyway, the fmap implementation is pretty simple. If it's an empty value of Nothing, then just return a Nothing . 

If we map over an empty box, we get an empty box. It makes sense. 
Just like if we map over an empty list, we get back an empty list. 

If it's not an empty value, but rather a single value packed up in a Just , 
then we apply the function on the contents of the Just . 

When a value is wrapped in a context, you can't apply a normal function to it:

This is where fmap comes in. fmap is from the street, fmap is hip to contexts. fmap knows how to apply functions to values that are wrapped in a context. For example, suppose you want to apply (+3) to Just 2. Use fmap:
-}  

main :: IO ()
main = hspec $ do

  describe "functor map - fmap" $ do
  
    context "fmap Maybe" $ do

      it "fmap (++ \" INSIDE THE JUST\") (Just \"Something serious.\")" $ do
        fmap (++ " INSIDE THE JUST") (Just "Something serious.") `shouldBe` Just "Something serious. INSIDE THE JUST"

      it "fmap (++ \" INSIDE THE JUST\") Nothing" $ do
        fmap (++ " INSIDE THE JUST") Nothing `shouldBe` Nothing

      it "fmap (*2) (Just 200) shouldBe Just 400" $ do
        fmap (*2) (Just 200) `shouldBe` Just 400

      it "fmap (*2) Nothing shouldBe Nothing" $ do
        fmap (*2) Nothing `shouldBe` Nothing

      it "fmap (+3) (Just 2) shouldBe Just 5" $ do
        fmap (+3) (Just 2) `shouldBe` Just 5


    context "<$> infix fmap Maybe" $ do

      it "(++ \"INSIDE THE JUST\") <$> (Just \"Something serious.\")" $ do
        (++ " INSIDE THE JUST") <$> (Just "Something serious.") `shouldBe` Just "Something serious. INSIDE THE JUST"

      it "(++ \" INSIDE THE JUST\") <$> Nothing" $ do
        (++ " INSIDE THE JUST") <$> Nothing `shouldBe` Nothing

      it "(*2) <$> (Just 200) shouldBe Just 400" $ do
        (*2) <$> (Just 200) `shouldBe` Just 400

      it "(*2) <$> Nothing shouldBe Nothing" $ do
        (*2) <$> Nothing `shouldBe` Nothing

      it "(+3) <$> (Just 2) shouldBe Just 5" $ do
        (+3) <$> (Just 2) `shouldBe` Just 5


