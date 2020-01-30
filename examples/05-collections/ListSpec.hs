
import Test.Hspec
import Test.QuickCheck

import qualified Data.List as L 
import Data.Char
import Data.Function (on)


main :: IO ()
main = hspec $ do
    describe "List operations" $ do

        it "intersperse a list of chars" $
            L.intersperse '.' "HELLO" `shouldBe` "H.E.L.L.O"

        it "intercalate words" $
            L.intercalate " " ["hey","there","guys"] `shouldBe` "hey there guys"

        it "transpose words" $
            L.transpose ["hey","there","guys"] `shouldBe` ["htg","ehu","yey","rs","e"]

        it "concat words" $
            L.concat ["foo","bar","car"] `shouldBe` "foobarcar"

        it "concat numbers" $
            L.concat [[1 * 6], [2 * 6], [3 * 6]] `shouldBe` [6,12,18]

        it "concat map" $
            L.concatMap (replicate 4) [1..3] `shouldBe` [1,1,1,1,2,2,2,2,3,3,3,3]

        it "any" $
            L.any (==4) [2,3,5,6,1,4] `shouldBe` True

        it "all greater than 4" $
            L.all (>4) [6,9,10] `shouldBe` True

        it "all elements in the list" $
            L.all (`elem` ['A'..'Z']) "HEYGUYSwhatsup" `shouldBe` False

        it "take and iterate" $
            L.take (3) (iterate (++ "haha") "haha") `shouldBe` ["haha","hahahaha","hahahahahaha"]

        it "take while" $
            L.takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1] `shouldBe` [6,5,4]

        it "take while /= " $ do
            L.takeWhile (/=' ') "This is a sentence" `shouldBe` "This"

        it "sum takewhile" $ do 
            L.sum ( (takeWhile (<10000))  ( map (^3) [1..] )) `shouldBe` 53361

        it "sum drop while" $ do
            L.dropWhile (/=' ') "This is a sentence" `shouldBe` " is a sentence" 

        it "break" $ do
            L.break (==4) [1,2,3,4,5,6,7] `shouldBe` ([1,2,3],[4,5,6,7])

        it "span" $ do
            L.span (/=4) [1,2,3,4,5,6,7] `shouldBe` ([1,2,3],[4,5,6,7])
            
        it "sort" $ do
            L.sort [8,5,3,2,1,6,4,2] `shouldBe` [1,2,2,3,4,5,6,8]

        it "group" $ do         
            L.group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7] `shouldBe` [[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]
        
        it "inits" $ do
            L.inits "w00t" `shouldBe` ["","w","w0","w00","w00t"]

        it "tails" $ do
            L.tails "w00t" `shouldBe` ["w00t","00t","0t","t",""]

        it "zip" $ do
            let w = "w00t"
            L.zip (L.inits w) (L.tails w) `shouldBe` [("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")]

        it "partition1" $ do
            L.partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy" `shouldBe` ("BOBMORGAN","sidneyeddy")

        it "partition2" $ do
            L.partition (>3) [1,3,5,6,3,2,1,0,3,7] `shouldBe` ([5,6,7],[1,3,3,2,1,0,3])
        
        it "find returns the first element where the predicate is true" $ do
            L.find (>4) [1,2,3,4,5,6] `shouldBe` Just 5
        
        it "find finding no match" $ do
            L.find (>9) [1,2,3,4,5,6] `shouldBe` Nothing
            
        it "elimIndices returns the list indices for the specified element" $ do
            ' ' `L.elemIndices` "Where are the spaces?" `shouldBe` [5,9,13]
        
        it "findIndex finds first index where the predicate holds true" $ do
            L.findIndex (==7) [5,3,2,1,6,4] `shouldBe` Nothing

        it "findIndices" $ do
            L.findIndices (`elem` ['A'..'Z']) "Where Are The Caps?" `shouldBe` [0,6,10,14]
        
        it "lines should take a string and returns every line of that string in a list" $ do
            L.lines "first line\nsecond line\nthird line" `shouldBe` ["first line","second line","third line"]
        
        it "unlines does the opposite" $ do
            L.unlines ["first line", "second line", "third line"] `shouldBe` "first line\nsecond line\nthird line\n"
        
        it "nub should remove duplicates" $ do
            L.nub [1,2,3,4,3,2,1,2,3,4,3,2,1] `shouldBe` [1,2,3,4]
                
        it "delete takes an element and a list and deletes the first occurence of that element in the list" $ do
            L.delete 'h' "hey there ghang!" `shouldBe` "ey there ghang!"
        
        it "\\\\ is the list difference function" $ do
            [1..10] L.\\ [2,5,9] `shouldBe` [1,3,4,6,7,8,10]
        
        {-
        union also acts like a function on sets. It returns the union 
        of two lists. It pretty much goes over every element in the second 
        list and appends it to the first one if it isn't already in yet. 
        Watch out though, duplicates are removed from the second list! 
        -}
        it "union appends second list items to first listing removing any duplicates" $ do
            "hey man" `L.union` "man what's up" `shouldBe` "hey manwt'sup"
            L.union [1..7][5..10] `shouldBe` [1..10]


        it "intersect works like set intersection, returns only elements in both lists" $ do
            [1..7] `L.intersect` [5..10] `shouldBe` [5,6,7]
        
        
        {- 
        insert takes an element and a list of elements that can be 
        sorted and inserts it into the last position where it's still 
        less than or equal to the next element. In other words, 
        insert will start at the beginning of the list and then keep 
        going until it finds an element that's equal to or greater 
        than the element that we're inserting and it will insert it 
        just before the element. 
        -}
        it "should insert an element in the list" $ do
            L.insert 4 [3,5,1,2,8,2] `shouldBe` [3,4,5,1,2,8,2]
        
        
        --     let values = [-4.3, 2.4, 1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, 2.4, 14.5, 2.9, 2.3]
        --     print $ L.groupBy (\x y -> (x > 0) == (y > 0)) values
        
        --     {-
        --     Data.Function.on 
        --     on :: (b > b > c) > (a > b) > a > a > c
        --     f `on` g = \x y > f (g x) (g y)
        
        --     So doing (==) `on` (> 0) returns an equality function that looks like 
        --     \x y > (x > 0) == (y > 0) . 
        --     on is used a lot with the By functions because with it, we can do:
        --     -}
        --     print $ L.groupBy ( (==) `on` (> 0)) values
        
        --     -- Simulate words funciton using on
        --     print $ L.filter (not . L.any isSpace) . L.groupBy ((==) `on` isSpace) $ "hey guys its me"
