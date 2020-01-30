

import qualified Data.List as L 
import Data.Char
import Data.Function (on)

main = do 
    print $ L.intersperse '.' "HELLO"

    print $ L.intercalate " " ["hey","there","guys"]

    print $ L.transpose ["hey","there","guys"]

    print $ L.concat ["foo","bar","car"]

    print $ L.concatMap (replicate 4) [1..3]

    print $ L.any (==4) [2,3,5,6,1,4]

    print $ L.all (>4) [6,9,10]

    print $ L.all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
    
    print $ L.take 3 $ iterate (++ "haha") "haha"

    -- let (a,b) = splitAt 3 "foobar" in b ++ a

    print $ L.takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]

    print $ L.takeWhile (/=' ') "This is a sentence"

    print $ L.sum $ takeWhile (<10000) $ map (^3) [1..]

    print $ L.dropWhile (/=' ') "This is a sentence"

    print $ L.break (==4) [1,2,3,4,5,6,7]

    print $ L.span (/=4) [1,2,3,4,5,6,7]
    
    print $ L.sort [8,5,3,2,1,6,4,2]

    print $ L.group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]

    print $ L.inits "w00t"

    print $ L.tails "w00t"

    let w = "w00t"
    print $ L.zip (L.inits w) (L.tails w)
    
    print $ L.partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"

    print $ L.partition (>3) [1,3,5,6,3,2,1,0,3,7]

    print $ L.find (>4) [1,2,3,4,5,6]

    print $ L.find (>9) [1,2,3,4,5,6]
    
    
    print $ ' ' `L.elemIndices` "Where are the spaces?"

    print $ L.findIndex (==7) [5,3,2,1,6,4]

    print $ L.findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"

    {-
    lines is a useful function when dealing with files or input 
    from somewhere. It takes a string and returns every line of 
    that string in a separate list. 
    -}
    print $ L.lines "first line\nsecond line\nthird line"

    print $ L.unlines ["first line", "second line", "third line"]


    -- words and unwords are for splitting a line of text into words or joining a
    -- list of words into a text. Very useful. 
    print $ L.words "hey these are the words in this sentence"

    print $ L.words "hey these are the words in this\nsentence"

    print $ L.unwords ["hey","there","mate"]
    
    -- Nub removes duplicates
    print $ L.nub [1,2,3,4,3,2,1,2,3,4,3,2,1]


    -- delete takes an element and a list and deletes the first occurence 
    -- of that element in the list. 
    print $ L.delete 'h' "hey there ghang!"

    {-
    \\ is the list difference function. 
    It acts like a set difference, basically. 
    For every element in the right-hand list, it removes
    a matching element in the left one. 
    -}
    print $ [1..10] L.\\ [2,5,9]

    print $ "Im a big baby" L.\\ "big"

    {-
    union also acts like a function on sets. It returns the union 
    of two lists. It pretty much goes over every element in the second 
    list and appends it to the first one if it isn't already in yet. 
    Watch out though, duplicates are removed from the second list! 
    -}
    print $ "hey man" `L.union` "man what's up"

    print $ L.union [1..7][5..10]
        

    -- intersect works like set intersection. It returns only 
    -- the elements that are found in both lists. 
    print $ [1..7] `L.intersect` [5..10]


    {- 
    insert takes an element and a list of elements that can be 
    sorted and inserts it into the last position where it's still 
    less than or equal to the next element. In other words, 
    insert will start at the beginning of the list and then keep 
    going until it finds an element that's equal to or greater 
    than the element that we're inserting and it will insert it 
    just before the element. 
    -}
    print $ L.insert 4 [3,5,1,2,8,2]


    let values = [-4.3, 2.4, 1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, 2.4, 14.5, 2.9, 2.3]
    print $ L.groupBy (\x y -> (x > 0) == (y > 0)) values

    {-
    Data.Function.on 
    on :: (b > b > c) > (a > b) > a > a > c
    f `on` g = \x y > f (g x) (g y)

    So doing (==) `on` (> 0) returns an equality function that looks like 
    \x y > (x > 0) == (y > 0) . 
    on is used a lot with the By functions because with it, we can do:
    -}
    print $ L.groupBy ( (==) `on` (> 0)) values

    -- Simulate words funciton using on
    print $ L.filter (not . L.any isSpace) . L.groupBy ((==) `on` isSpace) $ "hey guys its me"




    


