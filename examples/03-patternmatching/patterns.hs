
module Patterns
( factorial,
  tell,
  tell',
  take',
  lucky,
  charName,
  addVectors,
  bmiTell3,
  calcBmis,
  myCompare,
  initials,
  first
) where

--------------------------------------------------------------------------------
-- Haskell will match starting at the top, until it hits a catchall
--------------------------------------------------------------------------------
factorial :: Int -> Int
factorial 0 = 1
factorial n = n *  factorial(n - 1)


--------------------------------------------------------------------------------
-- Here we use a type class to make sure we can convert the input (inner) types to strings
--------------------------------------------------------------------------------
tell :: (Show a) => [a] -> String
tell [] = "Empty"
tell (x:[]) = "Contains only 1 item: " ++ show x
tell (x:y:[]) = "Contains only 2 items: " ++ show x ++ " and " ++ show y
tell all@(x:y:_) = "Contains many items... "++ show all


--------------------------------------------------------------------------------
-- Here is a variation on tell using [Char]
-- See 'all' below, this is a handy lable for the whole string input
--------------------------------------------------------------------------------
tell' :: String -> String
tell' "" = "Empty"
tell' all@(x:xs) = all ++ " -- first letter: " ++ [x] ++ " remaining letters are: " ++ xs



--------------------------------------------------------------------------------
-- Take number of items from a list , using recursion
--------------------------------------------------------------------------------
take' m ys = case (m,ys) of
     (0,_)       ->  []
     (_,[])      ->  []
     (n,x:xs)    ->  x : take' (n-1) xs


--------------------------------------------------------------------------------
-- Lucky number 7
--------------------------------------------------------------------------------
lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"  


--------------------------------------------------------------------------------
-- Char to print a name
--------------------------------------------------------------------------------
charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil" 
charName  _  = "Sorry, no matching names!"


--------------------------------------------------------------------------------
-- Adding vectors with pattern matching 
--------------------------------------------------------------------------------
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) 


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
bmiTell3 :: (RealFloat a) => a -> a -> String  
bmiTell3 weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
   where bmi = weight / height ^ 2  
         skinny = 18.5  
         normal = 25.0  
         fat = 30.0  

--  where bmi = weight / height ^ 2 
--    (skinny, normal, fat) = (18.5, 25.0, 30.0)  pattern matching alternative


--------------------------------------------------------------------------------
-- Just like we've defined constants in where blocks, you can also define 
-- functions. Staying true to our healthy programming theme, let's make a 
-- function that takes a list of weight-height pairs and returns a list of BMIs.
--------------------------------------------------------------------------------
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2 


--------------------------------------------------------------------------------
-- Compare using guards 
--------------------------------------------------------------------------------
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  -- use as infix operator using backticks
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT


--------------------------------------------------------------------------------
-- Using pattern match to print initials
--------------------------------------------------------------------------------
initials :: String -> String -> String  
initials firstname lastname = [f] ++ "." ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname 

--------------------------------------------------------------------------------
-- Finding elements of a tuple
--------------------------------------------------------------------------------
first :: (a, b, c) -> a  
first (x, _, _) = x  



-- (&&) :: Bool -> Bool -> Bool
-- True && True = True 
-- x    && y    = False


(&&) :: Bool -> Bool -> Bool
True && y = y 
_    && _    = False



