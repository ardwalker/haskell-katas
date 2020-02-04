
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
-- As-patterns
--------------------------------------------------------------------------------

-- For example, a function that duplicates the first element in a list might be written as: 
f :: [a] -> [a]
f (x:xs)  = x:x:xs

-- To improve readability, we might prefer to write x:xs just once, which we can achieve using an as-pattern as follows: (Another advantage to doing this is that a naive implementation might completely reconstruct x:xs rather than re-use the value being matched against.) 
f :: [a] -> [a]
f s@(x:xs) = x:s

-- The @ sign makes this an as-pattern, in this case, the entire list can be referenced by 'all'

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  



--------------------------------------------------------------------------------
-- Wild-cards.
--------------------------------------------------------------------------------
-- Another common situation is matching against a value we really care nothing about. For example, the functions head and tail defined in Section 2.1 can be rewritten as: 

head (x:_)             = x
tail (_:xs)            = xs



--------------------------------------------------------------------------------
-- pattern matching with guards
--------------------------------------------------------------------------------
-- top-level patterns may also have a boolean guard, as in this definition of a function that forms an abstract version of a number's sign: 
sign x |  x >  0        =   1
       |  x == 0        =   0
       |  x <  0        =  -1


abs :: Int -> Int
abs x
    | x < 0 = - x
    | x > 0 = x


-- The pattern-matching rules can have subtle effects on the meaning of functions. For example, consider this definition of take: 

take  0     _           =  []
take  _     []          =  []
take  n     (x:xs)      =  x : take (n-1) xs


and :: Bool -> Bool -> Bool
and True True = True
and False _ = False



data Tree a = Fork a (Tree a) (Tree a) | Nil

func :: Tree a -> Int
func Nil              = 1
func (Fork _ Nil Nil) = 2
func (Fork _ Nil _)   = 3
func (Fork _ _   Nil) = 4


lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"


sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5" 


first :: (a, b, c) -> a  
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z  

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  



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



