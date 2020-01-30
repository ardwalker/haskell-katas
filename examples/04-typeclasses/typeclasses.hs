
module Typeclasses  
(
  MyShinyNewType(..),
  Shape(..),
  Point(..),
  Person(..),
  Car(..),
  surface
) where


data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun 
  deriving (Show)
instance Eq DayOfWeek where 
  (==) Mon Mon = True 
  (==) Tue Tue = True 
  (==) Weds Weds = True 
  (==) Thu Thu = True 
  (==) Fri Fri = True 
  (==) Sat Sat = True 
  (==) Sun Sun = True 
  (==) _ _ = False

data Date = Date DayOfWeek Int
  deriving (Show)
instance Eq Date where 
  (==) (Date weekday dayOfMonth) 
       (Date weekday' dayOfMonth') = 
       weekday == weekday' && dayOfMonth == dayOfMonth'

-- Date Thu 10 == Date Thu 10




-- class BasicEq a where
--     isEqual :: a -> a -> Bool
-- 
-- instance BasicEq Bool where
--     isEqual True  True  = True
--     isEqual False False = True
--     isEqual _     _     = False


-- Two ways to include a user defined type in a type class
-- ‣ Method (1): use deriving; only works for some predefined, frequently used
-- type classes like Eq, Ord, Show, Read
data MyShinyNewType
  = This Int
  | That String
  deriving (Eq, Ord, Show)


-- ‣ Method (2): the programmer explicitly provides the definition for the member functions of the class 
-- data MyShinyNewType
--   = This Int
--   | That String
-- instance Eq MyShinyNewType where
--   (==) (This n) (This m) = True
--   (==) _        _        = False
--   (/=) t1 t2 = not (t1 == t2)
  

data Point = Point Float Float deriving (Show)

  
data Shape = 
    Circle Point Float 
  | Rectangle Point Point   
    deriving (Show)
  
  
-- 2 different ways to write a type 
-- data Person = Person String String Int Float String String deriving (Show)  
-- If you want to access the firstName, you'd have to write an accessor match 
-- and repeat for each data point 
-- E.g. 
-- firstName :: Person -> String  
-- firstName (Person firstname _ _ _ _ _) = firstname  


-- Instead, you can do this: 
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)   


data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)  


surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)


-- Type class
data TrafficLight = Red | Yellow | Green

{-
It defines the states of a traffic light. Notice how we didn't derive
any class instances for it. That's because we're going to write up some
instances by hand, even though we could derive them for types like Eq and Show . 
Here's how we make it an instance of Eq . 
-}

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

{-
  We did it by using the instance keyword. 
  So class is for defining new typeclasses and 
  instance is for making our types instances of typeclasses. 
-}

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

{-
Nice. We could have just derived Eq and it would have had the same effect 
(but we didn't for educational purposes). However, deriving Show would have 
just directly translated the value constructors to strings. 
But if we want lights to appear like "Red light" , then we have to make 
the instance declaration by hand. 
-}

-- -----------

class YesNo a where
  yesno :: a > Bool

{-
  Pretty simple. The YesNo typeclass defines one function. 
  That function takes one value of a type that can be considered 
  to hold some concept of true-ness and tells us for sure if it's true or not. 
  Notice that from the way we use the a in the function, a has to be a 
  concrete type. Next up, let's define some instances. 
  For numbers, we'll assume that (like in JavaScript) any number 
  that isn't 0 is true-ish and 0 is false-ish. 
-}
    
instance YesNo Int where
  yesno 0 = False
  yesno _ = True
  
  
instance YesNo [a] where
  yesno [] = False
  yesno _ = True
  
instance YesNo Bool where
  yesno = id 

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False
    
instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True
    
    
  
  




