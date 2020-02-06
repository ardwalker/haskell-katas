
-- Learn You a Haskell for Great Good!

module Typeclasses  
(
  MyShinyNewType(..), Shape(..), Point(..), Person(..), Car(..), surface
) where

--------------------------------------------------------------------------------
-- algebraic data types
--------------------------------------------------------------------------------

-- DayOfWeek datatype
data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun 
  deriving (Show)


-- Value constructors are functions like everything else. Make a function that 
-- takes a shape and returns its surface.
data Shape = Circle Float Float Float | Rectangle Float Float Float Float 

ghci> :t Circle  
Circle :: Float -> Float -> Float -> Shape  

ghci> :t Rectangle  
Rectangle :: Float -> Float -> Float -> Float -> Shape  


surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1) 

-- redefine using point
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

--------------------------------------------------------------------------------
-- record syntax
--------------------------------------------------------------------------------

-- instead of creating a verbose algebraic data type, you can do this: 
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)   

{-
  The main benefit of this is that it creates functions that lookup fields in the data type. By using record syntax to create this data type, Haskell automatically made these functions: firstName, lastName, age, height, phoneNumber and flavor.
-}

ghci> :t flavor  
flavor :: Person -> String  
ghci> :t firstName  
firstName :: Person -> String 



-- DayOfWeek data type support for the Eq typeclass
instance Eq DayOfWeek where 
  (==) Mon Mon = True 
  (==) Tue Tue = True 
  (==) Weds Weds = True 
  (==) Thu Thu = True 
  (==) Fri Fri = True 
  (==) Sat Sat = True 
  (==) Sun Sun = True 
  (==) _ _ = False


-- Date datatype
data Date = Date DayOfWeek Int
  deriving (Show)

-- DayOfWeek support for the Eq typeclass
instance Eq Date where 
  (==) (Date weekday dayOfMonth) 
       (Date weekday' dayOfMonth') = 
       weekday == weekday' && dayOfMonth == dayOfMonth'

-- Date Thu 10 == Date Thu 10


--------------------------------------------------------------------------------
-- the type class
--------------------------------------------------------------------------------
{-
  BasicEq typeclass; a typeclass defines a series of functions that must supported by any type that wants to be part of the typeclass...
-}
class BasicEq a where
    isEqual :: a -> a -> Bool



-- Two ways to include a user defined type in a type class

-- ‣ Method (1): use deriving; only works for some predefined, frequently used
-- type classes like Eq, Ord, Show, Read
data MyShinyNewType = This Int | That String deriving (Eq, Ord, Show)


-- ‣ Method (2): the programmer explicitly provides the definition for the 
-- member functions of the class 
data MyShinyNewType = This Int | That String 

instance Eq MyShinyNewType where
  (==) (This n) (This m) = True
  (==) _        _        = False
  (/=) t1 t2 = not (t1 == t2)

-- so to use booleans with the BasicEq typeclass, we deine the instance function
instance BasicEq Bool where
  isEqual True  True  = True
  isEqual False False = True
  isEqual _     _     = False





-- TrafficLight algebraic data type
data TrafficLight = Red | Yellow | Green

{-
  It defines the states of a traffic light. Notice how we didn't derive any class instances for it. That's because we're going to write up some instances by hand, even though we could derive them for types like Eq and Show . 
  
  Here's how we make it an instance of Eq
-}

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False


{-
  We did it by using the instance keyword. 
  Class is for defining new typeclasses and instance is for making our types instances of typeclasses. 
-}



{-
  We could have just derived Eq and it would have had the same effect (but we didn't for educational purposes). 
  
  However, deriving Show would have just directly translated the value constructors to strings. But if we want lights to appear like "Red light" , then we have to make the instance declaration by hand. 
-}
instance Show TrafficLight where
  show Red    = "Red light"
  show Yellow = "Yellow light"
  show Green  = "Green light"



{-
  The YesNo typeclass defines one function. 

  That function takes one value of a type that can be considered 
  to hold some concept of true-ness and tells us for sure if it's true or not. 

  Notice that from the way we use the a in the function, a has to be a 
  concrete type. Next up, let's define some instances.

  For numbers, we'll assume that (like in JavaScript) any number 
  that isn't 0 is true-ish and 0 is false-ish. 
-}

class YesNo a where
  yesno :: a > Bool

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



---------------------------------------------------------------------------------- builtin type classes
--------------------------------------------------------------------------------
{-
  A typeclass is a sort of interface that defines some behavior.
  If a type is a part of a typeclass, that means that it supports and implements the behavior the typeclass describes. 
  
  NOTE: the => operator, this is a class constraint. Everything before the => symbol is called a class constraint. We can read the previous type declaration like this: the equality function takes any two values that are of the same type and returns a Bool. The type of those two values must be a member of the Eq class (this was the class constraint).

  ghci> :t (==) 
  (==) :: (Eq a) => a -> a -> Bool
-}

Eq          -- equality 
Show        -- like toString 
Read        -- opposite of show, reads a string and returns a type

Ord         (deriving Eq)       -- ordering
Num         (deriving Eq, Show) -- numbers
Bounded                         -- bounded types

Enum
Real        (deriving Ord, Num) 
Fractional  (deriving Num)

Integral    (deriving Enum, Real) 
RealFrac    (deriving Real, Fractional) 
Floating    (deriving Fractional) 

Monad 
RealFloat   (deriving RealFrac, Floating)  

MonadPlus   (deriving Monad)    
Functor


--------------------------------------------------------------------------------
-- type parameters
--------------------------------------------------------------------------------

-- 'a' is called the type parameter
-- 'Maybe' is called the type constructor
--    before =   type constructor 
--    after =    value constructors
data Maybe a = Nothing | Just a 

{-
 Depending on what we want this data type to hold when it's not Nothing, this type constructor can end up producing a type of Maybe Int, Maybe Car, Maybe String, etc. No value can have a type of just Maybe, because that's not a type per se, it's a type constructor. In order for this to be a real type that a value can be part of, it has to have all its type parameters filled up.

 Type parameters are useful because we can make different types with them depending on what kind of types we want contained in our data type. 
 
 When we do :t Just "Haha", the type inference engine figures it out to be of the type Maybe [Char], because if the a in the Just a is a string, then the a in Maybe a must also be a string.
-}

-- we could change our Car data type from this:
data Car = Car{company :: String, model :: String, year :: Int} deriving (Show)  

-- to this:
data Car a b c = Car { company :: a, model :: b, year :: c } deriving (Show)  

{-
  But would we really benefit? The answer is: probably no, because we'd just end up defining functions that only work on the Car String String Int type. 
  
  For instance, given our first definition of Car, we could make a function that displays the car's properties in a nice little text.
-}
 

{-
  You can also make typeclasses that are subclasses of other typeclasses. The class declaration for Num is a bit long, but here's the first part:
  
  As we mentioned previously, there are a lot of places where we can cram in class constraints. So this is just like writing class Num a where, only we state that our type a must be an instance of Eq
-}
class (Eq a) => Num a where  
   ...    

 
 







