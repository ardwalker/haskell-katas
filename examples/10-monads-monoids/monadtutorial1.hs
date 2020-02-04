-- ============================================================================
-- From Why Monads - Luca Belli 
-- https://www.youtube.com/watch?v=7xmYgxmhx8M&t=1008s
-- ============================================================================

g :: Int -> Int -> Int 
g x y = x + y

f :: Int -> Int 
f y = y + 1

f = g 1


-- ============================================================================
-- More concrete example
-- ============================================================================

g :: Str -> Int 
g x = length x 

f :: Int -> Int 
f x =  x + 1

-- These can be conposed because the output type of g is the same as the
-- input type of f.
-- Note that (f . g) reads "f after g"
h :: Str -> Int 
h x = f ( g (x) ) = f ( g x) =  (f . g) x


-- ============================================================================
-- Now we want to represent failure ;
-- data Maybe a = Just a | Nothing
-- ============================================================================

gMaybe :: Str -> Maybe Int 
gMaybe str 
    | "https" `isPrefixOf` str = Just (length str)
    | otherwise Nothing

-- Can we compose with f now?

f :: Int -> Int 
f x = x + 1

-- No, we cannot compose how they are now, but we can modify 

-- Lets try to change Int , modify both input and output types
fMaybe :: Maybe Int -> Maybe Int
fMaybe Just x =  Just (f x)
fMaybe Nothing = Nothing 

-- Another example 
gList :: Str -> [Int]

fList :: [Int] -> [Int]
fList [] = []
fList xs = map f xs 

-- Can I compose with f - NO 
-- So we need to change 

-- ============================================================================
-- Abstract the pattern 
-- Advantages: I dont have to put this logic into anywhere else, not the function
--   or the map. It also makes everything composable.
-- ============================================================================

-- Maybe type:
-- map :: (a -> b) -> F a -> F b 
-- map :: (a -> b) -> Maybe a -> Maybe b 

instance Functor Maybe where 
     map f Nothing = Nothing 
     map f (Just x) = Just (f x)  
        -- you have a function and a wrapped value. 
        -- apply the function to the value and rewrap the result with Just constructor Just()


-- List type:
-- map :: (a -> b) -> F a -> F b 
-- map :: (a -> b) -> [a] -> [b] 

instance Functor Maybe where 
    map _ [] = [] 
    map f x:xs = f x : map xs  


fList = map f

-- ============================================================================
-- Game
-- ============================================================================

type Deck = Int 
type Card = Int 

addCard :: Card -> Deck -> Deck 
addCard c d = c + d 

-- Start game 
oneCard :: Deck 
oneCard = addCard 5 0 


twoCard = addCard 4 oneCard
-- equivalent to
--addCard 4 (addCard 5 0)

-- what happens if we add more?
-- readability suffers, read from inside out, nesting
addCard 2 (addCard 3 (addCard 4 (addCard 5 0)))

-- ============================================================================
-- Make the game more interesting ...
-- ============================================================================
addCard :: Card -> Deck -> Maybe Deck 
addCard c d = 
    | c + d < 10 = Just(c + d)
    | otherwise = Nothing -- you lose
    
oneCard :: Maybe Deck 
oneCard = addCard 5 0 

--  Deck != Maybe Deck 
addCard 3 oneCard

game :: Maybe Deck 
game = case addCard 5 0 of  -- returns a Maybe Deck
    Nothing -> Nothing 
    Just deck1 -> case addCard 4 deck1  of -- returns a Maybe Deck
        Nothing -> Nothing 
        Just deck2 -> case addCard 3 deck2  of -- returns a Maybe Deck
            Nothing -> Nothing 
            Just deck3 -> case addCard 2 deck3 -- returns a Maybe Deck
    
{-
Function that takes 2 arguments 

1st argument:
Maybe Deck ->            -- previous total 

2nd argument:
(Deck -> Maybe Deck) ->  -- addCard 

Return value:
Maybe Deck               -- new total


This function exists, its called Bind 
(>>=) :: Maybe Deck ->
         (Deck -> Maybe Deck) ->
         Maybe Deck
-}

emptyDeck :: Maybe Deck 
addCard 5 :: Deck -> Maybe Deck 

emptyDeck >>= addCard 5
emptyDeck = return 0

-- For full game 
game = emptyDeck >>= addFirstCard >>= addSecondCard >>= addThirdCard

-- Monad = Functor + return + >>= 











