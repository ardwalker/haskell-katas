
module Main where

import Typeclasses


-- Ord is for types that have an ordering.
-- ghci> :t (>)
-- (>) :: (Ord a) => a -> a -> Bool


-- Read will need a type if cannot be inferred from an expression
-- Prelude> read "3"
-- *** Exception: Prelude.read: no parse
-- Prelude> read "3" :: Int
-- 3


main = do
  let pt = Point 1.0 2.0 
  print $ show pt
  
  let person = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
  print $ firstName person ++ " " ++ lastName person

  print $ Car {company="Ford", model="Mustang", year=1967}

  print $ surface $ Circle (Point 0 0) 24
  print $ surface $ Rectangle (Point 0 0) (Point 100 100)


  

