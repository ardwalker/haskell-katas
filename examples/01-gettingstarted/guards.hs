

-- Guard 
hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2  -- only if n mod 2 is zero
  | otherwise      = 3 * n + 1
  

-- Foo function
foo :: Integer -> Integer
foo 0 = 16
foo 1
    | "Haskell" > "C++" = 3
    | otherwise         = 4
foo n
    | n < 0           = 0
    | n `mod` 17 == 2 = -43
    | otherwise       = n + 3
    



{-
Guards are evaluated top to bottom
'otherwise' is the catch all
-}

isEven :: Integer -> Bool
isEven n
    | n `mod` 2 == 0 = True
    | otherwise      = False

    
-- Simplified isEven
isEven' :: Integer -> Bool
isEven' n = n `mod` 2 == 0


main = do 
    print (hailstone 3)
    print [foo (-3), foo 0, foo 1, foo 36, foo 38]
    print [isEven 2, isEven 5]


