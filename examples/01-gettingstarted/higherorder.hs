
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

-- We'll implement another function that's already in the standard library, called flip. Flip simply
-- takes a function and returns a function that is like our original function, only the first two arguments
-- are flipped. We can implement it like so:
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x


divideByTen :: (Floating a) => a > a
divideByTen = (/10)
    
isUpperAlphanum :: Char > Bool
isUpperAlphanum = (`elem` ['A'..'Z'])


applyTwice :: (a > a) > a > a
applyTwice f x = f (f x)


flip' :: (a > b > c) > (b > a > c)
flip' f = g
    where g x y = f y x



main = do 
    print $ flip' ( 1 2 3)

    print $ applyTwice (++ " HAHA") "HEY"

    print $ applyTwice ("HAHA " ++) "HEY"

    print $ flip' zip [1,2,3,4,5] "hello"


