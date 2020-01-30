-- (.) :: (b > c) > (a > b) > a > c
-- f . g = \x > f (g x)

{--
    Mind the type declaration. f must take as its parameter a value that has
    the same type as g 's return value. So the resulting function takes a parameter
    of the same type that g takes and returns a value of the same type that f returns. 

    The expression negate . (* 3) returns a function that takes a number, 
    multiplies it by 3 and then negates it. 

    One of the uses for function composition is making functions on the fly to 
    pass to other functions. Sure, can use lambdas for that, but many
    times, function composition is clearer and more concise. 
--} 

oddSquareSum1 :: Integer
oddSquareSum1 = sum (takeWhile (<10000) (filter odd (map (^2) [1..]))) 

-- Being such a fan of function composition, I would have probably written that like this: 
oddSquareSum2 :: Integer
oddSquareSum2 = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

-- Easier to understand version
oddSquareSum3 :: Integer
oddSquareSum3 = 
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in sum belowLimit


main = do
    -- Lambda version
    print $ map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
    -- composition version - with the period .
    print $ map (negate . abs) [5,-3,-6,7,-3,2,-19,24]


    -- Lambda version
    print $ map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
    -- composition version - with the period .
    print $ map (negate . sum . tail) [[1..5],[3..6],[1..7]]


    print $ oddSquareSum3 






