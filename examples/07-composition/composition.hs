{-
  In mathematics, function composition is defined like this:  
  (f . g)(x) = f(g(x))
  
  meaning that composing two functions produces a new function that, when called with a parameter, say, x is the equivalent of calling g with the parameter x and then calling the f with that result.
-}

(.) :: (b -> c) -> (a -> b) -> a -> c 
f . g = \x -> f (g x)


{--
    Mind the type declaration. f must take as its parameter a value that has
    the same type as g 's return value. So the resulting function takes a parameter
    of the same type that g takes and returns a value of the same type that f returns. 

    The expression negate . (* 3) returns a function that takes a number, 
    multiplies it by 3 and then negates it. 

    One of the uses for function composition is making functions on the fly to 
    pass to other functions. Sure, can use lambdas for that, but many
    times, function composition is clearer and more concise. 
    
    Say we have a list of numbers and we want to turn them all into negative numbers. One way to do that would be to get each number's absolute value and then negate it, like so:
--} 
map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]  

-- using the . operator, this can be rewritten:
map (negate . abs) [5,-3,-6,7,-3,2,-19,24]  


{-
  Function composition is right-associative, so we can compose many functions at a time. The expression f (g (z x)) is equivalent to (f . g . z) x. With that in mind, we can turn
-}
map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]  

-- into

map (negate . sum . tail) [[1..5],[3..6],[1..7]]  






sum (takeWhile (<10000) (filter odd (map (^2) [1..]))) 

-- Being such a fan of function composition, I would have probably written that like this: 
sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]


{-
  Another common use of function composition is defining functions in the so-called point free style (also called the pointless style). Take for example this function that we wrote earlier:
-}
sum' :: (Num a) => [a] -> a     
sum' xs = foldl (+) 0 xs     


{-
  The xs is exposed on both right sides. Because of currying, we can omit the xs on both sides, because calling 
-}  
foldl (+) 0 

{-
  creates a function that takes a list.
-}
sum' = foldl (+) 0 




--------------------------------------------------------------------------------
-- On readability
--------------------------------------------------------------------------------
{-
  In the section about maps and filters, we solved a problem of finding the sum of all odd squares that are smaller than 10,000. Here's what the solution looks like when put into a function.
-}
oddSquareSum :: Integer  
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))     


-- Being such a fan of function composition, I would have probably written that like this:
oddSquareSum :: Integer  
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]  


-- However, if there was a chance of someone else reading that code, I would have written it like this which would probably be easier to read than a composition chain.
oddSquareSum :: Integer  
oddSquareSum =   
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit  








