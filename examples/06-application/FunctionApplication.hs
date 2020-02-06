
{-
  $ is the function application operator. 
  
-}
($) :: (a -> b) -> a -> b  
f $ x = f x 

{-
It's just function application! Well, almost, but not quite! Whereas normal function application (putting a space between two things) has a really high precedence, the $ function has the lowest precedence. Function application with a space is left-associative (so f a b c is the same as ((f a) b) c)), function application with $ is right-associative.


Most of the time, it's a convenience function so that we don't have to write so many parentheses. Consider the expression 
-}

sum (map sqrt [1..130])

-- Because $ has such a low precedence, we can rewrite that expression more succinctly as :
sum $ map sqrt [1..130]

{-
  When a $ is encountered, the expression on its right is applied as the parameter to the function on its left. How about 

  sqrt 3 + 4 + 9? This adds together 9, 4 and the square root of 3. If we want get the square root of 3 + 4 + 9, we'd have to write 
-}
sqrt (3 + 4 + 9) 

{-
  or if we use $ we can write it because $ has the lowest precedence of any operator. That's why you can imagine a $ being sort of the equivalent of writing an opening parentheses and then writing a closing one on the far right side of the expression.
-}
sqrt $ 3 + 4 + 9 


-- How about 

sum (filter (> 10) (map (*2) [2..10]))

-- Well, because $ is right-associative, 
f (g (z x)) 
-- is equal to 
f $ g $ z x

-- And so, we can rewrite 
sum (filter (> 10) (map (*2) [2..10])) 
-- as 
sum $ filter (> 10) $ map (*2) [2..10].


{-
  But apart from getting rid of parentheses, $ means that function application can be treated just like another function. That way, we can, for instance, map function application over a list of functions.
-}
ghci> map ($ 3) [(4+), (10*), (^2), sqrt]  
[7.0,30.0,9.0,1.7320508075688772] 










