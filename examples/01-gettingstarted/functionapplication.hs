
-- Function application with $ 

-- $ function, also called function application 

-- ($) :: (a > b) > a > b
-- f $ x = f x

-- Whereas normal function application (putting a space between two things) 
-- has a really high precedence, the $ function has the lowest precedence. 

-- Function application with a space is left-associative (so f a b c is
-- the same as ((f a) b) c) ), function application with $ is right-associative. 
 
-- Most of the time, it's a convenience function so that we don't have to
-- write so many parentheses. 

main = do 
    -- sqrt 3 + 4 + 9 
    print $ sqrt (3 + 4 + 9)
    print $ sqrt $ 3 + 4 + 9

    -- Example, this 
    print $ sum (filter (> 10) (map (*2) [2..10])) 
    -- Can be rewritten as -
    print $ sum $ filter (> 10) $ map (*2) [2..10] 




