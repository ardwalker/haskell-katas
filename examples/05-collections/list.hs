
{-
   to compile: ghc -o list list.hs
   or just run: runhaskell list.hs

    [1,2,3] is just syntactic sugar for 1:2:3:[]

    x:xs will bind the head of the list to x and the rest of it to xs
    even if there's only one element so xs ends up being an empty list.
  
    The x:xs pattern is used a lot, especially with recursive functions. 
    But patterns that have : in them only match against lists of length 1 or more. 

    If you want to bind, say, the first three elements to variables and the rest of the list to another variable, you can use something like x:y:z:zs
    It will only match against lists that have three elements or more.

-}


-- We already implemented our own length function using list comprehension. Now 
-- we'll do it by using pattern matching and a little recursion:
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

list = [1, 2, 3, 4, 5]


main = do
    print list

    print $ length' list
    print $ head list
    print $ tail list
    print $ last list
    print $ init list

    print $ list !! 3
    print $ elem 3 list

    print $ length list
    print $ null list
    print $ reverse list

    print $ take 2 list
    print $ drop 2 list

    print $ minimum list
    print $ maximum list

    print $ sum list
    print $ product list

    print [1..10]
    print ['A'..'Z']
    print [2,4..20]

    print $ take 10 $ cycle [1..4]
    print $ map (+1) list

    print $ filter (>3) list
    print $ all even list
    print $ any odd list

    print $ foldr (+) 0 list
    print $ foldr1 (+) list

    print $ foldl (+) 0 list
    print $ foldl1 (+) list

    print $ scanr (+) 0 list
    print $ scanr1 (+) list

    print $ scanl (+) 0 list
    print $ scanl1 (+) list

    print $ take 10 $ repeat 0
    print $ replicate 10 0
    print $ drop 3 list

    print $ ['a', 'b'] ++ ['c']
    print $ zip [1, 2, 3] ['a', 'b', 'c']
    print $ unzip [(1, 'a'), (2, 'b'), (3, 'c')]
    print $ zipWith (+) [1, 2, 3] [4, 5, 6]
    print $ [(x, y) | x <- [1..5], y <- ['a'..'e']]

    print $ words "Hello world"
    print $ unwords ["Hello", "world"] 
    
    -----
    
    -- Simple list comprehension
    -- Text.printf "Basic comprehension [x*2 | x <- [1..10]] = %d \n"
    print $ [x*2 | x <- [1..10]]

    -- comprehension with predicate filter 
    print $ [x*2 | x <- [1..10], x*2 >= 12]
    
    -- Let's say we want a comprehension that replaces each odd number greater than 10 with "BANG!" and each odd number that's less than 10 with "BOOM!" . If a number isn't odd, we throw it out of our list. For convenience, we'll put that comprehension inside a function so we can easily reuse it.
    print $ [ if x < 10 then "BOOM!" else "BANG!" | x <- [7..13], odd x]
    
    
    -- A list produced by a comprehension that draws from two lists of length 4 will have a length of 16, provided we don't filter them. If we have two lists, [2,5,10] and [8,10,11] and we want to get the products of all the possible combinations between numbers in those lists, here's what we'd do.
    print $ [ x*y | x <- [2,5,10], y <- [8,10,11]]
    
    
    -- What if we wanted all possible products that are more than 50?
    print $ [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
    
    -- Remove all non uppercase 
    let removeNonUppercase st = do {
      [ c | c <- st, c `elem` ['A'..'Z']]
    }    
    print $ removeNonUppercase "AbCdE"

    

    -- RECURSION 
    -- replicate' n x 
    --   | n <= 0 = []
    --   | otherwise = x:replicate' (n-1) x    
    -- 
    -- print $ replicate' 4 5
    
    
    
  
  