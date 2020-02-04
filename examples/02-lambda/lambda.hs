
-- Lambdas are basically anonymous functions that are used because we need some functions only once. 

-- Normally, we make a lambda with the sole purpose of passing it to a higher-order function.

-- Sometimes it is more convenient to use a lambda expression rather than
-- giving a function a name. This is often the case when using map and foldl
-- / foldr.

addOneList lst = map addOne lst 
                     where addOne x = x + 1


-- But here's another way, where we pass the anonymous function into map
-- rather than any named function.
addOneList' lst = map (\x -> x + 1) lst




maximum' :: (Ord a) => [a] > a
maximum' = foldr1 (\x acc > if x > acc then x else acc)

reverse' :: [a] > [a]
reverse' = foldl (\acc x > x : acc) []

product' :: (Num a) => [a] > a
product' = foldr1 (*)

filter' :: (a > Bool) > [a] > [a]
filter' p = foldr (\x acc > if p x then x : acc else acc) []

head' :: [a] > a
head' = foldr1 (\x _ > x)




-- \xs -> length xs > 15
-- \a b -> (a * 30 + 3) / b
-- \(a,b) -> a + b

main = do

  print $ (\x y -> x + y) 3 5

  print $ map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
  
  print $ (\x -> x + 1) 4  

  -- traditional way
  print $ addOneList [1,2,3]

  -- lambda way inlined
  print $ map (\x -> x + 1) [1,2,3]

  print $ filter (\x -> even x && x > 10) [1..20]  

  
  