

-- map funtion
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- filter function
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs


chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)


numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

    
sum' :: (Num a) => [a] > a
sum' xs = foldl (\acc x > acc + x) 0 xs


main = do 
    print $ map' (+3) [1,5,3,1,6]

    print $ map' (++ "!") ["BIFF", "BANG", "POW"]

    print $ map' (replicate 3) [3..6]

    print $ map' fst [(1,2),(3,5),(6,3),(2,6),(2,5)]

    print $ filter' (>3) [1,5,3,2,1,6,4,3,2,1]
    
    print $ filter' (==3) [1,2,3,4,5]

    print $ filter' even [1..10]

    let notNull x = not (null x) in filter' notNull [[1,2,3], [], [3,4,5], [2,2], [], [], []]
    print $ notNull "a"

    print $ filter' (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"

    -- Get all sum of all odd squares less than 10,000 
    print $ sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
    print $ sum (takeWhile (<10000) [n^2 | n <-[1..], odd (n^2)])  -- list comprehension 




