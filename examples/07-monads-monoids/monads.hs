
-- Monads in Haskell can be thought of as composable computation descriptions. 
-- The essence of monad is thus separation of composition timeline from the 
-- composed computation's execution timeline, as well as the ability of 
-- computation to implicitly carry extra data, as pertaining to the computation
-- itself, in addition to its one (hence the name) output, that it will produce 
-- when run (or queried, or called upon). This lends monads to supplementing 
-- pure calculations with features like I/O, common environment, 
-- updatable state, etc.


-- https://stackoverflow.com/questions/44965/what-is-a-monad
-- http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
-- https://softwareengineering.stackexchange.com/questions/161443/different-ways-to-see-a-monad


-- https://stackoverflow.com/questions/2704652/monad-in-plain-english-for-the-oop-programmer-with-no-fp-background


{- >>= is often called "bind" and is one of the required functions in the Monad typeclass -}

{-

--- THE BIND MONAD ---

f >>= g
-- is equivalent to
do
   a <- f
   g a
-- and to
 f >>= (\a -> g a)


 -- and also that
f >> g
-- is equivalent to
_ <- f
g
-- and to
f >>= (\_ -> g)
-}

inc :: Int -> Maybe Int
inc n = Just (n + 1)

add1 :: Int -> [Int]
add1 n = [n + 1]

mult9 :: Int -> Maybe Int
mult9 n = Just (n * 9)

main = do

    putStrLn "---- >> Maybe ----"
    print $ Nothing >> (Just 0)
    print $ (Just 0) >> (Nothing :: Maybe Int)
    print $ (Just 0) >> Nothing >> (Just 1)
    print $ (Just 0) >> (Just 1) >> (Just 2)


    putStrLn "---- >>= inc Maybe ----"
    print $ Nothing >>= inc >>= inc >>= inc
    print $ (Just 0) >>= inc >>= inc >>= inc


    putStrLn "---- >> lists ----"
    print $ [] >> [1, 2]
    print $ [1, 2] >> ([] :: [Int])
    print $ [1] >> [3, 4, 5]
    print $ [1, 2] >> [3, 4, 5]
    print $ [1, 2, 3] >> [3, 4, 5]

    putStrLn "---- >>= add1 ----"
    print $ [] >>= add1 >>= add1 >>= add1
    print $ [1, 2, 3] >>= add1
    print $ [1, 2, 3] >>= add1 >>= add1
    print $ [1, 2, 3] >>= add1 >>= add1 >>= add1

    
    print $ map (+3) [2]
    print $ fmap (+3) (Just 2)

    print $ (Just 9) >>= mult9 

