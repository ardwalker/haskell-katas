module CompositionSpec where

import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Composition" $ do
    
      it "sum  (takeWhile (<10000)  (filter odd  (map (^2) [1..])))" $
        sum (takeWhile (<10000) (filter odd (map (^2) [1..]))) == 166650

      it "sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]" $
        ( sum . takeWhile (<10000) . filter odd . map (^2) $ [1..] ) == 166650


-- oddSquareSum2 = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
-- 
-- -- Easier to understand version
-- oddSquareSum3 :: Integer
-- oddSquareSum3 = 
--     let oddSquares = filter odd $ map (^2) [1..]
--         belowLimit = takeWhile (<10000) oddSquares
--     in sum belowLimit
-- 
-- 
-- main = do
--     -- Lambda version
--     print $ map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
--     -- composition version - with the period .
--     print $ map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
-- 
-- 
--     -- Lambda version
--     print $ map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
--     -- composition version - with the period .
--     print $ map (negate . sum . tail) [[1..5],[3..6],[1..7]]
-- 
-- 
--     print $ oddSquareSum3 
-- 
-- 
    
    