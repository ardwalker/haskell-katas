-- ghc -o test2 test2.hs


-- solveConstraint = do 
--   x <- choose [1,2,3]
--   y <- choose [4,5,6]
--   guard (x*y == 8)
--   return (x,y)


main = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ name ++ "! This is a very nice name."
  putStrLn "Where do you live?"
  place <- getLine
  putStrLn $ place ++ " sounds cool!"
