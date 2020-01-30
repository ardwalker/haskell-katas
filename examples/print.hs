

-- print2.hs 

module Print2 where 

myGreeting :: String 
myGreeting = "hello" ++ " world!" 

hello :: String 
hello = "hello" 

world :: String 
world = "world!" 

topLevelFunction :: Integer -> Integer 
topLevelFunction x = x + woot + topLevelValue 
    where woot :: Integer woot = 10 

topLevelValue :: Integer 
topLevelValue = 5 

-- In the above, you could import and use topLevelFunction or topLevelValue 
-- from another module, and they are accessible to everything else in the module. 
-- However, woot is effectively invisible outside of topLevelFunction. 
-- The where and let clauses in Haskell introduce local bindings or declarations.



main :: IO () 
main = do 
    putStrLn "Count to four for me:" 
    putStr "one, two" 
    putStr ", three, and" 
    putStrLn " four!"

    putStrLn myGreeting 
    
    putStrLn secondGreeting 
    where secondGreeting = concat [hello, " ", world]






