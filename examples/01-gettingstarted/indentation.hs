

{-
INDENTATIONS 
https://en.wikibooks.org/wiki/Haskell/Indentation
-}

module Main where

-- Layout line up 
foo1 :: Double -> Double
foo1 x =
    let s = sin x
        c = cos x
    in 2 * s * c

-- can be rewritten without caring about the indentation rules as:
foo2 :: Double -> Double;
foo2 x = let {
    s = sin x;
    c = cos x;
} in 2 * s * c


{-
-- Wrong
if foo
   then do first thing
        second thing
        third thing
   else do something_else

-- Right
if foo
   then do first thing
           second thing
           third thing
   else do something_else

-- Right
if foo
   then do
     first thing
     second thing
     third thing
   else do 
     something_else
-}

-- THIS IS INCORRECT, REWRITE BELOW --
-- doGuessing num = do
--     putStrLn "Enter your guess:"
--     guess <- getLine
--     case compare (read guess) num of
--       LT -> do putStrLn "Too low!"
--                doGuessing num
--       GT -> do putStrLn "Too high!"
--                doGuessing num
--       EQ -> putStrLn "You Win!"

-- Rewritten as 
doGuessing num = do {
    putStrLn "Enter your guess:";
    guess <- getLine;
    case compare (read guess) num of {
        LT -> do {
            putStrLn "Too low!";
            doGuessing num;
        };
        GT -> do {
            putStrLn "Too high!";
            doGuessing num;
        };
        EQ -> putStrLn "You Win!";
    };
};

{-
-- wrong	
do first thing
second thing
third thing

-- wrong
do first thing
 second thing
 third thing

-- right
do first thing
   second thing
   third thing

-- right
do
  first thing
  second thing
  third thing
-}



-- One circumstance in which explicit braces and semicolons can be 
-- convenient is when writing one-liners in GHCi:
-- let foo :: Double -> Double; foo x = let { s = sin x; c = cos x } in 2 * s * c

main :: IO ()
main = do 
    putStrLn "Hello! What is your name?"
    name <- getLine
    -- This statement either needs parens or $
    -- let out = "Nice to meet you, " ++ name ++ "!"
    putStrLn $ "Nice to meet you, " ++ name ++ "!"

    