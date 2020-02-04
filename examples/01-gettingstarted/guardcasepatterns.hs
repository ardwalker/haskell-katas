
{-
From https://stackoverflow.com/questions/4156727/what-is-the-difference-between-pattern-matching-and-guards

Pattern Matching vs Guards

some rough guides:

- use pattern matching for anything that can be matched directly one 
or two constructors deep, where you don't really care about the compound data 
as a whole, but do care about most of the structure. 

The @ syntax lets you bind the overall structure to a variable while also 
pattern matching on it, but doing too much of that in one pattern can get 
ugly and unreadable quickly.

- use guards when you need to make a choice based on some property 
that doesn't correspond neatly to a pattern, e.g. comparing two Int values 
to see which is larger.

If you need only a couple pieces of data from deep inside a large structure, 
particularly if you also need to use the structure as a whole, guards and 
accessor functions are usually more readable than some monstrous 
pattern full of @ and _.

If you need to do the same thing for values represented by different patterns, 
but with a convenient predicate to classify them, using a single generic 
pattern with a guard is usually more readable. 
Note that if a set of guards is non-exhaustive, anything that fails all the 
guards will drop down to the next pattern (if any). So you can combine a 
general pattern with some filter to catch exceptional cases, then do pattern 
matching on everything else to get details you care about.

Definitely don't use guards for things that could be trivially checked 
with a pattern. 

Checking for empty lists is the classic example, use a pattern match for that.

In general, when in doubt, just stick with pattern matching by default, 
it's usually nicer. If a pattern starts getting really ugly or convoluted, 
then stop to consider how else you could write it. Besides using guards, 
other options include extracting subexpressions as separate functions or 
putting case expressions inside the function body in order to push some of 
the pattern matching down onto them and out of the main definition.

-}


{- 
   =============================================================================

   Guards 

   Notice how there is no equals sign on the first line of the function 
   definition — but there is an equals sign after each guard.
   
   The otherwise guard should always be last, it’s like the default case in a 
   C-style switch statement. 
   
   Guards are easier to read than if/then/else if there are more than two 
   conditional outcomes

   ============================================================================= 
-}

-- an if/then/else conditional

absolute x = if (x<0) then (-x) else x


{- 
   =============================================================================
   Pattern matching 
   =============================================================================
-}

factorial :: Int -> Int
factorial 0 = 1
factorial n = n *  factorial(n - 1)


-- Here we use a type class to make sure we can convert the input (inner) types to strings
--------------------------------------------------------------------------------
tell :: (Show a) => [a] -> String
tell [] = "Empty"
tell (x:[]) = "Contains only 1 item: " ++ show x
tell (x:y:[]) = "Contains only 2 items: " ++ show x ++ " and " ++ show y
tell all@(x:y:_) = "Contains many items... "++ show all



