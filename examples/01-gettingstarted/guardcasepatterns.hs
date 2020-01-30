
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

-- or with guards

absolute x
  | x < 0     = -x
  | otherwise = x
  

-- Golf scoring 

holeScore :: Int -> Int -> String
holeScore strokes par
  | strokes < par = show (par-strokes) ++ " under par"
  | strokes == par = "level par"
  | strokes > par = show(strokes-par) ++ " over par"


-- How could we tidy this up? Maybe we could turn the final guard into 
-- otherwise and also refactor with a where clause.

holeScore :: Int -> Int -> String
holeScore strokes par
  | score < 0 = show (abs score) ++ " under par"
  | score == 0 = "level par"
  | otherwise = show(score) ++ " over par"
 where score = strokes-par  



{-
   =============================================================================

   Case expressions 

   Recall from last week how we defined algebraic data types like binary trees. 
   A value with an algebraic data type may have one of several different forms 
   — such as a Leaf or a Node, in the case of Tree structures. 
   Therefore to process such a value we need several segments of code, one 
   for each possible form. The case expression examines the value, and chooses 
   the corresponding clause. It’s like a guard, but it selects based on the 
   form of the value, i.e. it does pattern matching.

   =============================================================================
-}

-- For the given algebraic data type 
data Pet = Cat | Dog | Fish

-- here is how I greet my pets.

hello :: Pet -> String
hello x = 
  case x of
    Cat -> "meeow"
    Dog -> "woof"
    Fish -> "bubble"


data Pet = Cat | Dog | Fish | Parrot String

hello :: Pet -> String
hello x = 
  case x of
    Cat -> "meeow"
    Dog -> "woof"
    Fish -> "bubble"
    Parrot name -> "pretty " ++ name

hello (Parrot "polly")
---> "Pretty polly"

-- The catchall underscore _ is used like 'otherwise' in Guards 

hello :: Pet -> String
hello x =
  case x of
    Parrot name -> "pretty " ++ name
    _ -> "grunt"


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



