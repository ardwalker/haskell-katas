
module Operators where

{-
https://tech.fpcomplete.com/haskell/tutorial/operators

--      comment
{-      start multiline
-}      end multiline


>>=     bind                  monad join/sequencing operator
>>      then/sequence         monad join without value, synonym for *>
*>      then/sequence         synonym for >>
->      to                    a -> b: a to b
<-      bind/is drawn from    (as it desugars to >>=)
<>      Monoidal append       synonym for mappend
<$>     (f)map                synonym for fmap (functor map) infix fmap
<$      map-replace by        0 <$ f: "f map-replace by 0"
<*>     apply                 applicative function application
>@>     composition           object composition operator (monads)

$       apply/of              function application
&       reverse application   & is just like $ only backwards.
.       function composition  a . b: "b pipe-to a"
!!      index
!       index / strict        a ! b: "a index b", foo !x: foo strict x
<|>     or / alternative      expr <|> term: "expr or term"
++      concat / plus / append
[]      empty list
:       cons
::      of type / as          f x :: Int: f x of type Int
\       lambda
@       as                    go ll@(l:ls): go ll as l cons ls
~       lazy                  go ~(a,b): go lazy pair a, b
|       such that             guard specifier
=       is defined as
_       whatever
=>      implies/then
<=<     left fish

\\      list difference
<-      list comprehension

-}



-- This is required for & operator
import Data.Function

double :: Int -> Int
double x = x + x

square :: Int -> Int
square x = x * x

main :: IO ()
main = do 
    -- List append 
    print $ "hello " ++ "there " ++ "world"



    -- ========================================================================
    -- $ operator ; function application
    -- All this does is apply a function. So, f $ x is exactly equivalent to f x. 
    -- If so, why would you ever use $? to avoid parentheses. 
    -- ========================================================================

    -- replace
    print $ (+3) ( (+2) ((+1) 3))

    -- with
    print $ (+3) $ (+2) $ (+1) 3

    -- ========================================================================
    -- The ($ 5) bit means “apply the function to 5”, and then we can use map to use 
    -- it with both the double and square functions.
    -- ========================================================================
    print $ map ($ 5) [double, square]


    -- ========================================================================
    -- . operator  ; function composition 
    -- ========================================================================
    (print . double . square) 5
    -- Or you can combine this together with the $ operator to avoid those parentheses 
    print . double . square $ 5

    -- ========================================================================
    -- & operator ; Reverse function application
    -- & is useful because the order in which functions are applied to their 
    -- arguments read left to right instead of the reverse (which is the case 
    -- for $). This is closer to how English is read so it can improve 
    -- code clarity.
    -- ========================================================================
    print $ double $ square 5
    -- This is semantically equivalent to:        
    5 & square & double & print


    -- ========================================================================
    -- <> operator ; Monoidal append (mappend infix)
    -- ========================================================================
    putStrLn $ "hello " <> "there " <> "world!"

    -- ========================================================================
    -- <$> operator ; Functor map (fmap infix)
    -- ========================================================================
    print $ fmap (*2) (Just 200) 
    -- is equivalent to
    print $ (*2) <$> (Just 200)

    -- print (Just 2) <$ (Just 3)
    -- print 1 $> 3
    







