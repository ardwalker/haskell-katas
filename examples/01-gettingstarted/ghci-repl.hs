ghci

:?        -- help
:module   -- loads a module ; e.g. :module + Data.Ratio
:info     -- prints information :info (+)
:set +t   -- prints the type
:show bindings
:[t]ype   -- prints type  
:{        -- starts multiline entry

-- =============================================================================
-- 9. The Haskell REPL
-- =============================================================================

-- Start the repl by typing `ghci`.
-- Now you can type in Haskell code. Any new values
-- need to be created with `let`:

let foo = 5

-- You can see the type of any value or expression with `:t`:

> :t foo
foo :: Integer


-- The a in the type is lower case, which means its a type variable 
-- this is like a generic. Functions that have type variables are called polymorphic functions.
ghci> :t head
head :: [a] -> a


-- Operators, such as `+`, `:` and `$`, are functions.
-- Their type can be inspected by putting the operator in parentheses:
> :t (:)
(:) :: a -> [a] -> [a]



-- You can get additional information on any `name` using `:i`:
> :i (+)
class Num a where
  (+) :: a -> a -> a
  ...

-- Defined in ‘GHC.Num’
infixl 6 +


-- You can also run any action of type `IO ()`
> sayHello
What is your name?
Friend!
Hello, Friend!





