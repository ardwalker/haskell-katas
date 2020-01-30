

--------------------------------------------------------------------------------
-- case expressions: Here's how you could parse command line arguments
--------------------------------------------------------------------------------

ex1 :: String -> String
ex1 args = case args of
  "help"  -> "Help"
  "start" -> "startProgram"
  _       -> "bad args"


ex2 :: String -> Int
ex2 s = case s of
  []      -> 3
  ('H':s) -> length s
  _       -> 7


-- Case expression takes a polymorphic list of any type and returns a string
ex3 :: [a] -> String  
ex3 xs = "The list is " ++ case xs of 
  [] -> "empty."  
  [x] -> "a singleton list."   
  xs -> "a longer list." 



main = do 
  print $ ex1 "start"

  print $ ex2 "Hello"

  print $ ex3 [1]

  let ex4 xs = do {
    "The list is " ++ case xs of 
    [] -> "empty."
    [x] -> "a singleton list."
    xs -> "a longer list."
  }

  print $ ex4 [1,2,3]


