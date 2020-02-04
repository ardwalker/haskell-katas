

module Main where 

import Patterns

main = do
  print $ factorial 6
  print $ tell' "Hello World"
  print $ tell [1, 2, 3, 4]
  print $ take' 2 [1,2,3,4,5]
  print $ lucky 7
  print $ charName 'z'
  print $ addVectors (1,2) (3,4)
  print $ first (3,6,9)

  -- List of tuples for testing list comprehension pattern matching
  let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]  
  print $ [a+b | (a,b) <- xs] 
  
  print $ bmiTell3 205.0 72.0
  print $ myCompare 1 2
  print $ initials "Andrew" "Walker"
  print $ calcBmis [(200.0,70.0),(250,72.0)]
  



