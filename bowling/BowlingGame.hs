
module BowlingGame (roll) where


roll :: [Int] -> Int
roll [] = 0 
roll (rollOne:rollTwo:[]) = rollOne + rollTwo 
roll (rollOne:rest)  | isStrike = 10 + rest!!0 + rest!!1 + roll(rest)
  where isStrike = rollOne == 10
roll (rollOne:rollTwo:rest) | isSpare = 10 + rest!!0 + roll(rest)
  where isSpare = rollOne + rollTwo == 10
roll (rollOne:rollTwo:rest) = rollOne + rollTwo + roll(rest)