
module FizzBuzz (fizzbuzz) where 
  
fizzbuzz :: [Int] -> [String] 
fizzbuzz numbers = map display numbers

display :: Int -> String 
display num 
  | num `mod` 3 == 0 && num `mod` 5 == 0 = "FizzBuzz"
  | num `mod` 3 == 0 = "Fizz"
  | num `mod` 5 == 0 = "Buzz"
  | otherwise = (show num)


