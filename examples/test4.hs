

tupleFromInputString :: String -> Maybe (String, String, Int)
tupleFromInputString input = if length stringComponents /= 3
  then Nothing
  else Just (stringComponents !! 0, stringComponents !! 1, age)
  where 
    stringComponents = words input
    age = (read (stringComponents !! 2) :: Int)



data Person = Person {
    firstName :: String,
    lastName :: String,
    age :: Int
} deriving Show
      
personFromTuple :: (String, String, Int) -> Person 
personFromTuple (fName, lName, age) = Person fName lName age

convertTuple :: Maybe (String, String, Int) -> Maybe Person
convertTuple Nothing = Nothing
convertTuple (Just t) = Just (personFromTuple t)
      

main = do 
    print $ tupleFromInputString "John Doe 24"
    print $ convertTuple $ Just ("John", "Doe", 24)

