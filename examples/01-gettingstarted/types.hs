
import qualified Data.Map as Map

-- type PhoneBook = [(String,String)]


type PhoneNumber = String
type Name = String
-- a better phonebook
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

-- Type parameterization
type AssocList k v = [(k,v)]

{-
functions, we can partially apply type parameters and get new type constructors from them. 
Just like we call a function with too few parameters to get back a
new function, we can specify a type constructor with too few type parameters 
and get back a partially applied type constructor. 
If we wanted a type that represents a map (from Data.Map ) from integers to something, 
we could either do this: 
-}

type IntMap v = Map.Map Int v

-- type IntMap = Map Int

{-
Make sure that you really understand the distinction between type constructors and 
value constructors. Just because we made a type synonym called IntMap or AssocList 
doesn't mean that we can do stuff like AssocList [(1,2),(4,5),(7,9)] . 
All it means is that we can refer to its type by using different names. 
We can do [(1,2),(3,5),(8,9)] :: AssocList Int Int , which will make the numbers 
inside assume a type of Int , but we can still use that list as we would any normal 
list that has pairs of integers inside. 
-}

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
    case Map.lookup lockerNumber map of 
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken 
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!" 

lockers :: LockerMap
lockers = Map.fromList [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

-- Assigning a type within a where clause
triple x = tripleItYo x 
    where tripleItYo :: Integer -> Integer 
          tripleItYo y = y * 3
                  

main = do 
    print $ lockerLookup 100 lockers
    print $ lockerLookup 101 lockers
    print $ lockerLookup 110 lockers


