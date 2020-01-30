import qualified Data.Map as Map
import qualified Data.List as L
import Data.Char as C


findKey1 :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey1 key [] = Nothing
findKey1 key ((k,v):xs) = if key == k
    then Just v
    else findKey1 key xs

-- better as a fold
findKey2 :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey2 key = Prelude.foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
    
-- Append phone numbers for duplicate keys
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs


main = do 
    let phoneBook = [("betty","555-2938"),("bonnie","452-2928")]
        phoneBook2= [("betty","555-2938"),("bonnie","452-2928"),("patsy","205-2928"), ("patsy","306-3462")]

    print $ findKey2 "betty" phoneBook
    print $ L.lookup "betty" phoneBook

    
    print $ Map.fromList phoneBook2

    -- Gets the size of the list
    print $ Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]

    -- Create a singleton element map, then insert a new k,v
    print $ Map.insert 5 9 $ Map.singleton 3 9

    -- Is the key in the map
    print $ Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]

    print $ Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]

    print $ Map.filter C.isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]

    print $ Map.toList . Map.insert 9 2 $ Map.singleton 4 3

    print $ Map.lookup "patsy" $ phoneBookToMap phoneBook2

    print $ Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]

    print $ Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]


    




    





    
    

    
