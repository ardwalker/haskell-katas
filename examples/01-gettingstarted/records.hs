

data Car a b c = Car { company :: a
                    , model :: b
                    , year :: c } deriving (Show)

-- tellCar :: Car  -> String
-- tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y


data Person = Person { firstName :: String
                    , lastName :: String
                    , age :: Int
                    } deriving (Eq, Show, Read)


main = do
    -- let stang = Car {company="Ford", model="Mustang", year=1967}
    -- tellCar stang
    let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
    let adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
    let mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}
    
    let beastieBoys = [mca, adRock, mikeD]
    print $ mikeD `elem` beastieBoys  -- from Eq

    print $ mikeD  -- from Show
    

    
    
