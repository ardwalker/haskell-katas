

main = do 

    print "--- using fmap ---"
    print $ fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")
    print $ fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing

    print $ fmap (*2) (Just 200)
    print $ fmap (*2) Nothing

    print $ fmap (+3) (Just 2)

    -- <$> is infix fmap 
    print "--- using fmap infix <$> ---"
    print $ (++ " HEY GUYS IM INSIDE THE JUST") <$> (Just "Something serious.")
    print $ (++ " HEY GUYS IM INSIDE THE JUST") <$> Nothing

    print $ (*2) <$> (Just 200)
    print $ (*2) <$> Nothing
    print $ (+3) <$> (Just 2)




