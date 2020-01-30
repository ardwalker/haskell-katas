
import Control.Applicative

-- (<$>), which lifts a single-argument function into a Functor
-- (<*>), which chains a multi-argument function through an Applicative
-- (=<<), which binds a function that enters a Monad onto an existing computation

-- (<*>) -- Tie Fighter
-- (*>)  -- Right Tie
-- (<*)  -- Left Tie
-- pure  -- also called "return"


main = do
    print $ (pure 1 :: Maybe Int)

    print $ "--- applicative maybe ---"
    print $ Just (+ 1) <*> Nothing
    print $ Just (+ 1) <*> Just 2

    print $ "--- applicative list ---"
    print $ [(+ 1), (* 2)] <*> []
    print $ [(+ 1), (* 2)] <*> [1, 2, 3]

    -- returns value on left 
    print $ "--- applicative left value ---"
    print $ Just 1 <* Just 2
    -- returns value on right
    print $ "--- applicative right value ---"
    print $ Just 1 *> Just 2

    print $ "--- functors ---"
    print $ (+ 1) <$> Nothing
    print $ (+ 1) <$> Just 2

    print $ "--- functor left val ---"
    print $ 1 <$ Nothing
    print $ 1 <$ Just 2

    print $ Nothing <**> Just (+ 2)
    print $ Just 1 <**> Just (+ 2)

    print $ liftA (+ 1) Nothing
    print $ liftA (+ 1) $ Just 2

    print $ liftA2 (+) Nothing Nothing
    print $ liftA2 (+) (Just 1) (Just 2)

    print $ (+) <$> Just 1 <*> Nothing
    print $ (+) <$> Just 1 <*> Just 2
    
    