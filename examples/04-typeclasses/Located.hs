
{-
Typeclass example 
-}

-- Located typeclass 
-- Location, in two dimensions.
class Located a where
    getLocation :: a -> (Int, Int)

-- Movable typeclass, where a must be of type Located
class (Located a) => Movable a where
    setLocation :: (Int, Int) -> a -> a



-- An example type, with accompanying instances.
-- Define the NamedPoint type
data NamedPoint = NamedPoint
    { pointName :: String
    , pointX    :: Int
    , pointY    :: Int
    } deriving (Show)
    
instance Located NamedPoint where -- add Located typeclass to named point
    getLocation p = (pointX p, pointY p)

instance Movable NamedPoint where -- add Movable typeclass 
    setLocation (x, y) p = p { pointX = x, pointY = y }



-- Moves a value of a Movable type by the specified displacement.
-- This works for any movable, including NamedPoint.
move :: (Movable a) => (Int, Int) -> a -> a
move (dx, dy) p = setLocation (x + dx, y + dy) p
    where
    (x, y) = getLocation p
