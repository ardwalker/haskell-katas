
-- Import a module, must be before any functions 
import Data.List 

-- Import only specific functions 
import Data.List (nub,sort)

-- Remove specific functions
import Data.List hiding (sort)

-- Allows us to use the fully qualified name without clash with Prelude
import qualified Data.Map

-- Qualified import renaming
import qualified Data.Map as M

numUniques :: (Eq a) => [a] > Int
numUniques = length . nub




