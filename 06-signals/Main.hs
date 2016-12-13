-- stack runghc --verbosity error

import qualified Data.Map.Strict as Map
import Data.List (foldl', maximumBy, transpose)
import Data.Function (on)

type CharCounts = Map.Map Char Integer

-- Count characters in a single list
count1 :: [Char] -> CharCounts
count1 = foldl' f Map.empty
  where f map c = Map.insertWithKey (const (+)) c 1 map

-- Count characters in each column of a list of lines
count :: [[Char]] -> [CharCounts]
count = map count1 . transpose

opposite :: Ordering -> Ordering
opposite LT = GT
opposite EQ = EQ
opposite GT = LT

-- TODO: Think of a better name or symbol.
-- Take a binary function and map the result.
(.$) :: (b -> c) -> (a -> a -> b) -> a -> a -> c
g .$ f = \a b -> g $ f a b

maximumKeyBy :: (a -> a -> Ordering) -> Map.Map k a -> k
maximumKeyBy compare' = fst . maximumBy (compare' `on` snd) . Map.toList

main = do
  counts <- count . lines <$> getContents
  putStrLn $ map (maximumKeyBy compare) $ counts
  putStrLn $ map (maximumKeyBy $ opposite .$ compare) $ counts
