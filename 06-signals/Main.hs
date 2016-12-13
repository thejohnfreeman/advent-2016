-- stack runghc --verbosity error

import qualified Data.Map.Strict as Map
import Data.List (foldl', maximumBy, transpose)
import Data.Function (on)

type Counts k = Map.Map k Integer

-- Count characters in a single list
count1 :: Ord k => [k] -> Counts k
count1 = foldl' f Map.empty
  where f map c = Map.insertWithKey (const (+)) c 1 map

-- Count characters in each column of a list of lines
countByColumn :: Ord k => [[k]] -> [Counts k]
countByColumn = map count1 . transpose

maximumKeyBy :: (a -> a -> Ordering) -> Map.Map k a -> k
maximumKeyBy compare' = fst . maximumBy (compare' `on` snd) . Map.toList

main :: IO ()
main = do
  counts <- countByColumn . lines <$> getContents
  putStrLn $ map (maximumKeyBy compare) counts
  putStrLn $ map (maximumKeyBy $ flip compare) counts
