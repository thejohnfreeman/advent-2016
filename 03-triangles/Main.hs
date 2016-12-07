-- stack runghc --verbosity error

import Data.List (transpose, genericLength)
import Data.List.Split (chunksOf)

parse :: String -> [[Integer]]
parse b = [map read $ words l | l <- lines b]

isGood :: [Integer] -> Bool
isGood [x, y, z] = x + y > z && y + z > x && x + z > y
isGood _ = error "not a triple"

-- Takes a 3x3 list matrix and returns the number of good triangle columns.
isGood' :: [[Integer]] -> Integer
isGood' = genericLength . filter id . map isGood . transpose

main :: IO ()
main = do
  tris <- parse <$> getContents
  print $ length $ filter isGood $ tris
  print $ sum $ map isGood' $ chunksOf 3 tris
