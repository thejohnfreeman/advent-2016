-- stack runghc --verbosity error

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.List (foldl', scanl')

data Direction = U | D | L | R
  deriving (Show, Eq)

parse :: Char -> Direction
parse 'U' = U
parse 'D' = D
parse 'L' = L
parse 'R' = R
parse _ = error "not a direction"

class Button b where
  move :: Direction -> b -> b
  toString :: b -> String

type Button1 = Int

instance Button Button1 where
  move U b = if b <= 3 then b else b - 3
  move D b = if b >= 7 then b else b + 3
  move L b = if b `mod` 3 == 1 then b else b - 1
  move R b = if b `mod` 3 == 0 then b else b + 1

  toString = show

start1 = 5 :: Button1

keypad = ["0000000"
         ,"0001000"
         ,"0023400"
         ,"0567890"
         ,"00ABC00"
         ,"000D000"
         ,"0000000"
         ]

type Button2 = (Int, Int)

instance Button Button2 where
  move U b@(r,c) = if keypad !! (r-1) !! c == '0' then b else (r-1,c)
  move D b@(r,c) = if keypad !! (r+1) !! c == '0' then b else (r+1,c)
  move L b@(r,c) = if keypad !! r !! (c-1) == '0' then b else (r,c-1)
  move R b@(r,c) = if keypad !! r !! (c+1) == '0' then b else (r,c+1)

  toString (r,c) = pure $ keypad !! r !! c

start2 = (3,1) :: Button2

getCode :: Button b => b -> [[Direction]] -> String
getCode b = concatMap toString . drop 1 . scanl' move' b
  where move' = foldl' $ flip move

main :: IO ()
main = do
  ds <- map (map parse) . lines <$> getContents :: IO [[Direction]]
  putStrLn . getCode start1 $ ds
  putStrLn . getCode start2 $ ds
