{-|
  The general idea is to assume starting at position (0,0) facing North.
  We fold over the directions producing a new position with each turn.
  The last position produced will show how far we have traveled from the origin.
-}
module AdventDay1 (Turn (..)) where

import Data.List.Split

data Turn = R | L deriving (Read, Show)

data Direction = North | East | South | West deriving Show

data Position = Position { facing :: Direction, coords :: (Int, Int) } deriving Show

move :: Position -> (Turn, Int) -> Position
move Position { facing=North, coords=(x,y) } (R, amount) = Position East (x + amount, y)
move Position { facing=East, coords=(x,y) } (R, amount) = Position South (x, y - amount)
move Position { facing=South, coords=(x,y) } (R, amount) = Position West (x - amount, y)
move Position { facing=West, coords=(x,y) } (R, amount) = Position North (x, y + amount)
move Position { facing=North, coords=(x,y) } (L, amount) = Position West (x - amount, y)
move Position { facing=East, coords=(x,y) } (L, amount) = Position North (x, y + amount)
move Position { facing=South, coords=(x,y) } (L, amount) = Position East (x + amount, y)
move Position { facing=West, coords=(x,y) } (L, amount) = Position South (x, y - amount)

parseSingleDirection :: String -> (Turn, Int)
parseSingleDirection (x:xs) = (read [x], read xs)

parseDirections :: String -> [(Turn, Int)]
parseDirections = map parseSingleDirection . splitOn ", "

followDirections :: String -> Position
followDirections = foldl move (Position North (0,0)) . parseDirections

directions = "R3, L5, R2, L2, R1, L3, R1, R3, L4, R3, L1, L1, R1, L3, R2, L3, L2, R1, R1, L1, R4, L1, L4, R3, L2, L2, R1, L1, R5, R4, R2, L5, L2, R5, R5, L2, R3, R1, R1, L3, R1, L4, L4, L190, L5, L2, R4, L5, R4, R5, L4, R1, R2, L5, R50, L2, R1, R73, R1, L2, R191, R2, L4, R1, L5, L5, R5, L3, L5, L4, R4, R5, L4, R4, R4, R5, L2, L5, R3, L4, L4, L5, R2, R2, R2, R4, L3, R4, R5, L3, R5, L2, R3, L1, R2, R2, L3, L1, R5, L3, L5, R2, R4, R1, L1, L5, R3, R2, L3, L4, L5, L1, R3, L5, L2, R2, L3, L4, L1, R1, R4, R2, R2, R4, R2, R2, L3, L3, L4, R4, L4, L4, R1, L4, L4, R1, L2, R5, R2, R3, R3, L2, L5, R3, L3, R5, L2, R3, R2, L4, L3, L1, R2, L2, L3, L5, R3, L1, L3, L4, L3"

solve :: Position
solve = followDirections directions
