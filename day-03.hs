{-|
  The idea here is very basic.
  Accept the sides as a List and verify if they pass the isTriangle validation.
  There is really no error handling of any kind (same as the other days)
  That is a definite improvement to be made, but I am keeping these simple.
-}
module AdventDay3 where

import Data.List.Split

isTriangle :: [Int] -> Bool
isTriangle [a,b,c]
  = a + b > c
  && a + c > b
  && b + c > a

parsePotentialTriangle :: String -> [Int]
parsePotentialTriangle = map read . words

getNumberOfTriangles :: String -> Int
getNumberOfTriangles = length . filter isTriangle . map parsePotentialTriangle . lines

solve = do
  s <- readFile "./day-03.txt"
  print $ getNumberOfTriangles s
