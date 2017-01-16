{-|
  The room data is read in as a line-delimited string.
  The overall approach is roughly as follows:
  1. Parse the input into a List of Rooms
  2. Filter out all Room instances where the provided checksum does not match
      the provided checksum
  3. Then sum the sector values of the remaining rooms.
-}
module AdventDay4 where

import Control.Arrow ((>>>))
import Data.List.Split
import qualified Data.Map as DMap

data Room =
  Room { name :: String
       , sector :: Int
       , checksum :: String
       }
  deriving (Show)

-- Builds up a Map of character to count key values
-- saw a much more clever way to do this in Franklin's solution:
-- https://github.com/FranklinChen/advent-of-code2016/blob/master/src/Day04.hs#L52-L54
charCounter :: DMap.Map Char Int -> String -> DMap.Map Char Int
charCounter m [] = m
charCounter m (c:cs) = charCounter increment cs
  where
    increment = DMap.insertWithKey (\_ _ oldVal -> oldVal + 1) c 1 m


-- Use quicksort algorithm to first sort on count desc and then char asc
-- Again, saw a nicer way in Franklin's solution using built in sorting:
-- https://github.com/FranklinChen/advent-of-code2016/blob/master/src/Day04.hs#L60-L62
-- I need to read up on that a little more because it seems very useful
sortByCountThenCharacter :: [(Char, Int)] -> [(Char, Int)]
sortByCountThenCharacter [] = []
sortByCountThenCharacter ((k1, v1):xs) =
  sortByCountThenCharacter larger ++ [(k1,v1)] ++ sortByCountThenCharacter smaller
  where
    smaller = filter (\(k2,v2) -> if v1 == v2 then k2 > k1 else v2 < v1) xs
    larger = filter (\(k2,v2) -> if v1 == v2 then k2 < k1 else v2 > v1) xs

-- Produces the correct checksum for a Room according to challenge rules
-- We can use this to compare to the Room's checksum to see if it is real.
produceChecksum :: Room -> String
produceChecksum =
  name
  >>> charCounter DMap.empty
  >>> DMap.toDescList
  >>> sortByCountThenCharacter
  >>> take 5
  >>> map fst

-- Simply compare the existing checksum to the correctly calculated checksum
isRealRoom :: Room -> Bool
isRealRoom r = checksum r == produceChecksum r


sumRealRoomSectors :: [Room] -> Int
sumRealRoomSectors =
  filter isRealRoom
  >>> map sector
  >>> sum

-- Again, very basic parsing like the other challenges
-- After splitting the string we reverse it so we can easily pattern match.
-- "bkwzkqsxq-tovvilokx-nozvyiwoxd-172[fstek]" -> Room "bkwzkqsxqtovvilokxnozvyiwoxd","172","fstek"
parseOneRoom :: String -> Room
parseOneRoom =
  splitOneOf "-[]"
  >>> reverse . init
  >>> (\(check:sector:name) -> Room (concat name) (read sector) check)

parseRooms :: String -> [Room]
parseRooms =
  lines
  >>> map parseOneRoom

-- Answer: 185371
solve = do
  rooms <- parseRooms <$> readFile "./day-04.txt"
  print $ sumRealRoomSectors rooms
