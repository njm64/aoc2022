module Day3 where

import Data.Char
import Data.List
import Util
import qualified Aoc

parse = id

charPriority :: Char -> Int
charPriority c
  | isLower c = ord c - ord 'a' + 1
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = 0

backpackScore :: String -> Int
backpackScore bp =
  let cs = chunks (length bp `div` 2) bp in
  sum . (map charPriority) . nub . foldl1 intersect $ cs

groupScore :: [String] -> Int
groupScore = charPriority . head . foldl1 intersect
  
part1 = sum . map backpackScore
part2 = sum . map groupScore . chunks 3
run = Aoc.run 3 parse part1 part2

