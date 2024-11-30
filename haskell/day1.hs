module Day1 where

import Data.List
import Util
import qualified Aoc

parse :: [String] -> [[Int]]
parse lines = map (map read) $ splitWith null lines
  
part1 = maximum . (map sum)
part2 = sum . take 3 . reverse . sort . (map sum)
run = Aoc.run 1 parse part1 part2
