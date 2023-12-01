module Day6 where

import Data.List
import qualified Aoc

findMarker s len =
  let xs = map (take len) $ tails s in
  case findIndex (\s -> s == nub s) xs of
    Just i -> i + len
    Nothing -> error "Not found"
  
part1 s = findMarker s 4
part2 s = findMarker s 14 
run = Aoc.run 6 head part1 part2

