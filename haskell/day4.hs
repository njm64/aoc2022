module Day4 where

import Util
import Data.List
import qualified Aoc

parseRange :: String -> (Int, Int)
parseRange = listToPair . map read . splitList '-'

parse :: [String] -> [((Int, Int), (Int, Int))]
parse = map (listToPair . map parseRange . splitList ',')

isSorted xs = sort xs == xs

contains ((minA, maxA), (minB, maxB)) =
  isSorted [minA, minB, maxB, maxA] ||
  isSorted [minB, minA, maxA, maxB]

overlaps ((minA, maxA), (minB, maxB)) =
  isSorted [minB, minA, maxB] ||
  isSorted [minB, maxA, maxB] ||
  isSorted [minA, minB, maxB, maxA] ||
  isSorted [minB, minA, maxA, maxB]

part1 = length . filter contains
part2 = length . filter overlaps
run = Aoc.run 4 parse part1 part2

