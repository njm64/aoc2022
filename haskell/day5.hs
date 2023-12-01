module Day5 where

import Data.Array
import Data.Char
import Util
import qualified Aoc

type Rule = (Int, Int, Int) -- Count, From, To
type Crate = Char
type Stack = [Crate]
type StackArray = Array Int Stack

parseStackRow :: Int -> String -> [(Int, Crate)]
parseStackRow cols s =
  [(col, crate) | col <- [1..cols],
                  let crate = s !! (col * 4 - 3),
                  isUpper crate]

parseStacks :: [String] -> StackArray
parseStacks lines =
  let revLines = tail (reverse lines) in
  let cols = length (words (head revLines)) in
  let stackRows = concatMap (parseStackRow cols) revLines in
  accumArray (flip (:)) [] (1, cols) stackRows 

parseRule :: String -> Rule
parseRule line = (read (ws !! 1), read (ws !! 3), read (ws !! 5))
  where ws = words line
  
parse :: [String] -> (StackArray, [Rule])
parse lines =
  let (stacks, rules) = splitPair "" lines in
  (parseStacks stacks, map parseRule rules)

move :: StackArray -> Rule -> StackArray
move sa (count, src, dst) =
  let crates = take count $ (sa ! src) in
  sa // [(src, drop count $ sa ! src),
         (dst, reverse crates ++ sa ! dst)]

move2 :: StackArray -> Rule -> StackArray
move2 sa (count, src, dst) =
  let crates = take count $ (sa ! src) in
  sa // [(src, drop count $ sa ! src),
         (dst, crates ++ sa ! dst)]

part1 (stacks, rules) =
  let stacks' = foldl move stacks rules in
  map head (elems stacks')
  
part2 (stacks, rules) =
  let stacks' = foldl move2 stacks rules in
  map head (elems stacks')

run = Aoc.run 5 parse part1 part2
