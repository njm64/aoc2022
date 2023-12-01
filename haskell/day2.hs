module Day2 where

import Data.List
import Util
import qualified Aoc

data Shape = Rock | Paper | Scissors deriving Eq
data Result = Win | Lose | Draw deriving Eq
type Rule = (Shape, Shape, Result) 

rules = [
  (Rock, Rock, Draw),
  (Rock, Paper, Win),
  (Rock, Scissors, Lose),
  (Paper, Rock, Lose),
  (Paper, Paper, Draw),
  (Paper, Scissors, Win),
  (Scissors, Rock, Win),
  (Scissors, Paper, Lose),
  (Scissors, Scissors, Draw)]

parseShape :: Char -> Shape
parseShape 'A' = Rock
parseShape 'B' = Paper
parseShape 'C' = Scissors
parseShape 'X' = Rock
parseShape 'Y' = Paper
parseShape 'Z' = Scissors

parseResult :: Char -> Result
parseResult 'X' = Lose
parseResult 'Y' = Draw
parseResult 'Z' = Win

resultToScore :: Result -> Int
resultToScore Lose = 0
resultToScore Draw = 3
resultToScore Win = 6

shapeToScore :: Shape -> Int
shapeToScore Rock = 1
shapeToScore Paper = 2
shapeToScore Scissors = 3

parse :: [String] -> [(Char, Char)]
parse = map (\s -> (head s, last s))

findResult (a, b) =
  let theirs = parseShape a in
  let ours = parseShape b in
  head [r | (s1, s2, r) <- rules, s1 == theirs && s2 == ours]

findShape (a, b) =
  let theirs = parseShape a in
  let result = parseResult b in
  head [s2 | (s1, s2, r) <- rules, s1 == theirs && r == result]

part1 = sum . (map score)
  where score (a,b) =
          let r = findResult (a,b) in
          let s = parseShape b in
          (resultToScore r) + (shapeToScore s)
          
part2 = sum . (map score)
  where score (a,b) =
          let s = findShape (a,b) in
          let r = parseResult b in
          (resultToScore r) + (shapeToScore s)

run = Aoc.run 2 parse part1 part2

