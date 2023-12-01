module Aoc where

import System.IO
import System.CPUTime
import Text.Printf

testMode = False

inputDir testMode
  | testMode = "../test"
  | otherwise = "../input"

readInputWithMode :: Bool -> Int -> IO [String]
readInputWithMode testMode day = do
  let filename = (inputDir testMode) ++ "/day" ++ show day ++ ".txt"
  contents <- readFile filename
  let input = lines contents
  return input

readInput = readInputWithMode False
readTestInput = readInputWithMode True

getTime :: IO Double
getTime = do
  t <- getCPUTime
  return ((fromInteger t) * 1e-12)

runPart :: Int -> Int -> a -> (a -> Int) -> IO ()
runPart dayNum partNum input f = do
  putStr $ printf "Day %02d Part %d: " dayNum partNum
  t1 <- getTime
  let !r = f input
  t2 <- getTime
  putStrLn $ printf "%-15d %.6fs" r (t2 - t1)

runWithMode testMode d parse part1 part2 = do
  input <- readInputWithMode testMode d
  let parsed = parse input
  runPart d 1 parsed part1
  runPart d 2 parsed part2

run = runWithMode False
test = runWithMode True


