import System.Environment
import Text.Read
import Data.Maybe
import qualified Day1 
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6

runners = [
  Day1.run, Day2.run, Day3.run, Day4.run, Day5.run,
  Day6.run]
  
parseInt :: String -> Maybe Int
parseInt s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing
  
runnerForDay :: Int -> Maybe (IO ())
runnerForDay n = listToMaybe $ drop (n - 1) runners
  
main :: IO ()
main = do
  args <- getArgs
  if null args then
    sequence_ runners
  else 
    case parseInt (head args) >>= runnerForDay of
      Just r -> r
      _ -> putStrLn "Usage: aoc <day>"
