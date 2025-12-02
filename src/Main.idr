module Main

import System
import System.File

import Day1

solveDay : String -> String -> String -> String
solveDay "day1" "1" input = Day1.part1 input
solveDay "day1" "2" input = Day1.part2 input
solveDay day part _ = "Unknown day/part: " ++ day ++ " part " ++ part

runDay : String -> String -> IO ()
runDay day part = do
  let filename = "inputs/" ++ day ++ ".txt"
  Right contents <- readFile filename
    | Left err => putStrLn ("Error reading " ++ filename ++ ": " ++ show err)
  putStrLn (solveDay day part contents)

main : IO ()
main = do
  args <- getArgs
  case args of
    [_, day, part] => runDay day part
    _ => putStrLn "Usage: advent-of-code-2025 <day> <part>"
