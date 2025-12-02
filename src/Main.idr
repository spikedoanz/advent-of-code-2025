module Main

import System
import System.File

import Day0
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25

solveDay : String -> String -> String -> String
solveDay "day0" "1" input = Day0.part1 input
solveDay "day0" "2" input = Day0.part2 input
solveDay "day1" "1" input = Day1.part1 input
solveDay "day1" "2" input = Day1.part2 input
solveDay "day2" "1" input = Day2.part1 input
solveDay "day2" "2" input = Day2.part2 input
solveDay "day3" "1" input = Day3.part1 input
solveDay "day3" "2" input = Day3.part2 input
solveDay "day4" "1" input = Day4.part1 input
solveDay "day4" "2" input = Day4.part2 input
solveDay "day5" "1" input = Day5.part1 input
solveDay "day5" "2" input = Day5.part2 input
solveDay "day6" "1" input = Day6.part1 input
solveDay "day6" "2" input = Day6.part2 input
solveDay "day7" "1" input = Day7.part1 input
solveDay "day7" "2" input = Day7.part2 input
solveDay "day8" "1" input = Day8.part1 input
solveDay "day8" "2" input = Day8.part2 input
solveDay "day9" "1" input = Day9.part1 input
solveDay "day9" "2" input = Day9.part2 input
solveDay "day10" "1" input = Day10.part1 input
solveDay "day10" "2" input = Day10.part2 input
solveDay "day11" "1" input = Day11.part1 input
solveDay "day11" "2" input = Day11.part2 input
solveDay "day12" "1" input = Day12.part1 input
solveDay "day12" "2" input = Day12.part2 input
solveDay "day13" "1" input = Day13.part1 input
solveDay "day13" "2" input = Day13.part2 input
solveDay "day14" "1" input = Day14.part1 input
solveDay "day14" "2" input = Day14.part2 input
solveDay "day15" "1" input = Day15.part1 input
solveDay "day15" "2" input = Day15.part2 input
solveDay "day16" "1" input = Day16.part1 input
solveDay "day16" "2" input = Day16.part2 input
solveDay "day17" "1" input = Day17.part1 input
solveDay "day17" "2" input = Day17.part2 input
solveDay "day18" "1" input = Day18.part1 input
solveDay "day18" "2" input = Day18.part2 input
solveDay "day19" "1" input = Day19.part1 input
solveDay "day19" "2" input = Day19.part2 input
solveDay "day20" "1" input = Day20.part1 input
solveDay "day20" "2" input = Day20.part2 input
solveDay "day21" "1" input = Day21.part1 input
solveDay "day21" "2" input = Day21.part2 input
solveDay "day22" "1" input = Day22.part1 input
solveDay "day22" "2" input = Day22.part2 input
solveDay "day23" "1" input = Day23.part1 input
solveDay "day23" "2" input = Day23.part2 input
solveDay "day24" "1" input = Day24.part1 input
solveDay "day24" "2" input = Day24.part2 input
solveDay "day25" "1" input = Day25.part1 input
solveDay "day25" "2" input = Day25.part2 input
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
