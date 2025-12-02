# Advent of Code 2025 in Idris2!

## Usage

```
pack run advent-of-code-2025 day1 1   # run day 1 part 1
pack run advent-of-code-2025 day1 2   # run day 1 part 2
```

## REPL

```
pack repl advent-of-code-2025
:module Day1
part1 "test input"
```

## Adding a new day

1. Create `src/DayN.idr` with `part1` and `part2` functions
2. Add cases to `solveDay` in `src/Main.idr`
3. Add input to `inputs/dayN.txt`
