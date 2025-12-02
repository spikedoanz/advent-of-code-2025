# Advent of Code 2025 in Idris2!

## Usage

Quick and dirty
```
# runs day 1 part 1
./run.sh 1 1
```

Which is just
```
pack exec src/Main.idr day1 1
```

With "proper build"
```
pack cleanbuild advent-of-code-2025 ; pack run advent-of-code-2025 day1 1   # run day 1 part 1
pack cleanbuild advent-of-code-2025 ; pack run advent-of-code-2025 day1 2   # run day 1 part 2
```

Note: to run yourself, please source your own input files from adventofcode.com and chuck them
into the inputs/ directory

## Adding a new day

1. Create `src/DayN.idr` with `part1` and `part2` functions
2. Add cases to `solveDay` in `src/Main.idr`
3. Add input to `inputs/dayN.txt`
