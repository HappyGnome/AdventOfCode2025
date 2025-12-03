# Advent of Code 2025 solutions

Here are the attempts of [HappyGnome](https://github.com/HappyGnome) to solve the puzzles for [Advent of Code](https://adventofcode.com/2025) 2025 by Eric Wastl.

Language used: Haskell

# Commentary on solutions

In the pages linked below are some brief discussions of the key points of the problems and mistakes I made solving them. Comments will be added about problems if/when time permits, and if I think of something worth mentioning.
* [Day 1](docs/day1.md) - going in circles
* [Day 2](docs/day2.md) - a repeating problem
* [Day 3](docs/day3.md) - unwise choices



# Project structure
* `NextPuzzle.bat/NextPuzzle.ps1` Used to update the `cabal` and `Main.hs` files each day (run the `.bat`)
* `AdventOfCode2025.cabal` is the project cabal file - specifying lib dependencies, project modules and options 
* `app/Main.hs` is the entry point for the app (simply runs `exec` in today's challenge module)
* `app/Common` contains some implementations of common algorithms and helper methods the author has built up when working on similar problems
* `app/Puzzles/Input` contains a folder per challenge attempted, each containing input and test inputs
* `app/Puzzles/Parts` contains one Haskell file (module) per puzzle

# Copyright
Unless otherwise stated, the content of this repo is Copyright (2025) of HappyGnome.

HappyGnome does not assert copyright on the puzzle inputs that are included as components of the respective solutions. Advent of Code may assert copyright on the use of these inputs in other cases. 
