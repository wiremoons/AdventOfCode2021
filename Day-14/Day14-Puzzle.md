# Advent of Code 2021

See the full AoC 2021 **Day 11** puzzle at the URL: https://adventofcode.com/2021/day/8

## Part ONE:

- Use input file: [day11-input.txt](./day11-input.txt)
- TypeScript (Deno) source code: [aoc_day11_p1.ts](./aoc_day11_p1.ts)

**Task**

You enter a large cavern full of rare bioluminescent dumbo octopuses! They seem to not like the Christmas lights on your
submarine, so you turn them off for now.

There are 100 octopuses arranged neatly in a 10 by 10 grid. Each octopus slowly gains energy over time and flashes
brightly for a moment when its energy is full. Although your lights are off, maybe you could navigate through the cave
without disturbing the octopuses if you could predict when the flashes of light will happen.

Each octopus has an energy level - your submarine can remotely measure the energy level of each octopus (your puzzle
input). For example:

```
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
```

The energy level of each octopus is a value between 0 and 9. Here, the top-left octopus has an energy level of 5, the
bottom-right one has an energy level of 6, and so on.

You can model the energy levels and flashes of light in steps. During a single step, the following occurs:

First, the energy level of each octopus increases by 1. Then, any octopus with an energy level greater than 9 flashes.
This increases the energy level of all adjacent octopuses by 1, including octopuses that are diagonally adjacent. If
this causes an octopus to have an energy level greater than 9, it also flashes. This process continues as long as new
octopuses keep having their energy level increased beyond 9. (An octopus can only flash at most once per step.) Finally,
any octopus that flashed during this step has its energy level set to 0, as it used all of its energy to flash. Adjacent
flashes can cause an octopus to flash on a step even if it begins that step with very little energy.

After 100 steps, there have been a total of 1656 flashes.

Given the starting energy levels of the dumbo octopuses in your cavern, simulate 100 steps.

**Answer needed** : How many total flashes are there after 100 steps?

**Run the TypeScript (Deno) Source Code**

Make sure [Deno](https://deno.land/) is installed and in your path, then on Linux or macOS:

```console
chmod 755 aoc_day11_p1.ts
./aoc_day11_p1.ts
```

## Part TWO:

- Use input file: [day11-input.txt](./day11-input.txt)
- TypeScript (Deno) source code: [aoc_day11_p2.ts](./aoc_day11_p2.ts)

**Task**

**Answer needed** :

**Run the TypeScript (Deno) Source Code**

Make sure [Deno](https://deno.land/) is installed and in your path, then on Linux or macOS:

```console
chmod 755 aoc_day11_p2.ts
./aoc_day11_p2.ts
```

[Return to the main repo page](../README.md)
