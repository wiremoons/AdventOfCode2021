# Advent of Code 2021

See the full AoC 2021 **Day 4** puzzle at the URL: https://adventofcode.com/2021/day/4

## Part ONE:

- Use input file: [day04-input.txt](./day04-input.txt)
- Fortran source code: [aoc_day04_p1.f90](./aoc_day04_p1.f90)

**Task**

he submarine has a bingo subsystem to help passengers (currently, you and 
the giant squid) pass the time. It automatically generates a random order 
in which to draw numbers and a random set of boards (your puzzle input). 

For example:
```
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
```

The score of the winning board can now be calculated. Start by finding the sum 
of all unmarked numbers on that board; in this case, the sum is 188. Then, multiply 
that sum by the number that was just called when the board won, 24, to get the 
final score, 188 * 24 = 4512.

To guarantee victory against the giant squid, figure out which board will win first. 

**Anser needed** : What will your final score be if you choose that board?

**Build the Fortran Source Code**
```console
gfortran -static-libgcc -o aoc_day04_p1 aoc_day04_p1.f90
```

## Part TWO:

- Use input file: [day04-input.txt](./day04-input.txt)
- Fortran source code: [aoc_day04_p2.f90](./aoc_day04_p2.f90)

**Task**

On the other hand, it might be wise to try a different strategy: *let the giant squid win**.

You aren't sure how many bingo boards a giant squid could play at once, so rather than waste 
time counting its arms, the safe thing to do is to **figure out which board will win last** and 
choose that one. That way, no matter which boards it picks, it will win for sure.

In the above example, the second board is the last to win, which happens after 13 is 
eventually called and its middle column is completely marked. If you were to 
keep playing until this point, the second board would have a sum of unmarked numbers 
equal to 148 for a final score of 148 * 13 = 1924.

**Anser needed** : Figure out which board will win last. Once it wins, what would its final score be?

**Build the Fortran Source Code**
```console
gfortran -static-libgcc -o aoc_day04_p2 aoc_day04_p2.f90
```

[Return to the main repo page](../README.md)