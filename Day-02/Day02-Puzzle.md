# Advent of Code 2021

Once you have solved **Day 1** - see the full AoC 2021 **Day 2** puzzle at the URL: https://adventofcode.com/2021/day/2

For more background information please see: [Day One - solved using Fortran](../Day-01/Day01-Puzzle.md)

## Part ONE:

- Use input file: [day02-input.txt](./day02-input.txt)
- Fortran source code: [aoc_day02_p1.f90](./aoc_day02_p1.f90)

**Task**

Follow the manoeuvring instructions to figure out where the submarine is going. For example:

```
forward 5
down 5
forward 8
up 3
down 8
forward 2
```

Your horizontal position and depth both start at 0. The steps above would then modify 
them as follows:

- forward 5 adds 5 to your horizontal position, a total of 5.
- down 5 adds 5 to your depth, resulting in a value of 5.
- forward 8 adds 8 to your horizontal position, a total of 13.
- up 3 decreases your depth by 3, resulting in a value of 2.
- down 8 adds 8 to your depth, resulting in a value of 10.
- forward 2 adds 2 to your horizontal position, a total of 15.

After following these instructions, you would have a horizontal position of 15 and a 
depth of 10. (Multiplying these together produces 150.)

Using the provided puzzle input - calculate the horizontal position and depth you would 
have after following the planned course.

**Anser needed** : What do you get if you multiply your final horizontal position by 
your final depth?

**Build the Fortran Source Code**
```console
gfortran -static-libgcc -o aoc_day02_p1 aoc_day02_p1.f90
```

## Part TWO:

- Use input file: [day02-input.txt](./day01-input.txt)
- Fortran source code: [aoc_day02_p2.f90](./aoc_day02_p2.f90)

**Task**

The movement commands mean something entirely different than you first thought:

- down X increases your aim by X units.
- up X decreases your aim by X units.
- forward X does two things:
  - It increases your horizontal position by X units.
  - It increases your depth by your aim multiplied by X.

Again note that since you're on a submarine, down and up do the opposite of what you 
might expect: "down" means aiming in the positive direction.

Now, the above example does something different:

- forward 5 adds 5 to your horizontal position, a total of 5. Because your aim is 0, your depth does not change.
down 5 adds 5 to your aim, resulting in a value of 5.
- forward 8 adds 8 to your horizontal position, a total of 13. Because your aim is 5, your depth increases by 8*5=40.
- up 3 decreases your aim by 3, resulting in a value of 2.
- down 8 adds 8 to your aim, resulting in a value of 10.
- forward 2 adds 2 to your horizontal position, a total of 15. Because your aim is 10, your depth increases by 2*10=20 to a total of 60.

After following these new instructions, you would have a horizontal position of 15 and a 
depth of 60. (Multiplying these produces 900.)

Using this new interpretation of the commands, calculate the horizontal position and 
depth you would have after following the planned course.


**Anser needed** : What do you get if you multiply your final horizontal position by 
your re-calculated final depth?

**Build the Fortran Source Code**
```console
gfortran -static-libgcc -o aoc_day02_p2 aoc_day02_p2.f90
```

[Return to the main repo page](../README.md)