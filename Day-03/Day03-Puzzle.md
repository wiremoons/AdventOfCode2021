# Advent of Code 2021

See the full AoC 2021 **Day 3** puzzle at the URL: https://adventofcode.com/2021/day/3

This one was quite hard, but I certainly learnt a lot about [Fortran](https://fortran-lang.org)!

Most notably I found the Fortran language '*column major*' tricky as I had not come across 
it before, as all prior languages I have worked with use '*row major*'. It certainly soaked 
some time up figuring out that!

I used Fortran to create the needed outputs for part 1, but did then use an on-line 
*binary to decimal* convertor to get the final answer.

I then needed a break from Fortran - so completed Part 2 in TypeScript to give myself a break!

Day 3 was a tricky one anyway - adding in my learning curve for Fortran, I was a long one...!


## Part ONE:

- Use input file: [day03-input.txt](./part1/day03-input.txt)
- Fortran source code: [aoc_day03_p1.f90](./part1/aoc_day03_p1.f90)

**Task**

The diagnostic report (your puzzle input) consists of a list of binary numbers which, 
when decoded properly, can tell you many useful things about the conditions of the submarine. 
The first parameter to check is the power consumption.

You need to use the binary numbers in the diagnostic report to generate two new binary 
numbers (called the gamma rate and the epsilon rate). The power consumption can then be 
found by multiplying the gamma rate by the epsilon rate.

Each bit in the gamma rate can be determined by finding the most common bit in the 
corresponding position of all numbers in the diagnostic report. For example, given the 
following diagnostic report:

```
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
```

Considering only the first bit of each number, there are five 0 bits and seven 1 bits. Since the 
most common bit is 1, the first bit of the gamma rate is 1.

The most common second bit of the numbers in the diagnostic report is 0, so the second bit 
of the gamma rate is 0.

The most common value of the third, fourth, and fifth bits are 1, 1, and 0, respectively, and 
so the final three bits of the gamma rate are 110.

So, the gamma rate is the binary number 10110, or 22 in decimal.

The epsilon rate is calculated in a similar way; rather than use the most common bit, the 
least common bit from each position is used. So, the epsilon rate is 01001, or 9 in decimal. 
Multiplying the gamma rate (22) by the epsilon rate (9) produces the power consumption, 198.

Use the binary numbers in your diagnostic report to calculate the gamma rate and epsilon rate, then multiply them together. 
What is the power consumption of the submarine? 

**Be sure to represent your answer in decimal, not binary.**

**Answer needed** : What is the power consumption of the submarine?


**Build the Fortran Source Code**
```console
gfortran -static-libgcc -o aoc_day03_p1 aoc_day03_p1.f90
```

## Part TWO:

- Use input file: [day03-input.txt](./part2/day03-input.txt)
- TypeScript source code for [Deno](https://deno.land/) runtime: [aoc_day03_p2.ts](./part2/aoc_day03_p2.ts)

**Task**

**Build the Fortran Source Code**
Make sure [Deno](https://deno.land/) is installed and in your path, then on Linux or macOS:
```console
chmod 755 aoc_day03_p2.ts
./aoc_day03_p2.ts
```

[Return to the main repo page](../README.md)