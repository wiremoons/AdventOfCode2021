# Advent of Code 2021

See the full AoC 2021 **Day 1** puzzle at the URL: https://adventofcode.com/2021/day/1

My personal objective was to solve the puzzle using the [Fortran language](https://fortran-lang.org).

I dont have much experience with *Fortran**, so the source code is more verbose than a simple 
approach focusing on just solving the puzzel quickly. 

I wanted to learn the use of *functions*, *subroutines*, files handling, arrays, and many 
other new concepts. The resulting programs are the longest and most complex Fortran programs 
I have written to date. 

You have to start somewhere - and [AoC Puzzles](https://adventofcode.com/) provide a good 
purpose to try a language out!

## Part ONE:

- Use input file: **day01-input.txt**

**Task**

Count the number of times a depth measurement increases from the previous measurement. 
(There is no measurement before the first measurement.) In the example given, the 
changes are as follows:

```
199 (N/A - no previous measurement)
200 (increased)
208 (increased)
210 (increased)
200 (decreased)
207 (increased)
240 (increased)
269 (increased)
260 (decreased)
263 (increased)
```
In this example, there are 7 measurements that are larger than the previous measurement.

**Anser needed** : How many measurements are larger than the previous measurement?

**Build the Fortran Source Code**
```console
gfortran -static-libgcc -o aoc_day01_p1 aoc_day01_p1.f90
```

## Part TWO:

- Use input file: **day01-input.txt**

**Task**

Consider sums of a three-measurement sliding window. Again considering the example:

```
199  A      
200  A B    
208  A B C  
210    B C D
200  E   C D
207  E F   D
240  E F G  
269    F G H
260      G H
263        H
```
Start by comparing the first and second three-measurement windows. The measurements in the 
first window are marked A (199, 200, 208); their sum is 199 + 200 + 208 = 607. The second 
window is marked B (200, 208, 210); its sum is 618. The sum of measurements in the second 
window is larger than the sum of the first, so this first comparison increased. count the 
number of times the sum of measurements in this sliding window increases from the previous 
sum. So, compare A with B, then compare B with C, then C with D, and so on. Stop when 
there aren't enough measurements left to create a new three-measurement sum.

**Anser needed** : How many measurements are larger than the previous measurement in 
this sliding window increase?

**Build the Fortran Source Code**
```console
gfortran -static-libgcc -o aoc_day01_p2 aoc_day01_p2.f90
```
