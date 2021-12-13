#!/usr/bin/env -S deno run --quiet --allow-read=./ --log-level info
/**
 * @file aoc_day11_p1.ts
 * @brief Advent of Code (AOC) 2021 Puzzle solution for:  Day 9 Part 1.
 *
 * @author simon rowe <simon@wiremoons.com>
 * @license open-source released under "MIT License"
 *
 * @date originally created: 11 Dec 2021
 *
 * @details Advent of Code (AOC) 2021 Puzzle solution. See: https://adventofcode.com/2021/
 *
 * @note The program can be run with Deno using the command:
 * @code deno run --quiet --allow-read --allow-write --allow-env --allow-run ./aoc_day11_p1.ts
 */

//--------------------------------
// MODULE IMPORTS
//--------------------------------
// import modules to support program:
import {existsFile} from "https://deno.land/x/deno_mod@0.7.4/mod.ts";

//--------------------------------
// GLOBAL PATHS and VARIABLES
//--------------------------------
// define the base location for Deno application directories:
const aocDay = "11"
const aocPart = "01"
// Puzzle data:
//const inputFile = `./day${aocDay}-input.txt`;
// Puzzle data 'TEST' input only:
const inputFile = `./day${aocDay}-TEST-input.txt`;


//--------------------------------
// FUNCTIONS
//--------------------------------



//--------------------------------
// MAIN
//--------------------------------
if (import.meta.main) {

    // Display startup message
    console.log(`\nAdvent Of Code 2021 :  Day ${aocDay} Part ${aocPart}\n`)

    // Ensure the 'puzzle input' file can be found and loaded
    if (!await existsFile(inputFile)) {
        console.error(`\nERROR: failed to find the puzzle input file: '${inputFile}'. Exit.\n`);
        Deno.exit(1);
    }

    // Read in the puzzle data
    const puzzleData = await Deno.readTextFile(inputFile);
    // read puzzle data into lines of text
    const lines = puzzleData.split("\n");
    // convert lines into an array per line
    const lineArray = Array.from(lines);
    // split array of lines into individual numbers in their own array
    const octiMap = lineArray.map((line) => line.split("").map(Number));

    // display ociMap of all octopus values
    console.table(octiMap);

}