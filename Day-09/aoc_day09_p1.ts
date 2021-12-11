#!/usr/bin/env -S deno run --quiet --allow-read=./ --log-level info
/**
 * @file aoc_day09_p1.ts
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
 * @code deno run --quiet --allow-read --allow-write --allow-env --allow-run ./aoc_day09_p1.ts
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
const aocDay = "09"
const aocPart = "01"
// Puzzle data:
const inputFile = `./day${aocDay}-input.txt`;
// Puzzle data 'TEST' input only:
//const inputFile = `./day${aocDay}-TEST-input.txt`;

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

    // Read in the puzzle data and find correct oxygen and CO2 measurements to multiply for answer!
    const puzzleData = await Deno.readTextFile(inputFile);
    // read data into lines of text
    const lines = puzzleData.split("\n");
    // convert lines into an array per line
    const lineArray = Array.from(lines);
    // split array of lines into individual numbers in their own array
    const caveMap = lineArray.map((line) => line.split("").map(Number));

    // puzzle answer
    let riskCount = 0;

    console.log(`Checking '${caveMap.length}' rows of puzzle input data...\n`);

    (caveMap.map((item,idx_col) =>{

        //console.debug(`\n\nNEW ROW: item: ${idx_col} : ${item}`)

        item.forEach((element, idx) => {

            //console.debug(`\nIdx_Col: ${idx_col} Idx: ${idx} = Value ${element}`);

            // track checking of different numbers
            const checks = [NaN,NaN,NaN,NaN,NaN];

            // check value in column above
            if (idx_col > 0) {
                const valAbove = caveMap[idx_col - 1][idx]
                //console.debug(`Column ABOVE: ${valAbove}`);
                element < valAbove  ? checks[0] = 1 : checks[0] = 0;
                //console.debug(`${element} < ${valAbove} is ${checks}`);
            }
            // check value in column below
            if (caveMap.length - 1 > idx_col) {
                const valBelow = caveMap[idx_col + 1][idx]
                //console.debug(`Column BELOW: ${valBelow}`);
                element  < valBelow ? checks[1] = 1 : checks[1] = 0;
                //console.debug(`${element} < ${valBelow} is ${checks}`);
            }
            // check value in row position after
            if (idx < item.length -1) {
                const valAfter = caveMap[idx_col][idx + 1];
                //console.debug(`Element AFTER: ${valAfter}`);
                element < valAfter ? checks[2] = 1 : checks[2] = 0;
                //console.debug(`${element} < ${valAfter} is ${checks}`);
            }
            // check value in row position before
            if (idx > 0) {
                const valBefore = caveMap[idx_col][idx - 1];
                //console.debug(`Element BEFORE: ${valBefore}`);
                element < valBefore ? checks[3] = 1 : checks[3] = 0;
                //console.debug(`${element} < ${valBefore} is ${checks}`);
            }
            // check if current value is zero
            if (element === 0) {
                checks[4] = 4;  // if '0' then automatically lower!
                //console.debug(`ELEMENT: ${element} === '0' is ${checks}`);
            }

            // remove any remaining 'NaN' and sum up checks
            const checksSum = checks.filter(x => !isNaN(x)).reduce((a, b) => a + b, 0);

            //console.debug(`Check Sum : ${checksSum} and Check: ${checks.filter(x => !isNaN(x)).length}`);
            if (checksSum >= checks.filter(x => !isNaN(x)).length ) riskCount = riskCount + (element + 1);
            //console.debug(`Current risk count: ${riskCount}`);

        })
    }));

    console.log(` » Part 1: Risk levels of height map low points: ${riskCount}\n`);

}