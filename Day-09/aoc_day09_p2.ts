#!/usr/bin/env -S deno run --quiet --allow-read=./ --log-level info
/**
 * @file aoc_day09_p2.ts
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
 * @code deno run --quiet --allow-read --allow-write --allow-env --allow-run ./aoc_day09_p2.ts
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
const aocPart = "02"
// Puzzle data:
//const inputFile = `./day${aocDay}-input.txt`;
// Puzzle data 'TEST' input only:
const inputFile = `./day${aocDay}-TEST-input.txt`;

// used to track checked cave point locations
type CavePosition = {
    point: number;
    col: number;
    row: number;
    checked : number;
}

// const ?? - but is altered in rest of code. Or OK as only applies to creation not growing/shrinking?
const lowPoints: Array<CavePosition> = [];

// input data array of cave 'row' and 'col' in a 2d array (matrix)
let caveMap:number[][] = [];

//--------------------------------
// FUNCTIONS
//--------------------------------

// Part 1 : Obtain all cave low points and their risk value
function getLowPoints(caveMap:number[][], lowPoints:CavePosition[] ) {

    let lowCount = 0;

    (caveMap.map((row, row_index) => {

        //console.debug(`\n\nNEW ROW: item: ${row_index} : ${row}`)

        row.forEach((rowValue, col_index) => {

            //console.debug(`\nCol: ${col_index} Row: ${row_index} = Value ${rowValue}`);

            // track checking of different numbers
            const checks = [NaN, NaN, NaN, NaN, NaN];

            // check value in column above
            if (row_index > 0) {
                const valAbove = caveMap[row_index - 1][col_index]
                //console.debug(`Column ABOVE: ${valAbove}`);
                rowValue < valAbove ? checks[0] = 1 : checks[0] = 0;
                //console.debug(`${rowValue} < ${valAbove} is ${checks}`);
            }
            // check value in column below
            if (caveMap.length - 1 > row_index) {
                const valBelow = caveMap[row_index + 1][col_index]
                //console.debug(`Column BELOW: ${valBelow}`);
                rowValue < valBelow ? checks[1] = 1 : checks[1] = 0;
                //console.debug(`${rowValue} < ${valBelow} is ${checks}`);
            }
            // check value in row_index position after
            if (col_index < row.length - 1) {
                const valAfter = caveMap[row_index][col_index + 1];
                //console.debug(`Element AFTER: ${valAfter}`);
                rowValue < valAfter ? checks[2] = 1 : checks[2] = 0;
                //console.debug(`${rowValue} < ${valAfter} is ${checks}`);
            }
            // check value in row_index position before
            if (col_index > 0) {
                const valBefore = caveMap[row_index][col_index - 1];
                //console.debug(`Element BEFORE: ${valBefore}`);
                rowValue < valBefore ? checks[3] = 1 : checks[3] = 0;
                //console.debug(`${rowValue} < ${valBefore} is ${checks}`);
            }
            // check if current value is zero
            if (rowValue === 0) {
                checks[4] = 4;  // if '0' then automatically lower!
                //console.debug(`ELEMENT: ${rowValue} === '0' is ${checks}`);
            }

            // remove any remaining 'NaN' and sum up checks
            const checksSum = checks.filter(x => !isNaN(x)).reduce((a, b) => a + b, 0);

            //console.debug(`Check Sum : ${checksSum} and Check: ${checks.filter(x => !isNaN(x)).length}`);
            if (checksSum >= checks.filter(x => !isNaN(x)).length) {
                //console.log(`Low point ${rowValue} at position row: ${row_index} and col: ${col_index}`);

                // gather the lowPoint array data needed
                // const lowPointsCollection: CavePosition = {
                //     point: rowValue,
                //     col: row_index,
                //     row: col_index,
                //     checked: 0
                // };
                // lowPoints.push(lowPointsCollection);
                // NB : above commented lines refactored to:
                lowPoints.push({ "point": rowValue, "col": row_index, "row": col_index, "checked": 0});
                //console.log(lowPoints);

                lowCount = lowCount + 1;
            }
            //console.debug(`Current risk count: ${riskCount}`);
        })
    }));
}

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
    caveMap = lineArray.map((line) => line.split("").map(Number));

    console.log(`Checking '${caveMap.length}' rows of puzzle input data...`);

    // get all low points and their risk count for part one
    getLowPoints(caveMap,lowPoints);

    // display found cave low points and the col & row position
    lowPoints.map((values, idx) => {
        console.log(`Mapping low point '${idx + 1} of ${lowPoints.length}' : Value '${values.point}' at col '${values.col}' & row '${values.row}'`);
    });

    console.log(` » Part 2: total basin low points found to map: ${lowPoints.length}`);

    console.log("\n")
}