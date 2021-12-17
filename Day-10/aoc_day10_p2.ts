#!/usr/bin/env -S deno run --quiet --allow-read=./
/**
 * @file aoc_day10_p2.ts
 * @brief Advent of Code (AOC) 2021 Puzzle solution for:  Day 10 Part 2.
 *
 * @author simon rowe <simon@wiremoons.com>
 * @license open-source released under "MIT License"
 *
 * @date originally created: 17 Dec 2021
 *
 * @details Advent of Code (AOC) 2021 Puzzle solution. See: https://adventofcode.com/2021/
 *
 * @note The program can be run with Deno using the command:
 * @code deno run --quiet --allow-read ./aoc_day10_p2.ts
 */

//--------------------------------
// MODULE IMPORTS
//--------------------------------
// import modules to support program:
import {existsFile, isString} from "https://deno.land/x/deno_mod@0.7.4/mod.ts";

//--------------------------------
// MAIN
//--------------------------------
if (import.meta.main) {
    //********************************
    // APPLICATION CONFIGURATION
    //********************************
    // define the base location for Deno application directories:
    const aocDay = "10";
    const aocPart = "02";

    // SET INPUT PUZZLE DATA FILE NAME:
    // Puzzle data:
    const inputFile = `./day${aocDay}-input.txt`;
    // Puzzle data 'TEST':
    //const inputFile = `./day${aocDay}-TEST-input.txt`;
    //********************************

    // Display startup message
    console.log(`\nAdvent Of Code 2021 :  Day ${aocDay} Part ${aocPart}\n`);

    // Ensure the 'puzzle input' file can be found and loaded

    if (!await existsFile(inputFile)) {
        console.error(`\nERROR: failed to find the puzzle input file: '${inputFile}'. Exit.\n`);
        Deno.exit(1);
    }
    /**/
    // Read in the puzzle data
    const puzzleData = await Deno.readTextFile(inputFile);
    // read puzzle data into lines of text
    const lines = puzzleData.split("\n");
    // convert lines into an array per line
    const lineArray = Array.from(lines);
    console.log(` » Checking total of '${lineArray.length}' chunks for any corruption...`);

    // for each line of puzzle input get the incomplete chuck lines only as an array of opening chucks
    const completionArray = lineArray.map((line) => getIncomplete(line)).filter(Boolean);
    console.log(` » Found '${completionArray.length}' chunks requiring completion.`);
    //console.debug(completionArray);

    // calculate completion score for Part 2
    const completionReversed = completionArray.map(incompleteChunk => incompleteChunk!.reverse()
        .reduce((score, c) => score * 5 + getCompletionAmount(c), 0))
        .sort((a, b) => a - b);
    //console.debug(completionReversed);
    const middleScore = completionReversed[Math.floor(completionReversed.length / 2)];

    console.log(` » PART 2: chunk completion score is: '${middleScore}'`);

    console.log("\nDONE");


}

//--------------------------------
// FUNCTIONS
//--------------------------------

/**
 * Find any corrupt chuck in each line of puzzle data input.
 * @param chunkLine string contain one line from the puzzle file.
 * @returns the corrupted chunk found as one of ') ] } >' or undefined
 */
function getIncomplete(chunkLine: string): string[] | undefined {

    const chunks: Record<string, string> = {
        "(": ")",
        "[": "]",
        "{": "}",
        "<": ">",
    };

    const stack: string[] = [];

    // Re-use code from Part 1 to remove corrupted lines
    for (const char of chunkLine.split("")) {
        if (char in chunks) {
            stack.push(char);
        } else if (char !== chunks[stack.pop()!]) {
            //console.debug("FOUND chuck syntax error\n");
            return undefined;
        }
    }

    // line processed is not corrupt - so return the 'stack' array for 'Part 2' processing
    //console.debug("No chuck syntax error found!\n");
    return stack;
}

/**
 * Find the score value for a completed chuck.
 * @param completionChar string contain one of '( [ { >'
 * @returns the score value for the corrupted chunk or 0 if not found
 */
function getCompletionAmount(completionChar:string | undefined):number {

    const scoring: Record<string, number> = {
        "(": 1,
        "[": 2,
        "{": 3,
        "<": 4
    };

    if (! (isString(completionChar))) return 0;
    if (! (completionChar in scoring)) return 0;

    return scoring[completionChar]!;
}
