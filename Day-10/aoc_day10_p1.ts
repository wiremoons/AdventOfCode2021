#!/usr/bin/env -S deno run --quiet --allow-read=./
/**
 * @file aoc_day10_p1.ts
 * @brief Advent of Code (AOC) 2021 Puzzle solution for:  Day 10 Part 1.
 *
 * @author simon rowe <simon@wiremoons.com>
 * @license open-source released under "MIT License"
 *
 * @date originally created: 17 Dec 2021
 *
 * @details Advent of Code (AOC) 2021 Puzzle solution. See: https://adventofcode.com/2021/
 *
 * @note The program can be run with Deno using the command:
 * @code deno run --quiet --allow-read ./aoc_day10_p1.ts
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
    const aocPart = "01";

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

    // for each line of puzzle input get the corrupted chucks the line includes
    const corruptCount = lineArray.map((line) => getCorrupted(line)).filter(Boolean);

    // get the total syntax error score for the corrupted chucks found
    const syntaxScores = (corruptCount.map((illegalChar) => getSyntaxErrorAmount(illegalChar)));
    //console.debug(`Scores per syntax error: ${syntaxScores}`);
    console.log(` » Found '${syntaxScores.length}' chunks with corruption.`);
    const totalScore = syntaxScores.reduce((a, b) => a + b, 0);

    //console.debug(`Corrupt chucks are: '${corruptCount}'`);
    console.log(` » PART 1: Syntax error score is: '${totalScore}'`);

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
function getCorrupted(chunkLine: string): string | undefined {

    const chunks: Record<string, string> = {
        "(": ")",
        "[": "]",
        "{": "}",
        "<": ">",
    };

    const stack: string[] = [];

    // EXPLANATION:
    // Add all opening chunk characters to an array called 'stack'.
    // When a closing chuck character is found in the line of chunks
    // input - check it against the 'stack' arrays last entry. As this
    // is the first closing character it must be closing the last one
    // opened. If not, then the chunk is starting to be closed by the
    // wrong chunk character. In other words it is adding a closing
    // character to the chunk before it has first added the corresponding
    // opening one. Therefore, the chunk just became corrupted.
    // With hindsight, it would be quicker just to compare the first closing
    // character found the last one, and if they are not a matching pair
    // then corruption point has been found - saves keeping a stack array.

    for (const char of chunkLine.split("")) {

        if (char in chunks) {
            stack.push(char);
            //console.debug(`Stack push is: ${char}`);
            //console.debug(`Full stack is: ${stack}`);
        } else if (char !== chunks[stack.pop()!]) {
            //console.debug(`Found unmatched: '${char}'`)
            //console.debug(`Full stack is: ${stack}`);
            //console.debug("FOUND chuck syntax error\n");
            return char;
        }
    }

    //console.debug("No chuck syntax error found!\n");

}

/**
 * Find the score value for a corrupt chuck.
 * @param illegalChar string contain one of ') ] } >'
 * @returns the score value for the corrupted chunk or 0 if not found
 */
function getSyntaxErrorAmount(illegalChar:string | undefined):number {

    const scoring: Record<string, number> = {
        ")": 3,
        "]": 57,
        "}": 1197,
        ">": 25137
    };

    if (! (isString(illegalChar))) return 0;
    if (! (illegalChar in scoring)) return 0;

    return scoring[illegalChar]!;
}
