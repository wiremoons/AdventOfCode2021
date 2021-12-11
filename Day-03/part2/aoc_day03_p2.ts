#!/usr/bin/env -S deno run --quiet --allow-read=./
/**
 * @file aoc_day03_p2.ts
 * @brief Advent of Code (AOC) 2021 Puzzle solution for:  Day 3 Part 2.
 *
 * @author simon rowe <simon@wiremoons.com>
 * @license open-source released under "MIT License"
 *
 * @date originally created: 04 Dec 2021
 *
 * @details Advent of Code (AOC) 2021 Puzzle solution. See: https://adventofcode.com/2021/
 *
 * @note The program can be run with Deno using the command:
 * @code deno run --quiet --allow-read --allow-write --allow-env --allow-run ./aoc_day03_p2.ts
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
const aocDay = "03"
const aocPart = "02"
const inputFile = `./day${aocDay}-input.txt`;

//--------------------------------
// FUNCTIONS
//--------------------------------

function checkCorrect(measurements: number[][], typeNeeded: (totalBits: number[]) => number) {
    let X = 0;
    while (measurements.length > 1) {
        const totalBits = [0, 0];
        for (let i = 0; i < measurements.length; i++) {
            const bit = measurements[i][X];
            totalBits[bit]++;
        }
        const bit = +typeNeeded(totalBits);
        measurements = measurements.filter((bits) => bits[X] === bit);
        X++;
    }
    return convertBits2Decimal(measurements[0]);
}

function convertBits2Decimal(bits: number[]): number {
    let decimal = 0;
    //console.log(bits)
    for (const bit of bits) {
        decimal = decimal << 1 | bit;
    }
    //console.log(decimal)
    return decimal;
}

function getOnes([zeroBits, oneBits]: number[]): number {
    return +(oneBits >= zeroBits);
}

function getZeros([zeroBits, oneBits]: number[]): number {
    return +(zeroBits > oneBits);
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
    const lines = puzzleData.split("\n");
    const binArray = Array.from(lines);
    const measurements = binArray.map((line) => line.split("").map(Number));
    const oxygen = checkCorrect(measurements, getOnes);
    const co2 = checkCorrect(measurements, getZeros);
    console.log(` » selected Oxygen is: ${oxygen}`)
    console.log(` » selected CO2 is: ${co2}\n`)
    console.log(`Solution answer is: ${oxygen * co2}\n`);
}
