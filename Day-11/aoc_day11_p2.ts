#!/usr/bin/env -S deno run --quiet --allow-read=./
/**
 * @file aoc_day11_p2.ts
 * @brief Advent of Code (AOC) 2021 Puzzle solution for:  Day 11 Part 2.
 *
 * @author simon rowe <simon@wiremoons.com>
 * @license open-source released under "MIT License"
 *
 * @date originally created: 19 Dec 2021
 *
 * @details Advent of Code (AOC) 2021 Puzzle solution. See: https://adventofcode.com/2021/
 *
 * @note The program can be run with Deno using the command:
 * @code deno run --quiet --allow-read ./aoc_day11_p2.ts
 */

//--------------------------------
// MODULE IMPORTS
//--------------------------------
// import modules to support program:
import { existsFile } from "https://deno.land/x/deno_mod@0.7.4/mod.ts";

//--------------------------------
// MAIN
//--------------------------------
if (import.meta.main) {
  //********************************
  // APPLICATION CONFIGURATION
  //********************************
  // define the base location for Deno application directories:
  const aocDay = "11";
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

  // split array of lines into individual numbers in their own array
  let octoMap = lineArray.map((line) => line.split("").map(Number));

  // set up tracking counters
  let count = 1;
  let flashTotal = 0;
  let stillFlashes = true;

  // changed below for Part 2 - keep looping until all have flashed at the same time
  while (!checkAllFlashed(octoMap)) {
    octoMap = incrementOctopus(octoMap);
    //console.debug("-->  NEW INCREMENTED MAP  <--");
    //console.table(octoMap);
    //console.debug("\n");

    // check for any flash candidates
    stillFlashes = octoFlashesLeft(octoMap);

    // keep processing octopus array 'octomap' until all flashes have been handled
    while (stillFlashes) {
      octoMap = upDateRowColNeighbours(octoMap);
      stillFlashes = octoFlashesLeft(octoMap);
    }

    //console.debug("\nDONE STATE:");
    //console.table(octoMap);

    // get the current flash count for this loop and add to the runing total
    flashTotal = flashTotal + getTotalFlashes(octoMap);

    //console.debug(`Current flash count: ${flashTotal}`);
    //console.debug(` --> LOOP COUNT COMPLETED: ${count}\n\n`);
    count++;
  } // end main while

  console.log(` » Total loop count: ${count - 1}`);
  console.log(` » Final flash count: ${flashTotal}\n`);
  console.log("DONE");
}

//--------------------------------
// FUNCTIONS
//--------------------------------

/**
 * Add one ot all octopus flash counts at the start of every cycle
 * @param octoMap array of the octopus flash counts
 * @return octoMap array updateed with each octopus incremented by one
 */
function incrementOctopus(octoMap: number[][]): number[][] {
  return octoMap.map((value) => {
    return value.map((cv) => {
      return cv + 1;
    }).map(Number);
  });
}

/**
 * Check if any octopus in the array is ready to flash still
 * @param octoMap array of the octopus flash counts
 * @return boolean indication of is any flash candidates exist still
 */
function octoFlashesLeft(octoMap: number[][]): boolean {
  let result = false;
  octoMap.map((rv) => {
    rv.map((cv) => {
      if (cv > 9) result = true;
    });
  });
  return result;
}

/**
 * Process each octopus element and if it has flashed set to zero and then increment its neighbours
 * @param octoMap array of the octopus flash counts
 * @return new octoMap array updated with the octopus flash counts including neighbours
 */
function upDateRowColNeighbours(octoMap: number[][]): number[][] {
  // get the new map to update
  const newOctoMap = octoMap;
  //process each Octopus entry for any neighbour updates needed
  octoMap.map((rv, row) => {
    //console.debug(`Processing row values ${rv} at colIdx: ${row}`);
    rv.map((_cv, col) => {
      //console.debug(`--> Start update neighbour check: value = ${newOctoMap[row][col]}. Index: ${row},${col}`);
      if (newOctoMap[row][col] > 9) {
        //console.debug("**OK** : Updating neighbours for flashed octopus...");

        if (col >= 1) {
          //console.debug(`${row},${col} is updating 'A' = r c-1`);
          if (newOctoMap[row][col - 1] !== 0) newOctoMap[row][col - 1]++;
        }
        if (row < newOctoMap.length - 1) {
          //console.debug(`${row},${col} is updating 'B' = r+1 c`);
          if (newOctoMap[row + 1][col] !== 0) newOctoMap[row + 1][col]++;
        }
        if (row >= 1 && col >= 1) {
          //console.debug(`${row},${col} is updating 'C' = r-1 c-1`);
          if (newOctoMap[row - 1][col - 1] !== 0) newOctoMap[row - 1][col - 1]++;
        }
        if (row >= 1 && col < newOctoMap[0].length - 1) {
          //console.debug(`${row},${col} is updating 'D' = r-1 c+1`);
          if (newOctoMap[row - 1][col + 1] !== 0) newOctoMap[row - 1][col + 1]++;
        }
        if (row < newOctoMap.length - 1 && col >= 1) {
          //console.debug(`${row},${col} is updating 'E' = r+1 c-1`);
          if (newOctoMap[row + 1][col - 1] !== 0) newOctoMap[row + 1][col - 1]++;
        }
        if (row < newOctoMap.length - 1 && col < newOctoMap[0].length - 1) {
          //console.debug(`${row},${col} is updating 'F' = r-1 c+1`);
          if (newOctoMap[row + 1][col + 1] !== 0) newOctoMap[row + 1][col + 1]++;
        }
        if (col < newOctoMap[0].length - 1) {
          //console.debug(`${row},${col} is updating 'G' = r c+1`);
          if (newOctoMap[row][col + 1] !== 0) newOctoMap[row][col + 1]++;
        }
        if (row >= 1) {
          //console.debug(`${row},${col} is updating 'H' = r-1 c`);
          if (newOctoMap[row - 1][col] !== 0) newOctoMap[row - 1][col]++;
        }
        //console.debug(`Set to Zero = ${newOctoMap[row][col]}. Index: ${row},${col}`);
        newOctoMap[row][col] = 0;
      }
      //console.debug(`No neighbour updates required for value = ${newOctoMap[row][col]}. Index: ${row},${col}`);
    });
  });
  ////console.debug("NEIGHBOUR UPDATE: completed updates are:");
  //console.table(newOctoMap);
  return newOctoMap;
}

/** Sum up all the flashes recorded in the 'octoMap' array
 *
 * @param octoMap array of the octopus flash counts
 * @return the number of flashes record
 */
function getTotalFlashes(octoMap: number[][]): number {
  let allFlashes = 0;
  octoMap.map((rv) => {
    rv.map((cv) => {
      if (cv === 0) allFlashes = allFlashes + 1;
    });
  });
  return allFlashes;
}

/**
 * Check the octopus flash counts to see if they has all flashed at the same time - PART 2
 * @param  octoMap array of the octopus flash counts
 * @return boolean indicating if all octopuses have flashed
 */
function checkAllFlashed(octoMap: number[][]): boolean {
  // set to a known state
  let result = true;

  octoMap.map((rv) => {
    rv.map((cv) => {
      // check for a flashed octopus
      if (cv > 1) {
        result = false;
      }
    });
  });

  return result;
}
