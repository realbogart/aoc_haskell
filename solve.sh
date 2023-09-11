#!/bin/bash

# $1 Year
# $2 Day
# Input function
# Input file

ghcid --command="cabal repl" --test "AoC.parseTestAndSolve $1.$2.parseInput $1.$2.$3 $1.$2.$3Tests \"$4\""

