#!/bin/bash

# $1 Year
# $2 Day
# Input function
# Input file

ghcid --command="cabal repl" --test "AoC.parseAndApply $1.$2.parseInput $1.$2.$3 \"$4\""

