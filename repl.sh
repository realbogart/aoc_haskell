#!/usr/bin/env bash

# $1 Year
# $2 Day

module_path="$1.$2"
ghcid --command="cabal repl --repl-options=\"-e 'import $module_path'\""

