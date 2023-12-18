#!/bin/bash

cabal run benchit -- +RTS -hc -p
hp2ps -c benchit.hp

