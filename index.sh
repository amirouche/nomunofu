#!/bin/bash

PWD=$(pwd)

INPUT_DIRECTORY=$1

for file in $(ls $INPUT_DIRECTORY); do
    echo "$INPUT_DIRECTORY/$file";
    if time ./run --optimize-level 3 --program src/nomunofu.scm import 3 "$INPUT_DIRECTORY/$file"; then
        echo "OK.";
        rm "$INPUT_DIRECTORY/$file";
    else
        echo "NOT OK!";
    fi
done;
