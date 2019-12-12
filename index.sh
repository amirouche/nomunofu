#!/bin/bash

PWD=$(pwd)

INPUT_DIRECTORY=$1
OUTPUT_DIRECTORY=$2

for file in $(ls $INPUT_DIRECTORY); do
    echo "$INPUT_DIRECTORY/$file";
    if guile -L . nomunofu.scm index "$INPUT_DIRECTORY/$file"; then
        echo "OK.";
        rm "$INPUT_DIRECTORY/$file";
    else
        echo "NOT OK!";
    fi
done;
