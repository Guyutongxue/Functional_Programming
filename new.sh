#!/bin/bash

# This script create a empty Racket source file with '#lang' snippet.
# Use this to avoid Racket Language Server's complaint while creating file.
# See https://github.com/Eugleo/magic-racket/issues/24 for detail.

if [ $# -ne 1 ]; then
    echo "Usage: $0 <path_to_new_file>"
    exit 1
fi

if [ -f $1 ]; then
    echo "File $1 already exist. Aborted."
    exit 1
fi

mkdir -p $(dirname "$1") && touch "$1" && {
    echo "#lang racket" > $1;
    code -r -g $1:2;
}