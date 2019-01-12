#!/bin/bash
set -Ceu

RUNTIME=src/runtime
MAIN=$RUNTIME/main.o

if [ ! -f $MAIN ]; then
    cc -c $RUNTIME/main.c -o $MAIN
fi

stack exec mincaml-exe $@
for base in $@; do
    src=$base.s
    obj=$base.o
    exe=$base.exe
    cc -c $src -o $obj
    cc $MAIN $obj -o $exe
done
