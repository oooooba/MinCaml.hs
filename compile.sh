#!/bin/bash
set -Ceu

RUNTIME=src/runtime
MAIN=$RUNTIME/main.o

if [ ! -f $MAIN ]; then
    echo compiling runtime
    cc -m32 -c $RUNTIME/main.c -o $MAIN
fi

stack exec mincaml-exe $@
for base in $@; do
    src=$base.s
    obj=$base.o
    exe=$base.exe
    cc -m32 -c $src -o $obj
    cc -m32 $MAIN $obj -o $exe
done
