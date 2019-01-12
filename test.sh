#!/bin/bash
set -Ceu

NAME=$1
EXPECTED=$2
SRC=/tmp/$NAME.ml
EXE=$SRC.exe

cd `dirname ${0}`

cat >|$SRC
bash compile.sh $SRC
$EXE --test $EXPECTED
