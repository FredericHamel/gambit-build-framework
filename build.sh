#!/usr/bin/bash
set -e

SCRIPT_NAME=$(realpath $0)
SCRIPT_PATH=${SCRIPT_NAME%/*}
GAMBIT_LIB=$(gsi -e '(print (path-expand "~~lib"))')

cd $SCRIPT_PATH
[[ -d "$SCRIPT_PATH/bin" ]] || mkdir $SCRIPT_PATH/bin
gsc-script -exe  -ld-options $GAMBIT_LIB/libgambitgsc.a -l $GAMBIT_LIB/_gambitgsc.c -o $SCRIPT_PATH/bin/_build _build.scm
cd -
