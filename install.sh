#!/usr/bin/env bash

hash gsi
BUILD_DIR=$PWD/build
GAMBIT_DIR=$(gsi -e '(print (##os-path-gambitdir))')
echo $GAMBIT_DIR
DESTDIR=$GAMBIT_DIR ninja -C $BUILD_DIR install
unset BUILD_DIR
unset GAMBIT_DIR
