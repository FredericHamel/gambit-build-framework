#!/usr/bin/env bash

hash gsi
BUILD_DIR=$PWD/build
GAMBIT_DIR=$(gsi -e '(print (path-strip-trailing-directory-separator (path-expand "~~")))')
echo $GAMBIT_DIR
DESTDIR=$GAMBIT_DIR ninja -C $BUILD_DIR uninstall
unset BUILD_DIR
unset GAMBIT_DIR
