#!/usr/bin/bash
set -e

SCRIPT_NAME=$(realpath $0)
SCRIPT_PATH=${SCRIPT_NAME%/*}

BUILD_DIR="$SCRIPT_PATH/.builds/v4.8.9@C"
LIBNAME="_build.o1"

cd $SCRIPT_PATH
if [[ -f lib/$LIBNAME ]];then
  echo "Using lib/_build"
  gsc-script -i -e '(load "lib/_build")(define-macro (macro-load-build) `(##require-module _build))' _build-bootstrap.scm
else
  echo "Building bootstrap"
  gsc-script -i -e '(define-macro (macro-load-build) `(load "_build"))' _build-bootstrap.scm
fi
test -d lib || mkdir lib
cp $BUILD_DIR/$LIBNAME lib
cd -
