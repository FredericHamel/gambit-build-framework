#!/usr/bin/bash

BUILD_LIB=".builds/v4.8.9@C/_build.o1"
if [[ -f $BUILD_LIB ]];then
  echo Use old build
  gsc-script -i -e '(define-macro (macro-load-build) `(load "lib/_build"))' _build-bootstrap.scm
else
  echo Create bootstrap
  gsc-script -i -e '(define-macro (macro-load-build) `(load "_build"))' _build-bootstrap.scm
fi
test -d lib || mkdir lib
cp $BUILD_LIB lib
