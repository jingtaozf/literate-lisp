#!/bin/bash
set -e # exit on any failure
if [ -d "/github/workspace/" ]; then
    ln -s /github/workspace /root/.roswell/local-projects/literate-lisp
fi
ros run -- --version
ros run -e '(pushnew :test *features*)' \
    -e '(ql:quickload :literate-lisp)' \
    -e '(literate-lisp:with-literate-syntax (load "./literate-lisp.org"))' \
    -e '(ql:quickload :literate-demo)' \
    -e '(format t "Run test in ~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))' \
    -e '(if (not (literate-lisp::run-test)) (uiop:quit 1))' \
    -e '(if (not (islands-puzzle::run-test)) (uiop:quit 1))' \
    -e '(if (not (literate-demo::run-test)) (uiop:quit 1))' \
    -e '(uiop:quit 0)'
