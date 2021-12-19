#!/bin/bash
filepath=$(cd "$(dirname "$0")"; pwd)
echo current path is ${filepath}

set -e # exit on any failure
if [ -d "/github/workspace/" ]; then
    ln -sf /github/workspace /root/quicklisp/local-projects/literate-lisp
    export HOME=/root
fi
echo target lisp is $LISP
case $LISP in
    sbcl)
    sbcl --non-interactive --load ${filepath}/run-tests.lisp

    ;;
    ccl)
    ccl --load /root/ccl-init.lisp --load ${filepath}/run-tests.lisp

    ;;
    abcl)
    abcl --load /root/.abclrc --load ${filepath}/run-tests.lisp

    ;;
    ecl)
    ecl --load /root/.abclrc --load ${filepath}/run-tests.lisp

    ;;
    *)
    sbcl --non-interactive --load /root/.sbclrc --load ${filepath}/run-tests.lisp
    ;;
esac
