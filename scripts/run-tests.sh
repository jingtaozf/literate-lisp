#!/bin/bash
filepath=$(cd "$(dirname "$0")"; pwd)
echo current path is ${filepath}
TEST_LISP_FILE=${filepath}/run-tests.lisp
set -e # exit on any failure
if [ -d "/github/workspace/" ]; then
    ln -sf /github/workspace /root/quicklisp/local-projects/literate-lisp
    export HOME=/root
fi
echo target lisp is $LISP
case $LISP in
    sbcl)
    sbcl --non-interactive --load ${TEST_LISP_FILE}

    ;;
    ccl)
    ccl --load /root/ccl-init.lisp --load ${TEST_LISP_FILE}

    ;;
    abcl)
    abcl --load /root/.abclrc --load ${TEST_LISP_FILE}

    ;;
    ecl)
    ecl --load /root/.abclrc --load ${TEST_LISP_FILE}

    ;;
    *)
    sbcl --non-interactive --load /root/.sbclrc --load ${TEST_LISP_FILE}
    ;;
esac
