#!/bin/bash
set -e
set -x
workspace_path=$(cd "$(dirname "$0")/.."; pwd)
echo project path is ${workspace_path}
LISP=$1
if [ -z $LISP ]; then
    LISP=sbcl
fi
docker run --name github-ci-literate-lisp --workdir /github/workspace --rm -e HOME=/github/home -e GITHUB_ACTIONS=true -e CI=true -v "${workspace_path}":"/github/workspace" ghcr.io/jingtaozf/literate-lisp/cl-base:v20211219 "env" "LISP=${LISP}" "bash" "-c" "/github/workspace/scripts/run-tests.sh"
