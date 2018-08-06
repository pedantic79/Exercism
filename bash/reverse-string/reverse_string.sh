#!/bin/bash

set -o errexit
set -o nounset

main() {
    local input=$1
    local i
    for ((i=${#input} - 1; i>=0; i--)); do
        echo -n "${input:$i:1}"
    done
    echo ""
}

main "$@"

