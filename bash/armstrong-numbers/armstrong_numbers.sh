#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
    local number="$1"
    local power=${#number}

    local sum=0
    local idx=0
    for ((idx = 0; idx < power; idx++)) do
        ((sum += ${number:$idx:1} ** power))
    done

    if [ "$number" -eq "$sum" ]; then
        echo "true"
        exit 0
    else
        echo "false"
        exit 1
    fi
}

main "$@"
