#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
    local -i number="$1"
    local -i power=${#number}

    local -i sum=0
    local -i idx=0
    for ((idx = 0; idx < power; idx++)) do
        ((sum += ${number:$idx:1} ** power)) || true
    done

    if [ "$number" -eq "$sum" ]; then
        echo "true"
    else
        echo "false"
    fi
}

main "$@"
