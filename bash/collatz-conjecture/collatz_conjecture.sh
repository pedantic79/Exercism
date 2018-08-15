#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
    n="$1"
    if [ "$n" -le 0 ]; then
        echo "Error: Only positive numbers are allowed"
        exit 1
    fi

    local count=0
    while [ "$n" -ne 1 ]; do
        if [ $((n % 2)) -eq 0 ]; then
            ((n/=2)) || true
        else
            ((n=3*n+1)) || true
        fi
        ((count++)) || true
    done

    echo "$count"
}

main "$@"
