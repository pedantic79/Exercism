#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
    local dna1="$1"
    local dna2="$2"

    if [ ${#dna1} -ne ${#dna2} ]; then
        echo "left and right strands must be of equal length"
        exit 1
    fi

    local diffs=0
    local idx
    for ((idx = 0; idx < ${#dna1}; idx++)) do
        if [ "${dna1:$idx:1}" != "${dna2:$idx:1}" ]; then
            # (( math )) operations returns the value of the expr as $?.
            # errexit doesn't like this, so suppressing with `|| true`
            (( diffs++ )) || true
        fi
    done

    echo "$diffs"
    exit 0
}

check_arg_count() {
    if [ $# -ne 2 ]; then
        echo "Usage: hamming.sh <strand1> <strand2>"
        exit 1
    fi
}

check_arg_count "$@"
main "$@"
