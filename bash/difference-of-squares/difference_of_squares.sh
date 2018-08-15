#!/usr/bin/env bash

set -o errexit
set -o nounset

square_of_sum() {
    local n="$1"
    echo $(((n * n * (n + 1) * (n + 1)) / 4))
}

sum_of_squares() {
    local n="$1"
    echo $(((n * (n + 1) * (2*n + 1)) / 6))
}

difference() {
    sqsum=$(square_of_sum "$1")
    sumsq=$(sum_of_squares "$1")

    echo $((sqsum - sumsq))
}

main() {
    local cmd="$1"
    local num="$2"

    eval "$cmd $num"
}

main "$@"
