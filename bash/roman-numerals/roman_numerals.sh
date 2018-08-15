#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
    local num="$1"
    declare -A numerals=([1000]=M [900]=CM [500]=D [400]=CD [100]=C [90]=XC
        [50]=L [40]=XL [10]=X [9]=IX [5]=V [4]=IV [1]=I)

    local roman
    while [ "$num" -gt 0 ]; do
        local divisor
        for divisor in 1000 900 500 400 100 90 50 40 10 9 5 4 1; do
            while (( num / divisor )); do
                roman+="${numerals[$divisor]}"
                (( num -= divisor )) || true
            done
        done
    done

    echo "$roman"
}

main "$@"
