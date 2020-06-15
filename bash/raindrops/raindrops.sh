#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
    local -i -r num="$1"
    local output

    if ! (( num % 3 )); then
        output="Pling"
    fi

    if ! (( num % 5 )); then
        output+="Plang"
    fi

    if ! (( num % 7 )); then
        output+="Plong"
    fi

    echo "${output:-$num}"
}

main "$@"
