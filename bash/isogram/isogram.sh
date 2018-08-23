#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
    local str=${1,,}
    str=${str//[^a-z]/}

    declare -A alpha=()
    local i
    for ((i=0; i<${#str}; i++)); do
        local letter=${str:$i:1}
        ((alpha[${letter:-unset}]++)) || true
    done

    IFS=''
    local values="${alpha[*]}"
    values=${values//1/}
    if [ "${#values}" -gt 0 ]; then
        echo false
    else
        echo true
    fi
}

main "$@"
