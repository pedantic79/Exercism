#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
    # lowercase the input string
    local INPUT="${1,,}"

    local -A frequency=()
    local -i idx
    for ((idx = 0; idx < ${#INPUT}; idx++)); do
        local char_key=${INPUT:$idx:1}

        # nounset will complain if the key doesn't exist and exit prematurely
        # ${map[key]+x} will return x if the key does exist, and nothing
        # if it does not
        if [ ! "${frequency[$char_key]+x}" ]; then
            frequency[$char_key]=1
        fi
    done

    local ch
    for ch in {a..z}; do
        # Use the previously describe check because of nounset
        if [ ! "${frequency[$ch]+x}" ]; then
            echo false
            exit 0
        fi
    done
    echo true
    exit 0
}

main "$@"
