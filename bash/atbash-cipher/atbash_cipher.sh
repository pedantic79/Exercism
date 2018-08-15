#!/usr/bin/env bash

set -o errexit
set -o nounset

encode() {
    local clear=${1,,}
    clear=${clear//[., ]/}

    local output
    output=$(tr '[:lower:]' 'zyxwvutsrqponmlkjihgfedcba' <<< "$clear")

    local i
    local groups=()
    for ((i = 0; i < ${#output}; i+=5)); do
        groups+=("${output:$i:5}")
    done

    echo "${groups[*]}"
}

decode() {
    local encrypted="$1"
    encrypted=${encrypted// /}
    tr '[:lower:]' 'zyxwvutsrqponmlkjihgfedcba' <<< "$encrypted"
}

main() {
    local op="$1"
    shift

    if [ "$op" = "encode" ]; then
        encode "$@"
    else
        decode "$@"
    fi
}

main "$@"
