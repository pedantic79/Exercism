#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
    local line="$1"
    IFS=' -_*' read -r -a words <<< "$line"

    local output=""
    local word
    for word in "${words[@]}"; do
        local letter="${word:0:1}"
        output+="${letter^^}"
    done

    echo "$output"
}

main "$@"
