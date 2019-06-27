#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
    if ! [ "$#" -eq 1 ]; then
        echo "Usage: leap.sh <year>"
        exit 1
    fi

    local year="$1"
    if ! [ "$year" -eq "$year" ] 2>/dev/null; then
        echo "Usage: leap.sh <year>"
        exit 1
    fi

    if [ $((year % 4)) -eq 0 ] && { [ $((year % 100)) -ne 0 ] || [ $((year % 400)) -eq 0 ]; }; then
        echo "true"
    else
        echo "false"
    fi
}

main "$@"
