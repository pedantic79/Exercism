#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
    local str="${1//[[:space:]]/}"

    local sum=0
    local pos=0

    if [[ "$str" =~ [^[:digit:]] ]]; then
        echo false
        exit
    fi

    for ((i=${#str} - 1; i >= 0; i--)); do
        local digit="${str:$i:1}"

        if [ $((pos % 2)) -ne 0 ]; then
            local double=$((digit * 2))
            if [ "$double" -gt 9 ]; then
                ((double-=9)) || true
            fi
            ((sum+=double)) || true
        else
            ((sum+=digit)) || true
        fi

        ((pos+=1)) || true
    done

    if [ "$pos" -lt 2 ] || [ $((sum % 10)) -ne 0 ]; then
        echo false
    else
        echo true
    fi
}

main "$@"
