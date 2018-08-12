#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
    local word="${1,,}"
    declare -a words
    declare -a output
    # shellcheck disable=SC2162
    read -a words <<< "$2"

    local w
    for w in "${words[@]}"; do
        if [ "${#w}" = "${#word}" ]; then
            local check="${w,,}"
            if [ "$check" != "$word" ]; then
                for (( idx=0; idx<${#word}; idx++ )); do
                    local found=0
                    for (( jdx=0; jdx<${#check}; jdx++ )); do
                        if [ "${word:idx:1}" = "${check:jdx:1}" ]; then
                            check="${check:0:jdx}${check:jdx+1}"
                            found=1
                            break
                        fi
                    done
                    if [ "$found" -eq 0 ]; then
                        break
                    fi
                done
            fi

            if [ "$check" = "" ]; then
                output+=("$w")
            fi
        fi
    done

    echo "${output[@]}"
}

main "$@"
