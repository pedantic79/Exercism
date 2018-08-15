#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
    local rna="${1:-}"

    local msg=""
    local n
    for ((n=0; n<${#rna}; n++)); do
        case "${rna:$n:1}" in
            "G") msg+="C";;
            "C") msg+="G";;
            "T") msg+="A";;
            "A") msg+="U";;
            *) echo "Invalid nucleotide detected."; exit 1;;
        esac
    done
    echo "$msg"

}

main "$@"
