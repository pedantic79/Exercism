#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
    local dna="$1"
    declare -A nucleotide

    local i
    for ((i=0; i<${#dna}; i++)); do
        local letter=${dna:$i:1}
        if [ "$letter" != 'A' ] && \
          [ "$letter" != 'C' ] && \
          [ "$letter" != 'G' ] && \
          [ "$letter" != 'T' ]; then
            echo "Invalid nucleotide in strand"
            exit 1
        fi
        nucleotide[$letter]=$((${nucleotide[$letter]:-}+1))
    done

    for i in 'A' 'C' 'G' 'T'; do
        echo "$i: ${nucleotide[$i]:-0}"
    done
}

main "$@"
