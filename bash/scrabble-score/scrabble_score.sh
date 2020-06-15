#!/bin/bash

set -o errexit
set -o nounset

declare -a SCORES=(1 3 3 2 1 4 2 4 1 8 5 1 3 1 1 3 10 1 1 1 1 4 4 8 4 10)

ord() {
    local ch
    ch=$(printf "%d" "'$1")
    echo $((ch - 97))
}

main() {
    local input=${1,,} #lowercase
    local -i total=0

    local -i i
    for ((i=0; i<${#input}; i++)); do
        local score=${SCORES["$(ord "${input:$i:1}")"]}
        ((total+=score))
    done
    echo "$total"
}

main "$@"

