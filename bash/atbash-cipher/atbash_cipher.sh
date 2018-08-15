#!/usr/bin/env bash

set -o errexit
set -o nounset

main() {
    local output
    output=$(codec "$2")

    if [ "$1" = "encode" ]; then
        local i
        local groups=()
        for ((i = 0; i < ${#output}; i+=5)); do
            groups+=("${output:$i:5}")
        done

        echo "${groups[*]}"
    else
        echo "$output"
    fi
}

codec() {
    local input=${1,,}
    input=${input//[^[:alnum:]]/}

    local output
    local i
    for (( i = 0; i < ${#input}; i++)) ; do
        output+=$(at_bash "${input:$i:1}")
    done

    echo "$output"
}

at_bash() {
    if [[ $1 =~ [[:digit:]] ]]; then
        echo "$1"
    else
        local ascii
        ascii=$(printf %d "'$1")
        ((ascii=219-ascii)) || true
        printf -v ascii "\\%o" "$ascii"
        printf "$ascii"
    fi
}



main "$@"
