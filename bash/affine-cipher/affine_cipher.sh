#!/usr/bin/env bash

set -o errexit
set -o nounset

ALPHA=()

ord() {
    local ch
    ch=$(printf "%d" "'$1")
    echo $((ch - 97))
}

chr() {
    local ascii
    local ch=$(($1 + 97))
    printf -v ascii "%o" "$ch"
    echo -e "\\0$ascii"
}

strindex() {
    x="${1%%$2*}"
    if [[ "$x" = "$1" ]]; then
        echo -1
    else
        echo "${#x}"
    fi
}

is_coprime () {
    local a=$1
    local b=$2

    local rem=1

    while [ "$rem" -ne 0 ]; do
        rem=$(( a % b ))
        a="$b"
        b="$rem"
    done

    [ "$a" -ne 1 ]
    return "$?"
}

init() {
    local a=$1
    local b=$2
    local i
    for ((i=0; i<26; i++)); do
        local ch=$(((a * i + b) % 26))
        ALPHA+=("$(chr "$ch")")
    done
}

encode() {
    local str=$1
    str=${str,,}
    str=${str//[^a-z0-9]/}

    local output=""
    local i
    for ((i=0; i<${#str}; i++)); do
        local x="${str:$i:1}"
        if [[ $x =~ [a-z] ]]; then
            x=$(ord "$x")
            output+="${ALPHA[$x]}"
        else
            output+="$x"
        fi
    done

    local groups=()
    for ((i = 0; i < ${#output}; i+=5)); do
        groups+=("${output:$i:5}")
    done

    echo "${groups[*]}"
}

decode() {
    local str=${1// /}
    local output=""
    local i
    local OLDIFS=$IFS
    IFS=''
    for ((i=0; i<${#str}; i++)); do
        local ch="${str:$i:1}"

        if [[ $ch =~ [a-z] ]]; then
            local pos
            pos=$(strindex "${ALPHA[*]}" "$ch")
            output+="$(chr "$pos")"
        else
            output+="$ch"
        fi
    done

    IFS="$OLDIFS"
    echo "$output"
}

check_input() {
    if is_coprime "$1" 26; then
        echo "a and m must be coprime."
        exit 1
    fi
}

main() {
    local op=$1
    shift

    check_input "$1"
    init "$@"
    shift 2
    if [ "$op" = "encode" ]; then
        encode "$@"
    else
        decode "$@"
    fi
}

main "$@"
